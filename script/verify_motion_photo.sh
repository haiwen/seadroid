#!/bin/bash
# 动态照片（Motion Photo / Live Photo）结构检测与内嵌视频提取
# Motion Photo / Live Photo structure inspection and embedded video extraction
#
# 支持 / Supported:
#   - Google Motion Photo / MicroVideo：JPEG 尾部附加 MP4，XMP GCamera 字段描述
#     MP4 appended to JPEG, described by XMP GCamera (Xiaomi MVIMG_*.jpg, Redmi, Pixel)
#   - Google Motion Photo v2：XMP Container:Directory 描述各 Item 长度
#     Item lengths described by XMP Container:Directory
#   - Samsung Motion Photo：JPEG 尾部附加 MP4；HEIF 使用顶层 mpvd box 内嵌视频
#     MP4 appended to JPEG; HEIF embeds video in a top-level mpvd box
#   - Apple Live Photo：HEIC + 伴生 MOV（或 MOV 封装的静态 JPEG）
#     HEIC with companion MOV (or a still JPEG wrapped in a MOV container)
#
# 用法 / Usage: ./verify_motion_photo.sh <file> [-o outdir] [-n]
# 退出码 / Exit codes: 0=motion photo, 2=not a motion photo, 1=bad arguments/file

usage() {
    cat <<'EOF'
Usage: verify_motion_photo.sh <file> [-o outdir] [-n]

Arguments:
  <file>      File to inspect (JPEG / HEIC / HEIF, or a MOV-wrapped still image)
  -o <dir>    Output directory for the extracted video (default: input directory)
  -n          Inspect only, do not extract the embedded video

Supported motion photo protocols:
  Google Motion Photo / MicroVideo (JPEG + appended MP4, XMP GCamera)
  Google Motion Photo v2 (XMP Container:Directory)
  Samsung Motion Photo (JPEG appended MP4 / HEIF mpvd box)
  Apple Live Photo (HEIC + companion MOV / MOV-wrapped JPEG)

Exit codes: 0=motion photo, 2=not a motion photo, 1=bad arguments or file
EOF
}

if [ $# -lt 1 ]; then
    usage
    exit 1
fi

FILE=""
OUTDIR=""
DO_EXTRACT=1

while [ $# -gt 0 ]; do
    case "$1" in
        -o) shift; OUTDIR="${1:-}" ;;
        -n) DO_EXTRACT=0 ;;
        -h|--help) usage; exit 0 ;;
        *)  FILE="$1" ;;
    esac
    shift
done

if [ -z "$FILE" ]; then
    usage
    exit 1
fi

if [ ! -f "$FILE" ]; then
    echo "Error: file not found: $FILE" >&2
    exit 1
fi

FILE=$(cd "$(dirname "$FILE")" && pwd)/$(basename "$FILE")
[ -z "$OUTDIR" ] && OUTDIR=$(dirname "$FILE")
if [ ! -d "$OUTDIR" ]; then
    echo "Error: output directory not found: $OUTDIR" >&2
    exit 1
fi

WORKDIR=$(mktemp -d "${TMPDIR:-/tmp}/motionphoto.XXXXXX") || exit 1
trap 'rm -rf "$WORKDIR"' EXIT

XMPTMP="$WORKDIR/xmp.xml"
: > "$XMPTMP"
XMP_SIG_HEX=$(printf '%s' 'http://ns.adobe.com/xap/1.0/' | xxd -p | tr -d '\n')

# ---------------------------------------------------------------------------
# 基础工具 / Basic helpers
# ---------------------------------------------------------------------------

file_size() { stat -f%z "$1" 2>/dev/null || stat -c%s "$1"; }

# 读取指定偏移的原始字节，输出为连续小写 hex
# Read raw bytes at the given offset and print them as contiguous lowercase hex
read_hex() {
    od -An -tx1 -j "$2" -N "$3" "$1" 2>/dev/null | tr -d ' \n'
}

hex2dec() {
    local h="${1:-}"
    [ -z "$h" ] && { echo 0; return; }
    h=$(printf '%s' "$h" | sed 's/^0*//')
    [ -z "$h" ] && h=0
    echo $((16#$h))
}

hex2ascii() {
    printf '%s' "${1:-}" | xxd -r -p 2>/dev/null
}

# 大端无符号 32 / 64 位整数 / Big-endian unsigned 32-bit and 64-bit integers
u32() { hex2dec "$(read_hex "$1" "$2" 4)"; }
u64() {
    local hi lo
    hi=$(hex2dec "$(read_hex "$1" "$2" 4)")
    lo=$(hex2dec "$(read_hex "$1" "$(( $2 + 4 ))" 4)")
    echo $(( hi * 4294967296 + lo ))
}

# 人类可读的文件大小 / Human-readable file size
human_size() {
    awk -v b="$1" 'BEGIN{
        split("B KB MB GB TB", u, " ");
        i = 1;
        while (b >= 1024 && i < 5) { b /= 1024; i++ }
        printf (i == 1 ? "%d %s" : "%.2f %s"), b, u[i]
    }'
}

# 打印 4 字节 box 类型（不可打印时回退为 hex）
# Print the 4-byte box type (fall back to hex when not printable)
box_name() {
    local s
    s=$(hex2ascii "$1")
    case "$s" in
        *[![:print:]]*) printf '%s' "$1" ;;
        *) printf '%s' "$s" ;;
    esac
}

# ---------------------------------------------------------------------------
# 文件类型识别 / File type detection
# ---------------------------------------------------------------------------

FSIZE=$(file_size "$FILE")
MAGIC=$(read_hex "$FILE" 0 16)
FILEKIND="UNKNOWN"
FTYP_BRAND=""

if [ "${MAGIC:0:4}" = "ffd8" ]; then
    FILEKIND="JPEG"
elif [ "${MAGIC:8:8}" = "66747970" ]; then
    FTYP_BRAND=$(hex2ascii "${MAGIC:16:8}")
    case "$FTYP_BRAND" in
        heic|heix|hevc|hevx|heim|heis|mif1|msf1|miaf|avif) FILEKIND="HEIF" ;;
        *) FILEKIND="MP4" ;;
    esac
fi

# ---------------------------------------------------------------------------
# JPEG：段结构解析 + APP1 XMP 提取
# JPEG: segment structure parsing and APP1 XMP extraction
# ---------------------------------------------------------------------------

JPEG_EOI=""
JPEG_SOS=""

scan_jpeg() {
    local pos=2
    local eoi="" sos=""
    echo "  Offset      Marker    Length      Description"
    while [ "$pos" -lt "$FSIZE" ]; do
        local h m1 m2
        h=$(read_hex "$FILE" "$pos" 4)
        [ ${#h} -lt 4 ] && break
        m1=${h:0:2}
        m2=${h:2:2}
        if [ "$m1" != "ff" ]; then
            echo "  [offset $pos] not a JPEG marker byte (0x${m1}), stop parsing"
            break
        fi
        # 填充字节 FF FF / Fill byte FF FF
        if [ "$m2" = "ff" ]; then
            pos=$((pos + 1))
            continue
        fi
        case "$m2" in
            d8)
                printf "  %-11s FFD8      %-10s Start of image (SOI)\n" "$pos" "-"
                pos=$((pos + 2))
                ;;
            d9)
                printf "  %-11s FFD9      %-10s End of image (EOI)\n" "$pos" "-"
                eoi=$pos
                break
                ;;
            da)
                local slen
                slen=$(hex2dec "${h:4:4}")
                printf "  %-11s FFDA      %-10s Start of scan (SOS), entropy-coded data follows\n" "$pos" "$slen"
                sos=$pos
                break
                ;;
            01|d0|d1|d2|d3|d4|d5|d6|d7)
                printf "  %-11s FF%-6s  %-10s Standalone marker (no length field)\n" "$pos" "$m2" "-"
                pos=$((pos + 2))
                ;;
            *)
                local slen seglen name
                slen=$(hex2dec "${h:4:4}")
                if [ "$slen" -lt 2 ]; then
                    echo "  [offset $pos] invalid segment length (${slen}), stop parsing"
                    break
                fi
                seglen=$((slen - 2))
                case "$m2" in
                    e0)  name="APP0/JFIF" ;;
                    e1)  name="APP1" ;;
                    e2)  name="APP2" ;;
                    e3)  name="APP3" ;;
                    e4)  name="APP4" ;;
                    e5)  name="APP5" ;;
                    e6)  name="APP6" ;;
                    e7)  name="APP7" ;;
                    e8)  name="APP8" ;;
                    e9)  name="APP9" ;;
                    ea)  name="APP10" ;;
                    eb)  name="APP11" ;;
                    ec)  name="APP12" ;;
                    ed)  name="APP13" ;;
                    ee)  name="APP14" ;;
                    ef)  name="APP15" ;;
                    fe)  name="COM" ;;
                    db)  name="DQT" ;;
                    c4)  name="DHT" ;;
                    dd)  name="DRI" ;;
                    c0|c1|c2|c3|c5|c6|c7|c9|ca|cb|cd|ce|cf) name="SOF(0xFF${m2})" ;;
                    *)   name="0xFF${m2}" ;;
                esac

                if [ "$m2" = "e1" ]; then
                    local sighex
                    sighex=$(read_hex "$FILE" "$((pos + 4))" 29)
                    if [ "${sighex:0:12}" = "457869660000" ]; then
                        name="APP1/Exif"
                    elif [ "${sighex:0:${#XMP_SIG_HEX}}" = "$XMP_SIG_HEX" ]; then
                        name="APP1/XMP"
                        # 导出 XMP 包（跳过 29 字节命名空间标识）
                        # Dump the XMP packet (skip the 29-byte namespace identifier)
                        tail -c +$((pos + 4 + 29 + 1)) "$FILE" 2>/dev/null \
                            | head -c $((seglen - 29)) >> "$XMPTMP"
                        printf '\n' >> "$XMPTMP"
                    fi
                fi
                printf "  %-11s FF%-6s  %-10s %s (payload %s bytes)\n" \
                    "$pos" "$m2" "$slen" "$name" "$seglen"
                pos=$((pos + 2 + slen))
                ;;
        esac
    done
    JPEG_SOS="$sos"
    # SOS 之后为熵编码数据，其中 FF 字节均被填充为 FF00，因此第一个 FFD9 即 EOI
    # After SOS comes entropy-coded data where FF bytes are stuffed as FF00,
    # so the first FFD9 after SOS is the real EOI
    if [ -n "$sos" ] && [ -z "$eoi" ]; then
        local p
        for p in $(LC_ALL=C grep -abo $'\xff\xd9' "$FILE" 2>/dev/null | LC_ALL=C cut -d: -f1); do
            if [ "$p" -gt "$sos" ]; then
                eoi=$p
                break
            fi
        done
        if [ -n "$eoi" ]; then
            printf "  %-11s FFD9      %-10s End of image (EOI, located inside entropy-coded data)\n" "$eoi" "-"
        fi
    fi
    JPEG_EOI="$eoi"
}

# ---------------------------------------------------------------------------
# ISOBMFF（HEIF / MP4）顶层 box 解析
# ISOBMFF (HEIF / MP4) top-level box parsing
# ---------------------------------------------------------------------------

MPVD_OFFSET=""
MPVD_PAYLOAD_OFF=""
MPVD_PAYLOAD_LEN=""
MOOV_OFF=""
MOOV_SIZE=""

scan_boxes() {
    local file=$1 start=$2 end=$3 indent=$4
    local off=$start
    local pad
    pad=$(printf "%${indent}s" "")
    while [ "$off" -lt "$end" ]; do
        local hdr size32 type hdrlen total
        hdr=$(read_hex "$file" "$off" 8)
        [ ${#hdr} -lt 16 ] && break
        size32=$(hex2dec "${hdr:0:8}")
        type=$(box_name "${hdr:8:8}")
        hdrlen=8
        if [ "$size32" -eq 1 ]; then
            total=$(u64 "$file" "$((off + 8))")
            hdrlen=16
        elif [ "$size32" -eq 0 ]; then
            total=$((end - off))
        else
            total=$size32
        fi
        if [ "$total" -lt "$hdrlen" ] || [ $((off + total)) -gt "$end" ]; then
            printf "%s%-8s offset=%-10s size=%-10s (invalid size, stop)\n" "$pad" "$type" "$off" "$total"
            break
        fi
        printf "%s%-8s offset=%-10s size=%-10s" "$pad" "$type" "$off" "$total"
        if [ "$type" = "mpvd" ]; then
            printf "  <== Motion Photo video data"
            if [ -z "$MPVD_OFFSET" ]; then
                MPVD_OFFSET=$off
                MPVD_PAYLOAD_OFF=$((off + hdrlen))
                MPVD_PAYLOAD_LEN=$((total - hdrlen))
            fi
        fi
        if [ "$type" = "moov" ] && [ "$indent" -eq 0 ] && [ -z "$MOOV_OFF" ]; then
            MOOV_OFF=$off
            MOOV_SIZE=$total
        fi
        printf "\n"

        # meta 是 FullBox，子 box 从 +12 开始；只展开一层用于观察结构
        # meta is a FullBox, its children start at +12; expand one level only
        if [ "$type" = "meta" ] && [ "$indent" -eq 0 ]; then
            scan_boxes "$file" "$((off + 12))" "$((off + total))" 2
        fi
        off=$((off + total))
    done
}

# ---------------------------------------------------------------------------
# MP4 定位与校验 / MP4 locating and validation
# ---------------------------------------------------------------------------

MP4_OFF=""
MP4_BRAND=""

# 校验 box 偏移处是否为合法 ftyp box，成功时设置 MP4_OFF / MP4_BRAND
# Validate that a legal ftyp box starts at the given offset; sets MP4_OFF / MP4_BRAND
mp4_validate() {
    local box=$1
    [ "$box" -lt 4 ] && return 1
    [ "$box" -ge "$FSIZE" ] && return 1
    local sz32
    sz32=$(u32 "$FILE" "$box")
    local total
    if [ "$sz32" -eq 1 ]; then
        total=$(u64 "$FILE" "$((box + 8))")
    elif [ "$sz32" -eq 0 ]; then
        total=$((FSIZE - box))
    else
        total=$sz32
    fi
    [ "$total" -lt 16 ] && return 1
    [ "$total" -gt $((FSIZE - box + 16)) ] && return 1
    local brand
    brand=$(hex2ascii "$(read_hex "$FILE" "$((box + 8))" 4)")
    [ -z "$brand" ] && return 1
    case "$(printf '%s' "$brand" | LC_ALL=C tr -d '[:alnum:] ._-')" in
        "") ;;
        *) return 1 ;;
    esac
    # 校验 ftyp 之后紧跟的顶层 box，排除图像数据中的偶然命中
    # Check the top-level box right after ftyp to reject accidental matches in image data
    [ $((box + total + 8)) -gt "$FSIZE" ] && return 1
    local nh next_type
    nh=$(read_hex "$FILE" "$((box + total))" 8)
    [ ${#nh} -lt 16 ] && return 1
    next_type=$(box_name "${nh:8:8}")
    case "$next_type" in
        moov|mdat|free|wide|uuid|styp|moof) ;;
        *) return 1 ;;
    esac
    MP4_OFF=$box
    MP4_BRAND=$brand
    return 0
}

# 在 [from, from+window) 范围内寻找 ftyp box 起点
# Find the start of an ftyp box within [from, from+window)
mp4_probe_near() {
    local from=$1 window=${2:-4096}
    [ "$from" -lt 0 ] && from=0
    local h pre rel
    h=$(read_hex "$FILE" "$from" "$window")
    case "$h" in
        *66747970*) ;;
        *) return 1 ;;
    esac
    pre=${h%%66747970*}
    rel=$(( ${#pre} / 2 ))
    [ $((rel % 2)) -ne 0 ] && rel=$((rel + 1))
    mp4_validate "$((from + rel - 4))"
}

# 在整个文件中按 ftyp 签名扫描，选择 >= from 的第一个合法候选
# Scan the whole file for the ftyp signature and pick the first valid candidate >= from
mp4_find_after() {
    local from=${1:-0}
    local p
    for p in $(LC_ALL=C grep -abo "ftyp" "$FILE" 2>/dev/null | LC_ALL=C cut -d: -f1); do
        local box=$((p - 4))
        [ "$box" -lt "$from" ] && continue
        if mp4_validate "$box"; then
            return 0
        fi
    done
    return 1
}

# ---------------------------------------------------------------------------
# XMP 字段提取 / XMP field extraction
# ---------------------------------------------------------------------------

xmp_num() {
    {
        grep -oE "GCamera:$1=\"[0-9]+\"" "$XMPTMP" 2>/dev/null \
            | head -1 | grep -oE '[0-9]+'
        grep -oE "<GCamera:$1>[0-9]+" "$XMPTMP" 2>/dev/null \
            | head -1 | grep -oE '[0-9]+'
    } | head -1
}

xmp_flag() {
    grep -qE "GCamera:$1=\"1\"|<GCamera:$1>1<" "$XMPTMP" 2>/dev/null
}

# MotionPhoto Item 的 Length / Padding（Container:Directory，兼容元素/属性两种写法）
# Length / Padding of the MotionPhoto item in Container:Directory
# (supports both element and attribute syntax)
container_motion_fields() {
    awk '
        {
            line = $0
            if (line ~ /Item:Semantic[^A-Za-z]*(MotionPhoto|<)/) inm = 1
            if (inm) {
                if (line ~ /Item:Length/) {
                    t = line; gsub(/[^0-9]/, "", t); l = t
                }
                if (line ~ /Item:Padding/) {
                    t = line; gsub(/[^0-9]/, "", t); p = t
                    print (l == "" ? 0 : l), (p == "" ? 0 : p)
                    exit
                }
            }
        }
    ' "$XMPTMP" | head -1
}

# ---------------------------------------------------------------------------
# 主流程 / Main flow
# ---------------------------------------------------------------------------

echo "========================================="
echo "Motion photo inspection: $(basename "$FILE")"
echo "========================================="
echo ""

# 1. 文件信息 / File information
echo "[1] File Information"
echo "  Path      : $FILE"
echo "  Size      : $FSIZE bytes ($(human_size "$FSIZE"))"
echo "  Magic     : $MAGIC"
case "$FILEKIND" in
    JPEG) echo "  Type      : JPEG still image" ;;
    HEIF) echo "  Type      : HEIC/HEIF (ftyp brand: ${FTYP_BRAND})" ;;
    MP4)  echo "  Type      : MP4/MOV container (ftyp brand: ${FTYP_BRAND})" ;;
    *)    echo "  Type      : Unknown (not JPEG / HEIF / MP4)" ;;
esac
echo ""

# 2. 按类型解析结构 / Parse structure by file type
STRUCT="$WORKDIR/struct.txt"
: > "$STRUCT"

PROTO=""
VIDEO_OFF=""
VIDEO_LEN=""
VIDEO_DESC=""
CONTENT_ID=""
COMPANION=""
STILL_HINT=0

MICRO_OFFSET=""
MICRO_LENGTH=""
CONTAINER_LEN=""
CONTAINER_PAD=""

case "$FILEKIND" in
    JPEG)
        scan_jpeg > "$STRUCT"

        # XMP：exiftool 缺失时也能用自解析结果
        # XMP: the self-parsed result works even without exiftool
        MICRO_OFFSET=$(xmp_num MicroVideoOffset)
        MICRO_LENGTH=$(xmp_num MicroVideoLength)
        CONTAINER_FIELDS=$(container_motion_fields)
        CONTAINER_LEN=$(echo "$CONTAINER_FIELDS" | awk '{print $1}')
        CONTAINER_PAD=$(echo "$CONTAINER_FIELDS" | awk '{print $2}')

        if xmp_flag MicroVideo; then
            PROTO="Google Motion Photo / MicroVideo (XMP GCamera:MicroVideo=1, MP4 appended to JPEG)"
        elif xmp_flag MotionPhoto; then
            PROTO="Google/Samsung Motion Photo (XMP GCamera:MotionPhoto=1)"
        fi

        # Samsung 专有标记 / Samsung proprietary marker
        if LC_ALL=C grep -abo "MotionPhoto_Data" "$FILE" >/dev/null 2>&1; then
            [ -z "$PROTO" ] && PROTO="Samsung Motion Photo (MotionPhoto_Data trailer)"
            echo "  OK: Samsung MotionPhoto_Data marker found" >> "$STRUCT"
        fi

        # 视频定位：优先 MicroVideoOffset（相对文件尾部）
        # Video location: prefer MicroVideoOffset (measured from end of file)
        if [ -n "$MICRO_OFFSET" ] && [ "$MICRO_OFFSET" -gt 0 ]; then
            if mp4_probe_near "$((FSIZE - MICRO_OFFSET))" 4096; then
                VIDEO_OFF=$MP4_OFF
                VIDEO_LEN=$((FSIZE - MP4_OFF))
                VIDEO_DESC="MicroVideoOffset=${MICRO_OFFSET} (from end of file) -> video start $((FSIZE - MICRO_OFFSET))"
            fi
        fi
        # Container:Directory 描述 / Container:Directory description
        if [ -z "$VIDEO_OFF" ] && [ -n "$CONTAINER_LEN" ] && [ "$CONTAINER_LEN" -gt 0 ]; then
            local_cand=$((FSIZE - CONTAINER_LEN))
            if mp4_probe_near "$local_cand" 4096; then
                VIDEO_OFF=$MP4_OFF
                VIDEO_LEN=$((FSIZE - MP4_OFF))
                VIDEO_DESC="Container:Directory MotionPhoto Item:Length=$CONTAINER_LEN Padding=$CONTAINER_PAD"
            fi
        fi
        # 兜底：全文件搜索第一个合法 ftyp（要求位于 JPEG 主体之后）
        # Fallback: scan for the first valid ftyp after the JPEG body
        if [ -z "$VIDEO_OFF" ]; then
            if [ -n "$JPEG_EOI" ]; then
                MP4_FROM=$((JPEG_EOI + 2))
            else
                MP4_FROM=1024
            fi
            if mp4_find_after "$MP4_FROM"; then
                VIDEO_OFF=$MP4_OFF
                VIDEO_LEN=$((FSIZE - MP4_OFF))
                VIDEO_DESC="ftyp signature scan at file tail (no usable XMP offset)"
            fi
        fi
        ;;

    HEIF)
        scan_boxes "$FILE" 0 "$FSIZE" 0 > "$STRUCT"

        # XMP 在 meta box 内，借助 exiftool 提取
        # XMP lives inside the meta box, so use exiftool to extract it
        if command -v exiftool >/dev/null 2>&1; then
            exiftool -xmp -b "$FILE" > "$XMPTMP" 2>/dev/null
            MICRO_OFFSET=$(xmp_num MicroVideoOffset)
            MICRO_LENGTH=$(xmp_num MicroVideoLength)
            CONTAINER_FIELDS=$(container_motion_fields)
            CONTAINER_LEN=$(echo "$CONTAINER_FIELDS" | awk '{print $1}')
            CONTAINER_PAD=$(echo "$CONTAINER_FIELDS" | awk '{print $2}')
            CONTENT_ID=$(exiftool -s3 -ContentIdentifier "$FILE" 2>/dev/null | head -1)
        fi
        if [ -z "$CONTENT_ID" ] \
                && LC_ALL=C grep -aqE "com\.apple\.quicktime\.content\.identifier|ContentIdentifier" "$FILE" 2>/dev/null; then
            CONTENT_ID="(ContentIdentifier present in metadata)"
        fi

        if [ -n "$MPVD_OFFSET" ]; then
            PROTO="HEIF Motion Photo (top-level mpvd box with embedded video)"
            if mp4_probe_near "$MPVD_PAYLOAD_OFF" 65536; then
                VIDEO_OFF=$MP4_OFF
                VIDEO_LEN=$((MPVD_PAYLOAD_OFF + MPVD_PAYLOAD_LEN - MP4_OFF))
                VIDEO_DESC="mpvd box @${MPVD_OFFSET} (payload $MPVD_PAYLOAD_LEN bytes, $((MP4_OFF - MPVD_PAYLOAD_OFF)) byte vendor header before video)"
            fi
        elif [ -n "$CONTENT_ID" ]; then
            PROTO="Apple Live Photo (HEIC still image, video stored in companion MOV)"
        elif xmp_flag MicroVideo || xmp_flag MotionPhoto; then
            PROTO="Google Motion Photo (HEIF, described by XMP GCamera)"
        fi

        # 无内嵌视频时，尝试查找伴生 MOV
        # When there is no embedded video, look for a companion MOV
        if [ -z "$VIDEO_OFF" ]; then
            localbase="${FILE%.*}"
            for ext in MOV mov MP4 mp4; do
                if [ -f "$localbase.$ext" ]; then
                    COMPANION="$localbase.$ext"
                    break
                fi
            done
        fi
        ;;

    MP4)
        scan_boxes "$FILE" 0 "$FSIZE" 0 > "$STRUCT"
        if command -v exiftool >/dev/null 2>&1; then
            CONTENT_ID=$(exiftool -s3 -ContentIdentifier "$FILE" 2>/dev/null | head -1)
        fi
        # Apple Live Photo 的 .JPG 实为 MOV 容器，moov/stsd 中含 'jpeg' 编码且 mdat 内为静态图
        # An Apple Live Photo .JPG is actually a MOV container: moov/stsd contains
        # a 'jpeg' codec entry and mdat holds the still image
        if [ -n "$MOOV_OFF" ] && [ "$FTYP_BRAND" = "qt  " ]; then
            if tail -c +$((MOOV_OFF + 1)) "$FILE" 2>/dev/null \
                    | head -c "$MOOV_SIZE" | LC_ALL=C grep -aq "jpeg"; then
                STILL_HINT=1
                PROTO="Apple Live Photo (MOV-wrapped still image, JPEG inside mdat)"
            fi
        fi
        ;;
esac

# 3. 元数据输出 / Metadata output
echo "[2] XMP Motion Photo Metadata"
if [ -s "$XMPTMP" ] && grep -qE "GCamera|Container|Item:" "$XMPTMP" 2>/dev/null; then
    echo "  --- Key fields ---"
    [ -n "$MICRO_OFFSET" ] && echo "  GCamera:MicroVideoOffset  = $MICRO_OFFSET"
    [ -n "$MICRO_LENGTH" ] && echo "  GCamera:MicroVideoLength  = $MICRO_LENGTH"
    for tag in MotionPhoto MicroVideo MicroVideoVersion MotionPhotoVersion \
               MotionPhotoPresentationTimestampUs MicroVideoPresentationTimestampUs; do
        val=$(xmp_num "$tag")
        [ -n "$val" ] && echo "  GCamera:$tag = $val"
    done
    if [ -n "$CONTAINER_LEN" ]; then
        echo "  Container Directory       : MotionPhoto Item:Length=$CONTAINER_LEN Padding=$CONTAINER_PAD"
    fi
    [ -n "$CONTENT_ID" ] && echo "  ContentIdentifier         = $CONTENT_ID"
    echo ""
    echo "  --- Raw XMP (first 60 lines) ---"
    grep -vE '^\s*$' "$XMPTMP" | head -60 | sed 's/^/  /'
elif command -v exiftool >/dev/null 2>&1 \
        && exiftool -a -G1 -s "$FILE" 2>/dev/null | grep -qiE "GCamera|MotionPhoto"; then
    echo "  --- Key fields (exiftool) ---"
    exiftool -a -G1 -s "$FILE" 2>/dev/null \
        | grep -iE "MotionPhoto|GCamera|MicroVideo|Container|ContentIdentifier" \
        | sed 's/^/  /'
else
    echo "  No XMP / Motion Photo metadata found"
    if ! command -v exiftool >/dev/null 2>&1; then
        echo "  (hint: exiftool is not installed, HEIC metadata parsing is limited)"
    fi
fi
echo ""

# 4. 结构输出 / Structure output
echo "[3] File Structure"
if [ "$FILEKIND" = "JPEG" ]; then
    echo "  JPEG segment structure:"
elif [ "$FILEKIND" = "HEIF" ] || [ "$FILEKIND" = "MP4" ]; then
    echo "  ISOBMFF box structure (top level / meta children):"
else
    echo "  Unsupported file type (only JPEG / HEIC / HEIF / MP4/MOV)"
fi
if [ -s "$STRUCT" ]; then
    sed 's/^/  /' "$STRUCT"
else
    echo "  (no structure data)"
fi
echo ""

# 5. 视频定位 / Video location
echo "[4] Embedded Video Location"
if [ -n "$VIDEO_OFF" ]; then
    echo "  OK: embedded video found"
    echo "  Start offset : $VIDEO_OFF"
    echo "  Length       : $VIDEO_LEN bytes ($(human_size "$VIDEO_LEN"))"
    echo "  End offset   : $((VIDEO_OFF + VIDEO_LEN)) / file size $FSIZE"
    [ -n "$VIDEO_DESC" ] && echo "  Located by   : $VIDEO_DESC"
    echo "  MP4 brand    : ${MP4_BRAND:-unknown}"
    echo "  First 24 bytes: $(read_hex "$FILE" "$VIDEO_OFF" 24)"
else
    echo "  ERROR: no embedded video data found"
    if [ "$STILL_HINT" -eq 1 ]; then
        echo "  Still image  : JPEG inside mdat (Apple Live Photo container)"
    fi
    if [ -n "$COMPANION" ]; then
        echo "  Companion    : ${COMPANION} (separate file, not embedded)"
    elif [ -n "$CONTENT_ID" ]; then
        echo "  ContentIdentifier: ${CONTENT_ID} (companion .MOV required)"
    fi
fi
echo ""

# 6. 结论 / Result
echo "[5] Result"
if [ -n "$PROTO" ] || [ -n "$VIDEO_OFF" ] || [ -n "$COMPANION" ]; then
    echo "  OK: this is a Motion Photo / Live Photo"
    [ -n "$PROTO" ] && echo "  Protocol  : $PROTO"
    if [ -n "$VIDEO_OFF" ]; then
        echo "  Form      : still image + embedded video (same file)"
    elif [ -n "$COMPANION" ]; then
        echo "  Form      : still image + companion video (separate files)"
    fi
else
    echo "  NO: not a Motion Photo / Live Photo (no Motion Photo / Live Photo structure found)"
fi
echo ""

# 7. 提取 / Extraction
echo "[6] Extraction"
if [ -z "$VIDEO_OFF" ]; then
    echo "  Nothing to extract (no embedded video)"
elif [ "$DO_EXTRACT" -eq 0 ]; then
    echo "  Extraction skipped (-n)"
else
    ext="mp4"
    [ "$MP4_BRAND" = "qt  " ] && ext="mov"
    out="$OUTDIR/$(basename "${FILE%.*}")_motion.$ext"
    if tail -c +$((VIDEO_OFF + 1)) "$FILE" 2>/dev/null | head -c "$VIDEO_LEN" > "$out"; then
        echo "  Output file  : $out"
        echo "  Written size : $(file_size "$out") bytes ($(human_size "$(file_size "$out")"))"
        obrand=$(hex2ascii "$(read_hex "$out" 8 4)")
        echo "  First box    : $(hex2ascii "$(read_hex "$out" 4 4)") / brand=${obrand:-unknown}"
        if command -v exiftool >/dev/null 2>&1; then
            info=$(exiftool -s3 -FileType -Duration -ImageWidth -ImageHeight -VideoFrameRate -AvgBitrate "$out" 2>/dev/null | tr '\n' ' ')
            [ -n "$info" ] && echo "  Video info   : $info"
            warn=$(exiftool -s3 -Warning "$out" 2>/dev/null | head -1)
            [ -n "$warn" ] && echo "  Parse warning: $warn"
        fi
    else
        echo "  ERROR: extraction failed"
    fi
fi
echo ""
echo "========================================="
echo "Detection finished"
echo "========================================="

if [ -n "$PROTO" ] || [ -n "$VIDEO_OFF" ] || [ -n "$COMPANION" ]; then
    exit 0
fi
exit 2
