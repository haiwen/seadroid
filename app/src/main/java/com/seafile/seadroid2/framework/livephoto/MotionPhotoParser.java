package com.seafile.seadroid2.framework.livephoto;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.drew.imaging.ImageMetadataReader;
import com.drew.metadata.Metadata;
import com.drew.metadata.xmp.XmpDirectory;
import com.seafile.seadroid2.annotation.NotSupport;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.annotation.Unstable;

@Todo
@Unstable
public final class MotionPhotoParser {

    /**
     * 解析结果
     */
    public static class Result {
        public boolean isMotionPhoto;       // 是否包含视频（成功定位）
        public long videoStartOffset = -1;  // 视频起点（>=0 时有效）
        public Type type = Type.PURE_IMAGE; // 类型

        public enum Type {
            STANDARD_XMP,       // XMP 标准格式
            NON_STANDARD_FTYP,  // 通过 ftyp 定位
            NO_VIDEO_FOUND,     // 存在 MotionPhoto 字段但无法定位 offset
            PURE_IMAGE          // 普通图片
        }

        @Override
        public String toString() {
            return "Result{" +
                    "isMotionPhoto=" + isMotionPhoto +
                    ", videoStartOffset=" + videoStartOffset +
                    ", type=" + type +
                    '}';
        }
    }

    // ===== 入口方法 =====
    public Result parse(File jpeg) {
        Result result = new Result();

        // 1. XMP
        Long offset = parseXmpOffset(jpeg);
        if (offset != null && offset > 0 && offset < jpeg.length()) {
            result.isMotionPhoto = true;
            result.videoStartOffset = jpeg.length() - offset;
            result.type = Result.Type.STANDARD_XMP;
            return result;
        }

        // 2. ftyp fallback
        long ftypPos = searchFtypOffset(jpeg);
        if (ftypPos > 0 && ftypPos < jpeg.length()) {
            result.isMotionPhoto = true;
            result.videoStartOffset = ftypPos;
            result.type = Result.Type.NON_STANDARD_FTYP;
            return result;
        }

        // 3. 判断是否存在 MotionPhoto 字段但 offset 缺失
        if (hasMotionPhotoTag(jpeg)) {
            result.isMotionPhoto = false;
            result.videoStartOffset = -1;
            result.type = Result.Type.NO_VIDEO_FOUND;
            return result;
        }

        // 4. 完全普通 JPEG
        result.isMotionPhoto = false;
        result.videoStartOffset = -1;
        result.type = Result.Type.PURE_IMAGE;
        return result;
    }

    // ===== XMP 解析部分 =====

    private static final String[] XMP_OFFSET_KEYS = new String[]{
            "GCamera:MicroVideoOffset",
            "Camera:MicroVideoOffset",
            "MicroVideoOffset",
            "Container:MicroVideoOffset"
    };

    private static final String[] XMP_MOTION_FLAG_KEYS = new String[]{
            "GCamera:MotionPhotoVersion", // 优先
            "GCamera:MotionPhoto",
            "Camera:MotionPhoto",
            "MotionPhoto"
    };

    private static final String[] XMP_MOTION_VIDEO_KEYS = new String[]{
            "GCamera:MotionPhotoVideo", // 有些实现把视频信息放这里
            "GCamera:MicroVideo",       // 也有厂商使用此字段作为标记
            "MicroVideo"
    };

    /**
     * 尝试从 XMP 中解析 MicroVideoOffset
     */
    /**
     * 从 XMP 中解析 Motion Photo 的视频 offset。
     * 返回 null 表示没有解析出 offset（可能不是 Motion Photo，或 Motion Photo 但无 offset 字段）。
     */
    private Long parseXmpOffset(File file) {
        try {
            Metadata metadata = ImageMetadataReader.readMetadata(file);
            XmpDirectory xmpDir = metadata.getFirstDirectoryOfType(XmpDirectory.class);
            if (xmpDir == null) return null;
            XMPMeta meta = xmpDir.getXMPMeta();
            if (meta == null) return null;

            // --- 1) 先判断这个 JPEG 是否声明为 Motion Photo ---
            boolean declaredMotionPhoto = false;
            for (String key : XMP_MOTION_FLAG_KEYS) {
                if (meta.doesPropertyExist(null, key)) {
                    // 可能是整数、布尔或字符串 "1"/"true"
                    try {
                        // 尝试读整数
                        Integer v = meta.getPropertyInteger(null, key);
                        if (v != null && v.intValue() == 1) {
                            declaredMotionPhoto = true;
                            break;
                        }
                    } catch (Exception ignored) { /* not integer */ }

                    try {
                        // 尝试读 boolean
                        Boolean b = meta.getPropertyBoolean(null, key);
                        if (b != null && b) {
                            declaredMotionPhoto = true;
                            break;
                        }
                    } catch (Exception ignored) { /* not boolean */ }

                    try {
                        // 最后尝试字符串形式
                        String s = meta.getPropertyString(null, key);
                        if (s != null) {
                            s = s.trim();
                            if ("1".equals(s) || "true".equalsIgnoreCase(s)) {
                                declaredMotionPhoto = true;
                                break;
                            }
                        }
                    } catch (Exception ignored) { /* not string */ }
                }
            }

            if (!declaredMotionPhoto) {
                // 如果没有声明为 Motion Photo，直接返回 null（不是 MotionPhoto）
                return null;
            }

            // --- 2) 确认为 Motion Photo：尝试读取 offset 字段（优先 XMP_OFFSET_KEYS） ---
            for (String key : XMP_OFFSET_KEYS) {
                try {
                    if (meta.doesPropertyExist(null, key)) {
                        // offset 常见为整数
                        Integer iv = meta.getPropertyInteger(null, key);
                        if (iv != null) {
                            long offset = iv.longValue();
                            if (offset > 0) return offset;
                        } else {
                            // 有时候是字符串的数字
                            String s = meta.getPropertyString(null, key);
                            if (s != null) {
                                s = s.trim();
                                try {
                                    long parsed = Long.parseLong(s);
                                    if (parsed > 0) return parsed;
                                } catch (NumberFormatException ignored) {
                                }
                            }
                        }
                    }
                } catch (XMPException ignore) {
                }
            }

            // --- 3) 若没有直接的 offset 字段，检查 GCamera:MotionPhotoVideo 或其他视频标记字段 ---
            for (String key : XMP_MOTION_VIDEO_KEYS) {
                if (meta.doesPropertyExist(null, key)) {
                    // 有些实现把一个结构体或字符串放在这里，尝试解析内含的 offset 字段名
                    // 先尝试直接读可能存在的数字属性（兼容性尝试）
                    try {
                        Integer iv = meta.getPropertyInteger(null, key);
                        if (iv != null && iv.longValue() > 0) return iv.longValue();
                    } catch (Exception ignored) {
                    }

                    try {
                        String s = meta.getPropertyString(null, key);
                        if (s != null) {
                            // 常见可能是 JSON-like 或 "offset=12345" 等，尝试提取数字
                            s = s.trim();
                            // 快速正则提取第一组连续数字
                            java.util.regex.Matcher m = java.util.regex.Pattern.compile("(\\d{3,})").matcher(s);
                            if (m.find()) {
                                String num = m.group(1);
                                try {
                                    long parsed = Long.parseLong(num);
                                    if (parsed > 0) return parsed;
                                } catch (NumberFormatException ignored) {
                                }
                            }
                        }
                    } catch (Exception ignored) {
                    }
                }
            }

        } catch (Exception ignore) {
            // 保持鲁棒性：任何异常视为未解析到 offset
        }
        return null;
    }

    /**
     * 判断是否存在 MotionPhoto 标记（用于区分 "NO_VIDEO_FOUND" 与 "PURE_IMAGE"）。
     * 逻辑：如果存在 MotionPhoto 标记（version 或 flag），返回 true。
     */
    private boolean hasMotionPhotoTag(File file) {
        try {
            Metadata metadata = ImageMetadataReader.readMetadata(file);
            XmpDirectory xmpDir = metadata.getFirstDirectoryOfType(XmpDirectory.class);
            if (xmpDir == null) return false;
            XMPMeta meta = xmpDir.getXMPMeta();
            if (meta == null) return false;

            for (String key : XMP_MOTION_FLAG_KEYS) {
                if (meta.doesPropertyExist(null, key)) {
                    // 只要有字段存在且值为 1/true/非空字符串，即视为存在 MotionPhoto 标记
                    try {
                        Integer v = meta.getPropertyInteger(null, key);
                        if (v != null && v.intValue() == 1) return true;
                    } catch (Exception ignored) {
                    }

                    try {
                        Boolean b = meta.getPropertyBoolean(null, key);
                        if (b != null && b) return true;
                    } catch (Exception ignored) {
                    }

                    try {
                        String s = meta.getPropertyString(null, key);
                        if (s != null) {
                            s = s.trim();
                            if (!s.isEmpty() && ("1".equals(s) || "true".equalsIgnoreCase(s) || s.matches("\\d+"))) {
                                return true;
                            }
                        }
                    } catch (Exception ignored) {
                    }

                    // 如果字段存在但不是明确的 true/1，也认为存在标记（部分厂商只写字段名）
                    return true;
                }
            }
        } catch (Exception ignore) {
        }
        return false;
    }

    // ===== ftyp fallback 搜索 =====

    /**
     * 在文件尾部扫描 ftyp box，定位 MP4 的起始位置。
     * 典型情况下视频尾部拼接，不需要扫描整个文件。
     */
    private long searchFtypOffset(File file) {
        final int SEARCH_MB = 4; // 扫描尾部 4MB
        final int SEARCH_SIZE = SEARCH_MB * 1024 * 1024;

        long fileLen = file.length();
        long scanStart = Math.max(0, fileLen - SEARCH_SIZE);

        byte[] target = new byte[]{'f', 't', 'y', 'p'};

        try (RandomAccessFile raf = new RandomAccessFile(file, "r")) {
            long total = fileLen - scanStart;
            if (total <= 0 || total > Integer.MAX_VALUE) return -1;

            byte[] buffer = new byte[(int) total];
            raf.seek(scanStart);
            raf.readFully(buffer);

            for (int i = 0; i < buffer.length - 4; i++) {
                if (buffer[i] == target[0] &&
                        buffer[i + 1] == target[1] &&
                        buffer[i + 2] == target[2] &&
                        buffer[i + 3] == target[3]) {
                    return scanStart + i;
                }
            }
        } catch (IOException ignore) {
        }
        return -1;
    }
}
