package com.seafile.seadroid2.framework.motion_photo;

import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPIterator;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.properties.XMPPropertyInfo;
import com.drew.imaging.ImageMetadataReader;
import com.drew.metadata.Metadata;
import com.drew.metadata.xmp.XmpDirectory;
import com.google.common.primitives.Longs;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.framework.util.SLogs;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

@Todo
@Unstable
public final class MotionPhotoParser {
    private static final String TAG = "MotionPhotoParser";
    private final int JPEG_SOI = 0xFFD8;
    private final int JPEG_APP1 = 0xFFE1;
    private final int JPEG_SOS = 0xFFD;

    private final String XIAOMI_MICRO_VIDEO = "MicroVideo";
    private final String XIAOMI_MICRO_VIDEO_OFFSET = "MicroVideoOffset";
    private final String GCAMERA_MICRO_VIDEO = "GCamera:MicroVideo";
    private final String GCAMERA_MICRO_VIDEO_OFFSET = "GCamera:MicroVideoOffset";
    private final String GOOGLE_MOTION_PHOTO = "GCamera:MotionPhoto";
    private final String OPPO_LIVE_PHOTO = "OpCamera:OLivePhotoVersion";

    static final String NS_ADOBE_GAIN_MAP = "http://ns.adobe.com/hdr-gain-map/1.0/";
    static final String NS_G_CAMERA = "http://ns.google.com/photos/1.0/camera/";
    static final String NS_G_CONTAINER = "http://ns.google.com/photos/1.0/container/";
    static final String NS_G_CONTAINER_ITEM = "http://ns.google.com/photos/1.0/container/item/";
    static final String NS_OPLUS_CAMERA = "http://ns.oplus.com/photos/1.0/camera/";

    public static final class Result {
        public final boolean isMotionPhoto;
        /**
         * The video frame timestamp corresponding to the still image thumbnail is measured in microseconds,
         * and if it is -1, it means that it is not set and not specified.
         * <br/>
         * 与静态图片缩对应的视频帧时间戳，以微秒为单位，为 -1 时，表示未设置/未指定。
         */
        public final long presentationTimestampUs;
        public final long videoStartOffset;
        public final long videoLength;

        public Result(boolean isMotionPhoto, long pts, long start, long length) {
            this.isMotionPhoto = isMotionPhoto;
            this.presentationTimestampUs = pts;
            this.videoStartOffset = start;
            this.videoLength = length;
        }

        @Override
        public String toString() {
            return "Result{" +
                    "isMotionPhoto=" + isMotionPhoto +
                    ", presentationTimestampUs=" + presentationTimestampUs +
                    ", videoStartOffset=" + videoStartOffset +
                    ", videoLength=" + videoLength +
                    '}';
        }
    }

    public enum MotionPhotoType {
        MICRO_VIDEO,
        MOTION_PHOTO,
        FTYP,
        JPEG
    }
    // ============================
    // Public API
    // ============================

    public static Result parse(File jpegFile) throws IOException {
        try (RandomAccessFile raf = new RandomAccessFile(jpegFile, "r")) {
            byte[] bytes = new byte[(int) raf.length()];
            raf.readFully(bytes);
            return parse(bytes);
        }
    }

    public static Result parse(byte[] jpegBytes) {
        XMPMeta xmp = extractXmp(jpegBytes);
        MotionPhotoType motionPhotoType = checkMotionPhoto(xmp, jpegBytes);

        if (motionPhotoType == MotionPhotoType.JPEG) {
            SLogs.d(TAG, "MotionPhotoType=JPEG");
            return new Result(false, 0L, -1, -1);
        }

        Long pts = readPtsSafe(xmp);
        long ptsValue = pts == null ? 0L : pts;
        SLogs.d(TAG, "MotionPhotoType=" + motionPhotoType + " PTS=" + ptsValue);

        if (motionPhotoType == MotionPhotoType.FTYP) {
            FtypScanResult ftypResult = fallbackLocateVideoWithLength(jpegBytes);
            return new Result(true, ptsValue, ftypResult.videoStartOffset, ftypResult.videoLength);
        }

        Long containerVideoLength = parseContainerVideoLength(xmp);

        long videoStart, videoLength;
        if (containerVideoLength == null) {
            FtypScanResult ftypResult = fallbackLocateVideoWithLength(jpegBytes);
            videoStart = ftypResult.videoStartOffset;
            videoLength = ftypResult.videoLength;
        } else {
            videoLength = containerVideoLength;
            videoStart = jpegBytes.length - videoLength;
        }

        return new Result(true, ptsValue, videoStart, videoLength);
    }

    // ============================
    // extract xmp
    // ============================
    private static XMPMeta extractXmp(byte[] jpegBytes) {
        try {
            Metadata metadata = ImageMetadataReader.readMetadata(new ByteArrayInputStream(jpegBytes));
            XmpDirectory dir = metadata.getFirstDirectoryOfType(XmpDirectory.class);
            if (dir != null) {
                return dir.getXMPMeta();
            }
        } catch (Exception ignore) {
        }
        return null;
    }

    private static MotionPhotoType checkMotionPhoto(XMPMeta xmp, byte[] jpegBytes) {

        try {
            if (xmp == null) {
                return checkMotionPhotoByFtyp(jpegBytes);
            }

            // 2. Xiaomi legacy MicroVideo
            String microVideoV = xmp.getPropertyString(NS_G_CAMERA, "MicroVideo");
            if (TextUtils.equals("1", microVideoV)) {
                SLogs.d(TAG, "Detected MotionPhotoType=MICRO_VIDEO");
                return MotionPhotoType.MICRO_VIDEO;
            }

            String motionPhotoV = xmp.getPropertyString(NS_G_CAMERA, "MotionPhoto");
            if (TextUtils.equals("1", motionPhotoV)) {
                SLogs.d(TAG, "Detected MotionPhotoType=MOTION_PHOTO");
                return MotionPhotoType.MOTION_PHOTO;
            }

            MotionPhotoType type = checkMotionPhotoByFtyp(jpegBytes);
            SLogs.d(TAG, "Detected MotionPhotoType by FTYP fallback: " + type);
            return type;
        } catch (Exception ignore) {
            return MotionPhotoType.JPEG;
        }
    }

    private static MotionPhotoType checkMotionPhotoByFtyp(byte[] jpegBytes) {
        if (jpegBytes == null) {
            return MotionPhotoType.JPEG;
        }

        if (jpegBytes.length == 0) {
            return MotionPhotoType.JPEG;
        }

        byte[] target = new byte[]{'f', 't', 'y', 'p'};
        for (int i = jpegBytes.length - 4; i >= 0; i--) {
            if (jpegBytes[i] == target[0] && jpegBytes[i + 1] == target[1] && jpegBytes[i + 2] == target[2] && jpegBytes[i + 3] == target[3]) {
                return MotionPhotoType.FTYP;
            }
        }
        return MotionPhotoType.JPEG;
    }

    @Nullable
    private static Long parseContainerVideoLength(XMPMeta xmp) {
        Long containerVideoLength = parseContainerVideoLengthWithMicroVideo(xmp);
        if (containerVideoLength == null) {
            containerVideoLength = parseContainerVideoLengthWithMotionPhoto(xmp);
        }

        return containerVideoLength;
    }

    /**
     * V1 data structures. xiaomi used it.
     *
     */
    @Nullable
    private static Long parseContainerVideoLengthWithMicroVideo(XMPMeta xmp) {
        try {
            String offsetStr = xmp.getPropertyString(NS_G_CAMERA, "MicroVideoOffset");
            if (TextUtils.isEmpty(offsetStr)) {
                return null;
            }
            return Longs.tryParse(offsetStr);
        } catch (XMPException e) {
            SLogs.e(e);
            return null;
        }
    }

    /**
     * V2 data structures. Android standard data structures.
     */
    @Nullable
    private static Long parseContainerVideoLengthWithMotionPhoto(XMPMeta xmp) {
        if (xmp == null) return null;

        // 临时记录：上一个路径的父节点（用于匹配 Length）
        String currentItemPath = null;
        boolean isVideoItem = false;

        long accumulatedOffset = 0;

        try {
            XMPIterator it = xmp.iterator();

            String currentSemantic = null;
            String currentMime = null;
            Long currentLength = null;
            Long currentPadding = null;

            while (it.hasNext()) {
                XMPPropertyInfo prop = (XMPPropertyInfo) it.next();

                String ns = prop.getNamespace();
                String path = prop.getPath();
                String value = prop.getValue();

                // 只处理 Item 命名空间
                if (!TextUtils.equals(NS_G_CONTAINER_ITEM, ns)) {
                    continue;
                }

                if (path.endsWith("Item:Semantic")) {
                    currentSemantic = value;
                }

                if (path.endsWith("Item:Mime")) {
                    currentMime = value;
                    if ("video/mp4".equals(value)) {
                        // 记录当前 Item 节点路径（不含属性名）
                        currentItemPath = path.replace("/Item:Mime", "");
                        isVideoItem = true;
                    } else {
                        isVideoItem = false;
                    }
                }

                if (path.endsWith("Item:Length")) {
                    try {
                        currentLength = Long.parseLong(value);
                    } catch (Exception ignore) {
                        currentLength = null;
                    }
                }

                if (path.endsWith("Item:Padding")) {
                    try {
                        currentPadding = Long.parseLong(value);
                    } catch (Exception ignore) {
                        currentPadding = 0L;
                    }
                }

                // Check if we have gathered Mime, Length, Padding for the current item
                // We assume these properties appear grouped per item, so when we see Length or Padding, we can process accumulated info
                // Or when path is last property of the item, but we have no clear indicator, so we process when Length or Padding is found.

                // Only process when length is not null (Length is mandatory for offset calculation)
                boolean hasLength = currentLength != null;
                if (hasLength) {
                    long paddingVal = currentPadding == null ? 0L : currentPadding.longValue();
                    // Log the current item info
                    SLogs.d(TAG, "Item: Semantic=" + currentSemantic + " Mime=" + currentMime + " Length=" + currentLength + " Padding=" + paddingVal + " accumulatedOffset=" + accumulatedOffset);

                    if (isVideoItem) {
                        // For video item, return the length, and the start offset is accumulatedOffset
                        // Note: The caller uses length and calculates start offset as jpegBytes.length - length,
                        // so here we just return length, but the accumulatedOffset can be used for debugging or future use.
                        return currentLength;
                    }

                    // Accumulate offset for next item
                    accumulatedOffset += currentLength + paddingVal;

                    // Reset current item info for next item
                    currentSemantic = null;
                    currentMime = null;
                    currentLength = null;
                    currentPadding = null;
                    isVideoItem = false;
                    currentItemPath = null;
                }
            }
        } catch (XMPException e) {
            throw new RuntimeException(e);
        }
        return null;
    }

    @Nullable
    private static Long readPtsSafe(XMPMeta meta) {
        if (meta == null) return 0L;
        try {
            String v1Pts = meta.getPropertyString(NS_G_CAMERA, "MicroVideoPresentationTimestampUs");
            String v2Pts = meta.getPropertyString(NS_G_CAMERA, "MotionPhotoPresentationTimestampUs");
            if (!TextUtils.isEmpty(v1Pts)) {
                Long pts = Longs.tryParse(v1Pts);
                SLogs.d(TAG, "Read PTS v1: " + pts);
                return pts;
            } else if (!TextUtils.isEmpty(v2Pts)) {
                Long pts = Longs.tryParse(v2Pts);
                SLogs.d(TAG, "Read PTS v2: " + pts);
                return pts;
            }
            return null;
        } catch (Exception e) {
            return null;
        }
    }

    private static class FtypScanResult {
        final long videoStartOffset;
        final long videoLength;

        FtypScanResult(long start, long length) {
            this.videoStartOffset = start;
            this.videoLength = length;
        }
    }

    /**
     * Scans JPEG bytes for ftyp box to locate embedded video.
     * Performs stricter JPEG structure check (SOI + APP1 segments before SOS) and estimates video length if XMP is missing.
     */
    private static FtypScanResult fallbackLocateVideoWithLength(byte[] jpegBytes) {
        if (jpegBytes == null || jpegBytes.length < 8) {
            throw new IllegalArgumentException("Invalid JPEG data");
        }

        // 1. Check JPEG SOI
        if ((jpegBytes[0] & 0xFF) != 0xFF || (jpegBytes[1] & 0xFF) != 0xD8) {
            throw new IllegalStateException("Not a valid JPEG file (SOI missing)");
        }

        // 2. Optional: skip APP0 / APP1 / XMP segments
        int pos = 2;
        while (pos + 3 < jpegBytes.length) {
            if ((jpegBytes[pos] & 0xFF) != 0xFF) break;
            int marker = jpegBytes[pos + 1] & 0xFF;
            int length = ((jpegBytes[pos + 2] & 0xFF) << 8) | (jpegBytes[pos + 3] & 0xFF);
            if (length < 2) break;
            pos += 2 + length;
            if (marker == 0xDA) break; // SOS start of scan
        }

        // 3. Scan from pos to end for 'ftyp'
        byte[] ftyp = new byte[]{'f', 't', 'y', 'p'};
        for (int i = pos; i <= jpegBytes.length - 4; i++) {
            if (jpegBytes[i] == ftyp[0] &&
                    jpegBytes[i + 1] == ftyp[1] &&
                    jpegBytes[i + 2] == ftyp[2] &&
                    jpegBytes[i + 3] == ftyp[3]) {
                long videoStart = i - 4;
                long videoLength = jpegBytes.length - videoStart;
                SLogs.d(TAG, "FTYP located at offset=" + videoStart + " length=" + videoLength);
                return new FtypScanResult(videoStart, videoLength);
            }
        }

        throw new IllegalStateException("Cannot locate ftyp box, unsupported MotionPhoto structure");
    }
}