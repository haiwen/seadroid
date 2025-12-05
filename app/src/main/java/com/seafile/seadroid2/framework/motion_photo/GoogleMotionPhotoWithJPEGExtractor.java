package com.seafile.seadroid2.framework.motion_photo;

import static com.seafile.seadroid2.framework.motion_photo.MotionPhotoParser.NS_G_CAMERA;
import static com.seafile.seadroid2.framework.motion_photo.MotionPhotoParser.NS_G_CONTAINER_ITEM;

import android.text.TextUtils;

import androidx.annotation.Nullable;
import androidx.core.util.Pair;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPIterator;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.properties.XMPPropertyInfo;
import com.drew.imaging.ImageMetadataReader;
import com.drew.imaging.jpeg.JpegMetadataReader;
import com.google.common.primitives.Longs;
import com.seafile.seadroid2.framework.util.SLogs;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.io.IOException;

public class GoogleMotionPhotoWithJPEGExtractor {
    private static final String TAG = "GoogleMotionPhotoWithJPEGExtractor";

    public static Pair<byte[], byte[]> extractData(File jegFile) throws IOException {
        if (jegFile == null || !jegFile.exists() || !jegFile.isFile()) {
            return null;
        }

        byte[] bytes = FileUtils.readFileToByteArray(jegFile);
        return extractData(bytes);
    }

    /**
     * 从JPEG动态照片中提取图像和视频数据
     *
     * @param jpegBytes JPEG动态照片字节数组
     * @return Pair对象，第一个元素是图像数据字节数组，第二个元素是视频数据字节数组
     */
    public static Pair<byte[], byte[]> extractData(byte[] jpegBytes) throws IOException {
        MotionPhotoParser.MotionPhotoType motionPhotoType = MotionPhotoParser.checkMotionPhotoType(jpegBytes);
        if (motionPhotoType == MotionPhotoParser.MotionPhotoType.NO) {
            // 如果不是动态照片，返回整个JPEG数据作为图像数据，视频数据为null
            return new Pair<>(jpegBytes, null);
        }

        XMPMeta xmp = HeifUtils.extractXmpFromBytes(jpegBytes);
        Long pts = readPtsSafe(xmp);
        long ptsValue = pts == null ? 0L : pts;
        SLogs.d(TAG, "MotionPhotoType=" + motionPhotoType + " PTS=" + ptsValue);

        if (motionPhotoType == MotionPhotoParser.MotionPhotoType.FTYP) {
            FtypScanResult ftypResult = fallbackLocateVideoWithLength(jpegBytes);
            // 对于FTYP类型，图像数据是从开始到视频数据之前的部分
            byte[] imageData = extractImageData(jpegBytes, 0, ftypResult.videoStartOffset);
            // 视频数据是从视频起始位置到文件末尾的部分
            byte[] videoData = extractVideoData(jpegBytes, ftypResult.videoStartOffset, ftypResult.videoLength);
            return new Pair<>(imageData, videoData);
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

        // 图像数据是从开始到视频数据之前的部分
        byte[] imageData = extractImageData(jpegBytes, 0, videoStart);
        // 视频数据是从视频起始位置到文件末尾的部分
        byte[] videoData = extractVideoData(jpegBytes, videoStart, videoLength);

        return new Pair<>(imageData, videoData);
    }

    public static byte[] extractImageData(byte[] jpegBytes) throws IOException {
        MotionPhotoParser.MotionPhotoType motionPhotoType = MotionPhotoParser.checkMotionPhotoType(jpegBytes);
        if (motionPhotoType == MotionPhotoParser.MotionPhotoType.NO) {
            // 如果不是动态照片，返回整个JPEG数据
            return jpegBytes;
        }

        XMPMeta xmp = HeifUtils.extractXmpFromBytes(jpegBytes);
        Long pts = readPtsSafe(xmp);
        long ptsValue = pts == null ? 0L : pts;
        SLogs.d(TAG, "MotionPhotoType=" + motionPhotoType + " PTS=" + ptsValue);

        if (motionPhotoType == MotionPhotoParser.MotionPhotoType.FTYP) {
            FtypScanResult ftypResult = fallbackLocateVideoWithLength(jpegBytes);
            // 对于FTYP类型，图像数据是从开始到视频数据之前的部分
            return extractImageData(jpegBytes, 0, ftypResult.videoStartOffset);
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

        // 图像数据是从开始到视频数据之前的部分
        return extractImageData(jpegBytes, 0, videoStart);
    }

    public static byte[] extractVideoData(byte[] jpegBytes) throws IOException {
        MotionPhotoParser.MotionPhotoType motionPhotoType = MotionPhotoParser.checkMotionPhotoType(jpegBytes);
        if (motionPhotoType == MotionPhotoParser.MotionPhotoType.NO) {
            SLogs.d(TAG, "MotionPhotoType=JPEG");
            return null;
        }

        XMPMeta xmp = HeifUtils.extractXmpFromBytes(jpegBytes);
        Long pts = readPtsSafe(xmp);
        long ptsValue = pts == null ? 0L : pts;
        SLogs.d(TAG, "MotionPhotoType=" + motionPhotoType + " PTS=" + ptsValue);

        if (motionPhotoType == MotionPhotoParser.MotionPhotoType.FTYP) {
            FtypScanResult ftypResult = fallbackLocateVideoWithLength(jpegBytes);
            return extractVideoData(jpegBytes, ftypResult.videoStartOffset, ftypResult.videoLength);
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

        return extractVideoData(jpegBytes, videoStart, videoLength);
    }

    /**
     * 从字节数组中提取图像数据
     *
     * @param jpegBytes  原始字节数组
     * @param imageStart 图像数据起始位置
     * @param imageEnd   图像数据结束位置
     * @return 图像数据字节数组
     */
    private static byte[] extractImageData(byte[] jpegBytes, long imageStart, long imageEnd) {
        if (imageStart < 0 || imageEnd <= imageStart || imageEnd > jpegBytes.length) {
            SLogs.e(TAG, "Invalid image start or end offset");
            return null;
        }

        try {
            int imageLength = (int) (imageEnd - imageStart);
            byte[] imageData = new byte[imageLength];
            System.arraycopy(jpegBytes, (int) imageStart, imageData, 0, imageLength);
            return imageData;
        } catch (Exception e) {
            SLogs.e(TAG, "Error extracting image data", e);
            return null;
        }
    }

    /**
     * Extract video data from byte arrays based on start position and length
     *
     * @param jpegBytes   原始字节数组
     * @param videoStart  视频数据起始位置
     * @param videoLength 视频数据长度
     * @return 视频数据字节数组
     */
    private static byte[] extractVideoData(byte[] jpegBytes, long videoStart, long videoLength) {
        if (videoStart < 0 || videoLength <= 0 || videoStart + videoLength > jpegBytes.length) {
            SLogs.e(TAG, "Invalid video start offset or length");
            return null;
        }

        try {
            byte[] videoData = new byte[(int) videoLength];
            System.arraycopy(jpegBytes, (int) videoStart, videoData, 0, (int) videoLength);
            return videoData;
        } catch (Exception e) {
            SLogs.e(TAG, "Error extracting video data", e);
            return null;
        }
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
        if (xmp == null)
            return null;

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
                // We assume these properties appear grouped per item, so when we see Length or
                // Padding, we can process accumulated info
                // Or when path is last property of the item, but we have no clear indicator, so
                // we process when Length or Padding is found.

                // Only process when length is not null (Length is mandatory for offset
                // calculation)
                boolean hasLength = currentLength != null;
                if (hasLength) {
                    long paddingVal = currentPadding == null ? 0L : currentPadding.longValue();
                    // Log the current item info
                    SLogs.d(TAG, "Item: Semantic=" + currentSemantic + " Mime=" + currentMime + " Length="
                            + currentLength + " Padding=" + paddingVal + " accumulatedOffset=" + accumulatedOffset);

                    if (isVideoItem) {
                        // For video item, return the length, and the start offset is accumulatedOffset
                        // Note: The caller uses length and calculates start offset as jpegBytes.length
                        // - length,
                        // so here we just return length, but the accumulatedOffset can be used for
                        // debugging or future use.
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
        if (meta == null)
            return 0L;
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
     * Performs stricter JPEG structure check (SOI + APP1 segments before SOS) and
     * estimates video length if XMP is missing.
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
            if ((jpegBytes[pos] & 0xFF) != 0xFF)
                break;
            int marker = jpegBytes[pos + 1] & 0xFF;
            int length = ((jpegBytes[pos + 2] & 0xFF) << 8) | (jpegBytes[pos + 3] & 0xFF);
            if (length < 2)
                break;
            pos += 2 + length;
            if (marker == 0xDA)
                break; // SOS start of scan
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
