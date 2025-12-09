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
import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.stream.Collectors;

public class GoogleMotionPhotoWithJPEGExtractor {
    private static final String TAG = "GoogleMotionPhotoWithJPEGExtractor";

    @Nullable
    public static ExtractResult extractData(File jegFile) throws IOException {
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
    @Nullable
    public static ExtractResult extractData(byte[] jpegBytes) throws IOException {
        MotionPhotoParser.MotionPhotoType motionPhotoType = MotionPhotoParser.checkMotionPhotoType(jpegBytes);
        if (motionPhotoType == MotionPhotoParser.MotionPhotoType.NO) {
            // 如果不是动态照片，返回整个JPEG数据作为图像数据，视频数据为null
            ExtractResult r = new ExtractResult();
            r.primaryBytes = jpegBytes;
            return r;
        }

        XMPMeta xmp = HeifUtils.extractXmpFromBytes(jpegBytes);
        Long pts = readPtsSafe(xmp);
        long ptsValue = pts == null ? 0L : pts;
        SLogs.d(TAG, "MotionPhotoType=" + motionPhotoType + " PTS=" + ptsValue);

        if (motionPhotoType == MotionPhotoParser.MotionPhotoType.FTYP) {
            FtypScanResult ftypResult = fallbackLocateVideoWithLength(jpegBytes);
            // 对于FTYP类型，图像数据是从开始到视频数据之前的部分
            byte[] imageData = extractBytes(jpegBytes, 0, ftypResult.videoStartOffset);
            // 视频数据是从视频起始位置到文件末尾的部分
            byte[] videoData = extractBytes(jpegBytes, ftypResult.videoStartOffset, ftypResult.videoLength);
            ExtractResult extractResult = new ExtractResult();
            extractResult.primaryBytes = imageData;
            extractResult.videoBytes = videoData;
            return extractResult;
        } else {
            return extractMotionItems(jpegBytes, xmp);
        }
    }

    private static byte[] extractBytes(byte[] jpegBytes, long start, long end) {
        if (jpegBytes == null || jpegBytes.length == 0) {
            return null;
        }

        if (start < 0 || end <= start || end > jpegBytes.length || start > jpegBytes.length) {
            SLogs.e(TAG, "Invalid start or end offset");
            return null;
        }

        int imageLength = (int) (end - start);
        byte[] d = new byte[imageLength];
        System.arraycopy(jpegBytes, (int) start, d, 0, imageLength);
        return d;
    }

    private static class ContainerInfo {
        String semantic;
        String mime;
        long offset;
        long length;
        long padding;

        ContainerInfo() {

        }

        ContainerInfo(String semantic, String mime, long offset, long length, long padding) {
            this.semantic = semantic;
            this.mime = mime;
            this.offset = offset;
            this.length = length;
            this.padding = padding;
        }
    }

    public static class ExtractResult {
        public byte[] primaryBytes;
        public byte[] videoBytes;
        public byte[] gainmapBytes;  // 如存在
    }

    /**
     * 从 JPEG MotionPhoto 中提取所有已知 Item（Primary / MotionPhoto / GainMap）。
     * 自动处理所有机型并兼容 padding。
     */
    public static ExtractResult extractMotionItems(byte[] jpegBytes, XMPMeta xmp) {
        List<ContainerInfo> items = parseAllContainers(xmp);
        if (items.isEmpty()) {
            // 非 Motion Photo → 全图像，无视频
            ExtractResult r = new ExtractResult();
            r.primaryBytes = jpegBytes;
            return r;
        }

        // 按 XMP 指定顺序累积偏移
        long jpegLen = jpegBytes.length;

        // 最后一个 item 是文件尾部
        long cursor = jpegLen;

        ExtractResult result = new ExtractResult();

        // 按反序解析（因为最后一个 item 在文件最末尾）
        for (int i = items.size() - 1; i >= 0; i--) {
            ContainerInfo item = items.get(i);

            long total = item.length + item.padding;
            long start = cursor - total;

            if (start < 0 || start >= jpegLen || item.length < 0) {
                continue; // 不非法 crash，继续容错
            }
            byte[] raw;
            if (i == 0 && TextUtils.equals(item.semantic, "Primary")) {
                raw = new byte[(int) cursor];
                start = 0l;
                item.length = cursor;
            } else {
                raw = new byte[(int) item.length];
            }

            System.arraycopy(jpegBytes, (int) start, raw, 0, (int) item.length);

            // 根据 Semantic 分配
            switch (item.semantic) {
                case "Primary":
                    result.primaryBytes = raw;
                    break;
                case "MotionPhoto":
                    result.videoBytes = raw;
                    break;
                case "GainMap":
                    result.gainmapBytes = raw;
                    break;
            }

            // 更新 cursor 指向下一个 Item 前
            cursor = start;
        }

        return result;
    }

    private static List<ContainerInfo> parseAllContainers(XMPMeta xmp) {
        List<ContainerInfo> containers = new ArrayList<>();
        if (xmp == null) return containers;

        try {
            XMPIterator it = xmp.iterator();
            Map<String, Map<String, String>> items = new HashMap<>();

            while (it.hasNext()) {
                XMPPropertyInfo prop = (XMPPropertyInfo) it.next();
                String ns = prop.getNamespace();
                String path = prop.getPath();
                String value = prop.getValue();

                // 只处理 Item 命名空间
                if (!TextUtils.equals(NS_G_CONTAINER_ITEM, ns)) {
                    continue;
                }

                // 提取Item路径和属性名
                int lastSlash = path.lastIndexOf('/');
                if (lastSlash == -1) continue;

                String itemPath = path.substring(0, lastSlash);
                String propName = path.substring(lastSlash + 1);

                // 收集Item属性
                items.computeIfAbsent(itemPath, k -> new HashMap<>()).put(propName, value);
            }

            // 处理收集到的Items
            long accumulatedOffset = 0;
            for (Map.Entry<String, Map<String, String>> entry : items.entrySet()) {
                Map<String, String> props = entry.getValue();
                String semantic = props.get("Item:Semantic");
                String mime = props.get("Item:Mime");
                String lengthStr = props.get("Item:Length");
                String paddingStr = props.get("Item:Padding");

                long length = 0;
                long padding = 0;

                try {
                    if (lengthStr != null) length = Long.parseLong(lengthStr);
                    if (paddingStr != null) padding = Long.parseLong(paddingStr);
                } catch (NumberFormatException e) {
                    SLogs.e(TAG, "Error parsing container length or padding", e);
                    continue;
                }

                containers.add(new ContainerInfo(semantic, mime, accumulatedOffset, length, padding));
                accumulatedOffset += length + padding;
            }
        } catch (XMPException e) {
            SLogs.e(TAG, "Error parsing containers", e);
        }

        return containers.stream().sorted(new Comparator<ContainerInfo>() {
            @Override
            public int compare(ContainerInfo o1, ContainerInfo o2) {
                if (TextUtils.equals(o1.semantic, "Primary")) {
                    return -1;
                }

                if (o1.offset < o2.offset) {
                    return -1;
                }

                if (o1.offset > o2.offset) {
                    return 1;
                }
                return 0;
            }
        }).collect(Collectors.toList());
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
