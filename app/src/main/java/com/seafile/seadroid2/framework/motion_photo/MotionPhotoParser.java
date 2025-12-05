package com.seafile.seadroid2.framework.motion_photo;

import android.content.ContentResolver;
import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import androidx.annotation.Nullable;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPIterator;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.properties.XMPPropertyInfo;
import com.google.common.primitives.Longs;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.annotation.Unstable;
import com.seafile.seadroid2.framework.util.SLogs;

import org.apache.commons.io.FileUtils;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;

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
        MICRO_VIDEO,// v1, motion photo based on JPEG format (image+xmp+video)
        JPEG_MOTION_PHOTO,// v2,motion photo based on JPEG format (image+xmp+video)
        HEIC_MOTION_PHOTO,// other,motion photo based on HEIC format (ISO BMFF:image+video)
        FTYP, // huawei motion photo based on JPEG format (image+video)
        NO;

        public boolean isMotionPhoto() {
            return this == JPEG_MOTION_PHOTO || this == HEIC_MOTION_PHOTO;
        }
    }

    public static MotionPhotoType checkMotionPhotoType(String path) throws IOException {
        byte[] bytes;
        if (path.startsWith("content://")) {
            bytes = readBytesFromContentUri(path);
        } else {
            File imageFile = new File(path);
            bytes = FileUtils.readFileToByteArray(imageFile);
        }
        return checkMotionPhotoType(bytes);
    }

    /**
     * Check if the image file is a motion photo and its type
     *
     * @return MotionPhotoType 动态照片类型
     */
    public static MotionPhotoType checkMotionPhotoType(byte[] bytes) throws IOException {
        if (bytes == null) {
            return MotionPhotoType.NO;
        }

        if (bytes.length == 0) {
            return MotionPhotoType.NO;
        }

        if (bytes.length < 12) {
            return MotionPhotoType.NO;
        }

        // 判断文件格式
        String format = detectImageFormat(bytes);

        if ("JPEG".equals(format)) {
            // 尝试提取XMP
            XMPMeta xmpMeta = HeifUtils.extractXmpFromBytes(bytes);
            if (xmpMeta != null) {
                // 检查是否为MICRO_VIDEO
                try {
                    String microVideo = xmpMeta.getPropertyString("http://ns.google.com/photos/1.0/camera/", "MicroVideo");
                    if ("1".equals(microVideo)) {
                        return MotionPhotoType.MICRO_VIDEO;
                    }
                } catch (XMPException e) {
                    // 忽略异常，继续检查其他类型
                }

                // 检查是否为JPEG_MOTION_PHOTO
                try {
                    String motionPhoto = xmpMeta.getPropertyString("http://ns.google.com/photos/1.0/camera/", "MotionPhoto");
                    if ("1".equals(motionPhoto)) {
                        return MotionPhotoType.JPEG_MOTION_PHOTO;
                    }
                } catch (XMPException e) {
                    // 忽略异常，继续检查其他类型
                }
            }

            // 如果提取不到XMP或XMP中没有相关属性，检查是否包含ftyp关键字
            return checkMotionPhotoByFtyp(bytes);
        } else if ("HEIC".equals(format)) {
            // 对于HEIC格式，使用GoogleMotionPhotoWithHeicExtractor判断
            try {
                if (GoogleMotionPhotoWithHEICExtractor.hasMotionVideo(bytes)) {
                    return MotionPhotoType.HEIC_MOTION_PHOTO;
                }
            } catch (XMPException e) {
                // 如果出现XMP异常，认为不是动态照片
                return MotionPhotoType.NO;
            }
        }

        return MotionPhotoType.NO;
    }

    /**
     * 从 content:// URI 读取文件字节数据
     *
     * @param contentUri content:// URI 字符串
     * @return 文件字节数据
     */
    public static byte[] readBytesFromContentUri(String contentUri) throws IOException {
        Context context = SeadroidApplication.getAppContext(); // 获取应用程序上下文
        ContentResolver contentResolver = context.getContentResolver();
        Uri uri = Uri.parse(contentUri);

        try (InputStream inputStream = contentResolver.openInputStream(uri);
             ByteArrayOutputStream buffer = new ByteArrayOutputStream()) {

            if (inputStream == null) {
                throw new IOException("Unable to open input stream for URI: " + contentUri);
            }

            byte[] data = new byte[16384]; // 16KB 缓冲区
            int nRead;
            while ((nRead = inputStream.read(data, 0, data.length)) != -1) {
                buffer.write(data, 0, nRead);
            }

            buffer.flush();
            return buffer.toByteArray();
        } catch (Exception e) {
            throw new IOException("Error reading from content URI: " + contentUri, e);
        }
    }

    /**
     * 检测图片格式
     *
     * @param header 文件头部字节
     * @return "JPEG" 或 "HEIC" 或 "UNKNOWN"
     */
    private static String detectImageFormat(byte[] header) {
        // JPEG格式检测 (SOI标记: 0xFFD8)
        if ((header[0] & 0xFF) == 0xFF && (header[1] & 0xFF) == 0xD8) {
            return "JPEG";
        }

        // HEIC格式检测 (ftyp box)
        // HEIC文件通常以长度(4字节) + ftyp(4字节) + brand(4字节) 开始
        // 其中brand可能是heic, heix, mif1等
        if (header[4] == 'f' && header[5] == 't' && header[6] == 'y' && header[7] == 'p') {
            // 检查brand是否为HEIC相关
            String brand = new String(header, 8, 4, StandardCharsets.US_ASCII);
            if ("heic".equals(brand) || "heix".equals(brand) || "mif1".equals(brand)) {
                return "HEIC";
            }
        }

        return "UNKNOWN";
    }

    private static MotionPhotoType checkMotionPhotoByFtyp(byte[] jpegBytes) {
        byte[] target = new byte[]{'f', 't', 'y', 'p'};
        for (int i = jpegBytes.length - 4; i >= 0; i--) {
            if (jpegBytes[i] == target[0] && jpegBytes[i + 1] == target[1] && jpegBytes[i + 2] == target[2] && jpegBytes[i + 3] == target[3]) {
                return MotionPhotoType.FTYP;
            }
        }
        return MotionPhotoType.NO;
    }
}