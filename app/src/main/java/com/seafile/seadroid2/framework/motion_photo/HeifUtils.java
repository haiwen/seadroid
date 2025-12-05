package com.seafile.seadroid2.framework.motion_photo;

import android.util.Log;

import androidx.annotation.OptIn;
import androidx.exifinterface.media.ExifInterface;
import androidx.media3.common.util.Size;
import androidx.media3.common.util.UnstableApi;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.drew.imaging.ImageMetadataReader;
import com.drew.metadata.Directory;
import com.drew.metadata.Metadata;
import com.drew.metadata.Tag;
import com.drew.metadata.xmp.XmpDirectory;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

/**
 * Utility class for HEIF operations
 */
public class HeifUtils {
    private static final String TAG = "HeifUtils";

//    public static byte[] jpeg2HeicWithMotionPhoto(File jpegMotionPhoto) {
//
//        try {
//
//            Metadata metadata = ImageMetadataReader.readMetadata(jpegMotionPhoto);
//            XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(jpegBytes);
//
//            ExifInterface exif = new ExifInterface("");
//            copyExifDirectory(metadata, ExifIFD0Directory.class, exif);
//            copyExifDirectory(metadata, ExifSubIFDDirectory.class, exif);
//            copyExifDirectory(metadata, GpsDirectory.class, exif);
//            copyExifDirectory(metadata, ExifThumbnailDirectory.class, exif);
//            copyExifDirectory(metadata, ExifInteropDirectory.class, exif);
//
//            ByteArrayOutputStream baos = new ByteArrayOutputStream();
//            exif.saveAttributes();
//            try (FileInputStream fis = new FileInputStream(temp)) {
//                byte[] buf = new byte[4096];
//                int len;
//                while ((len = fis.read(buf)) != -1) {
//                    baos.write(buf, 0, len);
//                }
//            }
//            byte[] datas = FileUtils.readFileToByteArray(jpegMotionPhoto);
//            return jpeg2HeicWithMotionPhoto(datas);
//        } catch (IOException e) {
//            throw new RuntimeException(e);
//        }
//    }

    private static <T extends Directory> void copyExifDirectory(
            Metadata metadata,
            Class<T> dirClass,
            ExifInterface exif) {

        T dir = metadata.getFirstDirectoryOfType(dirClass);
        if (dir == null) return;

        for (Tag tag : dir.getTags()) {
            try {
                String tagName = tag.getTagName();
                String value = tag.getDescription();
                if (value != null) {
                    exif.setAttribute(tagName, value);
                }
            } catch (Exception ignored) {
            }
        }
    }

    public static byte[] extractXMPBytes(File heicFile) throws IOException, XMPException {
        try (RandomAccessFile raf = new RandomAccessFile(heicFile, "r")) {
            long fileLength = raf.length();
            byte[] buffer = new byte[(int) fileLength];
            raf.readFully(buffer);

            String str = new String(buffer, "ISO-8859-1");
            int start = str.indexOf("<x:xmpmeta");
            int end = str.indexOf("</x:xmpmeta>");
            if (start >= 0 && end >= 0) {
                end += "</x:xmpmeta>".length();
                return str.substring(start, end).getBytes("ISO-8859-1");
            }
        }
        return null;
    }

    public static XMPMeta extractXmpFromBytes(byte[] jpegBytes) {
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


    /**
     * 获取JPEG图像尺寸
     */
    @OptIn(markerClass = UnstableApi.class)
    private static Size getImageSize(byte[] jpegData) {
        try {
            int i = 0;
            while (i < jpegData.length - 9) {
                // Check for the start of a marker (0xFF)
                if ((jpegData[i] & 0xFF) == 0xFF) {
                    int marker = jpegData[i + 1] & 0xFF;
                    // SOF0 (0xC0) or SOF2 (0xC2) markers contain image dimensions
                    if (marker == 0xC0 || marker == 0xC2) {
                        int height = ((jpegData[i + 5] & 0xFF) << 8) | (jpegData[i + 6] & 0xFF);
                        int width = ((jpegData[i + 7] & 0xFF) << 8) | (jpegData[i + 8] & 0xFF);
                        return new Size(width, height);
                    }
                }
                i++;
            }
        } catch (Exception e) {
            Log.e(TAG, "Error getting image size: " + e.getMessage());
        }
        return new Size(0, 0); // 默认尺寸
    }

    private static void copyExifToHeic(File heicFile, ExifInterface srcExif) throws IOException {
        // ExifInterface 仅在部分 Android 版本支持 HEIC 写入，依赖实现。
        // 这里把常见字段复制过去。
        ExifInterface heicExif = new ExifInterface(heicFile.getAbsolutePath());
        String[] tags = new String[]{
                ExifInterface.TAG_MAKE,
                ExifInterface.TAG_MODEL,
                ExifInterface.TAG_ORIENTATION,
                ExifInterface.TAG_DATETIME,
                ExifInterface.TAG_GPS_LATITUDE,
                ExifInterface.TAG_GPS_LONGITUDE,
                ExifInterface.TAG_F_NUMBER,
                ExifInterface.TAG_EXPOSURE_TIME,
                ExifInterface.TAG_ISO_SPEED_RATINGS,
                ExifInterface.TAG_FOCAL_LENGTH
        };
        for (String tag : tags) {
            String v = srcExif.getAttribute(tag);
            if (v != null) heicExif.setAttribute(tag, v);
        }
        heicExif.saveAttributes();
    }

}