package com.seafile.seadroid2.framework.motion_photo;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.XMPMetaFactory;
import com.adobe.internal.xmp.XMPIterator;
import com.adobe.internal.xmp.options.ParseOptions;
import com.adobe.internal.xmp.properties.XMPPropertyInfo;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;

public class GoogleMotionPhotoWithHEICExtractor2 {
    private static final String TAG = "GoogleMotionPhotoWithHeicExtractor";

    /**
     * 检查 HEIC 是否包含 Motion Photo 视频
     * 支持两种格式：MicroVideoOffset 和 Container:Directory + mpvd
     */
    public static boolean hasMotionVideo(byte[] xmpBytes) throws IOException, XMPException {
        if (xmpBytes == null || xmpBytes.length == 0) {
            return false;
        }

        XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(xmpBytes, new ParseOptions());
        
        // 首先尝试 MicroVideoOffset 格式（兼容旧格式）
        try {
            long offset = xmpMeta.getPropertyLong(
                    "http://ns.google.com/photos/1.0/camera/", "MicroVideoOffset");
            if (offset > 0) {
                return true;
            }
        } catch (XMPException ignored) {
        }

        // 然后尝试 Container:Directory 格式（HEIC MotionPhoto 标准格式）
        try {
            XMPIterator iterator = xmpMeta.iterator();
            while (iterator.hasNext()) {
                XMPPropertyInfo prop = (XMPPropertyInfo) iterator.next();
                String path = prop.getPath();
                
                // 检查是否有 Container:Directory
                if (path != null && path.contains("Container:Directory")) {
                    // 检查是否有视频 item
                    String value = prop.getValue();
                    if (value != null && value.contains("video/mp4") && value.contains("MotionPhoto")) {
                        return true;
                    }
                }
                
                // 检查 GCamera:MotionPhoto 标记
                if ("GCamera:MotionPhoto".equals(prop.getPath())) {
                    String value = prop.getValue();
                    if ("1".equals(value)) {
                        return true;
                    }
                }
            }
        } catch (XMPException ignored) {
        }

        return false;
    }

    /**
     * 提取视频数据为 byte[]
     * 支持 Container:Directory + mpvd box 格式
     */
    public static byte[] extractVideo(File heicFile) throws IOException, XMPException {
        byte[] xmpBytes = HeifUtils.extractXMPBytes(heicFile);
        if (xmpBytes == null) {
            throw new IOException("XMP not found in HEIC");
        }

        XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(xmpBytes, new ParseOptions());
        
        // 首先尝试 MicroVideoOffset 格式（向后兼容）
        try {
            long microVideoOffset = xmpMeta.getPropertyLong("http://ns.google.com/photos/1.0/camera/", "MicroVideoOffset");
            if (microVideoOffset > 0) {
                return extractVideoByOffset(heicFile, microVideoOffset);
            }
        } catch (XMPException ignored) {
        }

        // 尝试 Container:Directory + mpvd box 格式
        return extractVideoFromMpvdBox(heicFile);
    }

    /**
     * 从 mpvd box 提取视频数据（HEIC MotionPhoto 标准格式）
     */
    private static byte[] extractVideoFromMpvdBox(File heicFile) throws IOException {
        try (RandomAccessFile raf = new RandomAccessFile(heicFile, "r")) {
            long fileLength = raf.length();
            
            // 从文件末尾向前扫描，寻找 mpvd box
            // mpvd box 格式：size(4) + 'mpvd'(4) + video_data
            long scanStart = Math.max(0, fileLength - 1024); // 扫描最后 1KB
            
            for (long pos = scanStart; pos < fileLength - 8; pos++) {
                raf.seek(pos);
                long size = readUInt32(raf);
                String type = read4cc(raf);
                
                if ("mpvd".equals(type) && size > 8 && pos + size <= fileLength) {
                    // 找到了 mpvd box
                    long videoSize = size - 8; // 减去 header 大小
                    byte[] videoBytes = new byte[(int) videoSize];
                    raf.readFully(videoBytes);
                    return videoBytes;
                }
                
                // 跳到下一个可能的 box 位置
                if (size > 0 && size < fileLength - pos) {
                    pos += size - 8; // -8 因为已经读了 8 字节
                }
            }
        }
        
        throw new IOException("mpvd box not found in HEIC file");
    }

    /**
     * 通过偏移量提取视频数据（兼容旧格式）
     */
    private static byte[] extractVideoByOffset(File heicFile, long offset) throws IOException {
        try (RandomAccessFile raf = new RandomAccessFile(heicFile, "r")) {
            long videoLength = raf.length() - offset;
            if (videoLength <= 0) {
                throw new IOException("Invalid MicroVideoOffset or empty video data");
            }

            byte[] videoBytes = new byte[(int) videoLength];
            raf.seek(offset);
            raf.readFully(videoBytes);
            return videoBytes;
        }
    }

    /**
     * 提取视频数据到输出流
     */
    public static void extractVideo(File heicFile, OutputStream outputStream) throws IOException, XMPException {
        byte[] videoBytes = extractVideo(heicFile);
        outputStream.write(videoBytes);
    }

    private static long readUInt32(RandomAccessFile raf) throws IOException {
        byte[] b = new byte[4];
        raf.readFully(b);
        return ((b[0] & 0xFFL) << 24) | ((b[1] & 0xFFL) << 16) | ((b[2] & 0xFFL) << 8) | (b[3] & 0xFFL);
    }

    private static String read4cc(RandomAccessFile raf) throws IOException {
        byte[] t = new byte[4];
        raf.readFully(t);
        return new String(t, java.nio.charset.StandardCharsets.US_ASCII);
    }
}
