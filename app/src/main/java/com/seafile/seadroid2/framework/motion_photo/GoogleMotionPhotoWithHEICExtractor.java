package com.seafile.seadroid2.framework.motion_photo;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.adobe.internal.xmp.XMPMetaFactory;
import com.adobe.internal.xmp.options.ParseOptions;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.nio.channels.Channels;
import java.nio.channels.FileChannel;

public class GoogleMotionPhotoWithHEICExtractor {
    private static final String TAG = "GoogleMotionPhotoWithHeicExtractor";

    /**
     * 检查 HEIC 是否包含 Motion Photo 视频
     *
     * @param heicFile HEIC 文件
     * @return true 表示存在视频，false 表示纯图片
     */
    public static boolean hasMotionVideo(byte[] xmpBytes) throws IOException, XMPException {
        XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(xmpBytes, new ParseOptions());
        try {
            long offset = xmpMeta.getPropertyLong(
                    "http://ns.google.com/photos/1.0/camera/", "MicroVideoOffset");
            return offset > 0;
        } catch (XMPException e) {
            return false; // 没有 MicroVideoOffset，说明没有视频
        }
    }

    /**
     * 提取视频数据为 byte[]
     *
     * @param heicFile Google Motion Photo HEIC 文件
     * @return 视频字节数组
     */
    public static byte[] extractVideo(File heicFile) throws IOException, XMPException {
        byte[] xmpBytes = HeifUtils.extractXMPBytes(heicFile); // 从HEIC meta读取XMP
        if (xmpBytes == null) {
            throw new IOException("XMP not found in HEIC");
        }

        XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(xmpBytes, new ParseOptions());
        long microVideoOffset = xmpMeta.getPropertyLong("http://ns.google.com/photos/1.0/camera/", "MicroVideoOffset");

        try (RandomAccessFile raf = new RandomAccessFile(heicFile, "r")) {
            long videoLength = raf.length() - microVideoOffset;
            if (videoLength <= 0) {
                throw new IOException("Invalid MicroVideoOffset or empty video data");
            }

            byte[] videoBytes = new byte[(int) videoLength];
            raf.seek(microVideoOffset);
            raf.readFully(videoBytes);
            return videoBytes;
        }
    }

//    /**
//     * 提取视频数据为 byte[]（重载方法，接受字节数组）
//     *
//     * @param heicBytes Google Motion Photo HEIC 文件字节数据
//     * @return 视频字节数组
//     */
//    public static byte[] extractVideo(byte[] heicBytes) throws IOException, XMPException {
//        byte[] xmpBytes = HeifUtils.extractXMPBytes(heicBytes); // 从HEIC meta读取XMP
//        if (xmpBytes == null) {
//            throw new IOException("XMP not found in HEIC");
//        }
//
//        XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(xmpBytes, new ParseOptions());
//        long microVideoOffset = xmpMeta.getPropertyLong("http://ns.google.com/photos/1.0/camera/", "MicroVideoOffset");
//
//        long videoLength = heicBytes.length - microVideoOffset;
//        if (videoLength <= 0) {
//            throw new IOException("Invalid MicroVideoOffset or empty video data");
//        }
//
//        byte[] videoBytes = new byte[(int) videoLength];
//        System.arraycopy(heicBytes, (int) microVideoOffset, videoBytes, 0, (int) videoLength);
//        return videoBytes;
//    }

    /**
     * 提取视频数据到输出流
     *
     * @param heicFile     Google Motion Photo HEIC 文件
     * @param outputStream 输出流
     */
    public static void extractVideo(File heicFile, OutputStream outputStream) throws IOException, XMPException {
        byte[] xmpBytes = HeifUtils.extractXMPBytes(heicFile); // 从HEIC meta读取XMP
        if (xmpBytes == null) {
            throw new IOException("XMP not found in HEIC");
        }

        XMPMeta xmpMeta = XMPMetaFactory.parseFromBuffer(xmpBytes, new ParseOptions());
        long microVideoOffset = xmpMeta.getPropertyLong("http://ns.google.com/photos/1.0/camera/", "MicroVideoOffset");

        try (RandomAccessFile raf = new RandomAccessFile(heicFile, "r");
             FileChannel channel = raf.getChannel()) {

            long videoLength = raf.length() - microVideoOffset;
            if (videoLength <= 0) {
                throw new IOException("Invalid MicroVideoOffset or empty video data");
            }

            // 使用NIO传输数据，更高效
            channel.transferTo(microVideoOffset, videoLength, Channels.newChannel(outputStream));
        }
    }

}
