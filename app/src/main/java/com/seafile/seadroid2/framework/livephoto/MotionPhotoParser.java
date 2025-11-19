package com.seafile.seadroid2.framework.livephoto;

import com.adobe.internal.xmp.XMPException;
import com.adobe.internal.xmp.XMPMeta;
import com.drew.imaging.ImageMetadataReader;
import com.drew.metadata.Metadata;
import com.drew.metadata.xmp.XmpDirectory;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.annotation.Unstable;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;

@Todo
@Unstable
public final class MotionPhotoParser {

    public static final class Result {
        public final boolean isMotionPhoto;
        public final long presentationTimestampUs;
        public final long videoStartOffset;
        public final long videoLength;

        public Result(boolean isMotionPhoto, long pts, long start, long length) {
            this.isMotionPhoto = isMotionPhoto;
            this.presentationTimestampUs = pts;
            this.videoStartOffset = start;
            this.videoLength = length;
        }
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
        boolean isMotion = isMotionPhoto(xmp);
        if (!isMotion) {
            return new Result(false, 0, -1, -1);
        }

        long pts = readLongSafe(xmp, "http://ns.google.com/photos/1.0/camera/", "MotionPhotoPresentationTimestampUs");

        // Step A: 从 Container 结构中获取视频长度
        Long containerVideoLength = parseContainerVideoLength(xmp);

        if (containerVideoLength != null) {
            long videoLength = containerVideoLength;
            long videoStart = jpegBytes.length - videoLength;
            if (videoStart < 0) {
                videoStart = fallbackLocateVideo(jpegBytes);
                videoLength = jpegBytes.length - videoStart;
            }
            return new Result(true, pts, videoStart, videoLength);
        }

        // Step B: fallback → 直接扫描 ftyp box
        long start = fallbackLocateVideo(jpegBytes);
        long length = jpegBytes.length - start;
        return new Result(true, pts, start, length);
    }

    // ============================
    // XMP 相关
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

    private static boolean isMotionPhoto(XMPMeta xmp) {
        if (xmp == null) return false;
        try {
            String v = xmp.getPropertyString("http://ns.google.com/photos/1.0/camera/", "MotionPhoto");
            return "1".equals(v);
        } catch (XMPException e) {
            return false;
        }
    }

    /**
     * 解析：
     * <Container:Directory><rdf:Seq><rdf:li><Container:Item Item:Mime="video/mp4" Item:Semantic="MotionPhoto" Item:Length="XYZ" />
     */
    private static Long parseContainerVideoLength(XMPMeta xmp) {
        if (xmp == null) return null;

        try {
            final String nsContainer = "http://ns.google.com/photos/1.0/container/";
            final String nsItem = "http://ns.google.com/photos/1.0/container/item/";

            int seqSize = xmp.countArrayItems(nsContainer, "Directory");
            if (seqSize <= 0) return null;

            for (int i = 1; i <= seqSize; i++) {
                String basePath = String.format("Directory[%d]", i);

                String semantic = xmp.getPropertyString(nsItem,  "Semantic");
                String mime = xmp.getPropertyString(nsItem,  "Item:Mime");

                if ("MotionPhoto".equals(semantic) && "video/mp4".equals(mime)) {
                    String lenStr = xmp.getPropertyString(nsItem, basePath + "/Item:Length");
                    if (lenStr != null) {
                        try {
                            return Long.parseLong(lenStr);
                        } catch (NumberFormatException ignore) {
                        }
                    }
                }
            }
        } catch (XMPException ignore) {
        }

        return null;
    }

    private static long readLongSafe(XMPMeta meta, String ns, String name) {
        if (meta == null) return 0;
        try {
            String v = meta.getPropertyString(ns, name);
            return v != null ? Long.parseLong(v) : 0;
        } catch (Exception e) {
            return 0;
        }
    }

    // ============================
    // Fallback：ftyp 扫描
    // ============================

    private static long fallbackLocateVideo(byte[] jpegBytes) {
        byte[] target = new byte[]{'f', 't', 'y', 'p'};
        for (int i = jpegBytes.length - 4; i >= 0; i--) {
            if (jpegBytes[i] == target[0] &&
                    jpegBytes[i + 1] == target[1] &&
                    jpegBytes[i + 2] == target[2] &&
                    jpegBytes[i + 3] == target[3]) {
                return i - 4; // MP4 box header size field
            }
        }
        throw new IllegalStateException("Cannot locate ftyp box, unsupported MotionPhoto structure");
    }
}