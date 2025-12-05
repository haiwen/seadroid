package com.seafile.seadroid2.framework.motion_photo;

import com.adobe.internal.xmp.XMPMeta;

import org.apache.commons.io.FileUtils;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

/**
 * Simple Mpvd packer:
 * - reads HEIC bytes, finds XMP packet, updates/insert MotionPhotoPadding
 * - appends a top-level 'mpvd' box whose payload is the mp4 bytes
 * <p>
 * NOTE: For production: use xmpcore to modify XMP robustly (namespaces, encoding, rdf structure).
 */
public final class MpvdPacker {

    private static final String TAG = "MpvdPacker";
    private static final byte[] TYPE_MPVD = {'m', 'p', 'v', 'd'};

    /**
     * Pack heicFile + mp4File into outFile, adding a top-level mpvd box.
     * This method will:
     * - verify input heic does not already contain 'mpvd' top-level box (best-effort)
     * - find XMP packet and update/insert MotionPhotoPadding with headerSize (8 or 16)
     */
    public static void pack(byte[] heic, byte[] mp4, File outFile) throws IOException {

        // quick check: if heic already contains 'mpvd' at top-level tail, abort
        if (containsTopLevelMpvd(heic)) {
            throw new IOException("Input HEIC already contains top-level mpvd box");
        }

        long boxSize = mp4.length;
        // HEIC 必须支持大于 4GB？
        if (boxSize > 0xFFFFFFFFL) {
            throw new IOException("mpvd box too large");
        }


        int mpvdHeaderSize = 8; // header size bytes to be recorded as padding in XMP

        // modify XMP: set MotionPhotoPadding to header size
        byte[] newHeic = updateXmpMotionPhotoPadding(heic, mpvdHeaderSize);

//        try (RandomAccessFile raf = new RandomAccessFile(heicFile, "rw")) {
//
//            // 移动到文件末尾
//            raf.seek(raf.length());
//
//            // 写入 box size（4 字节大端）
//            raf.write(intToBigEndian((int) boxSize), 0, 4);
//
//            // 写入 box type "mpvd"
//            raf.write(TYPE_MPVD);
//
//            // 写入 video bytes
//            raf.write(mp4);
//        }
    }

    /**
     * 将 int 转为 4 字节大端
     */
    private static byte[] intToBigEndian(int value) {
        return new byte[]{
                (byte) ((value >> 24) & 0xFF),
                (byte) ((value >> 16) & 0xFF),
                (byte) ((value >> 8) & 0xFF),
                (byte) (value & 0xFF)
        };
    }

    // Check naive presence of top-level mpvd at the tail: look for 4-byte size followed by 'mpvd' near the end
    private static boolean containsTopLevelMpvd(byte[] heic) {
        // scan last 512 bytes for 'mpvd'
        int scanLen = Math.min(heic.length, 512);
        int start = heic.length - scanLen;
        byte[] needle = "mpvd".getBytes(StandardCharsets.US_ASCII);
        for (int i = start; i <= heic.length - 4; i++) {
            if (heic[i] == needle[0] && heic[i + 1] == needle[1]
                    && heic[i + 2] == needle[2] && heic[i + 3] == needle[3]) {
                // found 'mpvd' sequence near tail; treat as existing
                return true;
            }
        }
        return false;
    }

    /**
     * Find XMP packet in HEIC bytes and update or insert GCamera:MotionPhotoPadding value.
     * Approach:
     * - locate "<x:xmpmeta" ... "</x:xmpmeta>" or locate "<?xpacket begin=" ... "<?xpacket end=",
     * - inside look for tag <GCamera:MotionPhotoPadding>NUMBER</GCamera:MotionPhotoPadding>, replace if present.
     * - if not present, try to insert into rdf:Description element (simple heuristic).
     * <p>
     * Note: This is a heuristic text replacement. For robust editing use Adobe xmpcore.
     */
    private static byte[] updateXmpMotionPhotoPadding(byte[] heicBytes, int paddingValue) throws IOException {
        try {
            // 1. Extract XMP
            String xmp = XmpMotionPhotoHelper.extractXmp(heicBytes);

            // 2. Produce updated XMP
            String newXmp = XmpMotionPhotoHelper.updatePadding(xmp, paddingValue);

            // 3. Replace old XMP inside HEIC
            byte[] newHeic = XmpMotionPhotoHelper.replaceXmp(heicBytes, newXmp);

            return newHeic;
        } catch (Exception e) {
            throw new IOException("Failed to update XMP MotionPhotoPadding", e);
        }
    }


    private static byte[] intToBytes(int v) {
        return ByteBuffer.allocate(4).putInt(v).array();
    }

    private static byte[] longToBytes(long v) {
        return ByteBuffer.allocate(8).putLong(v).array();
    }
}
