package com.seafile.seadroid2.framework.motion_photo;

import com.adobe.internal.xmp.XMPMeta;

import org.apache.commons.io.FileUtils;

import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
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

//    public static void pack(File originalJpegFile, File mp4File, File outFile) throws IOException {
//        byte[] jpegBytes = FileUtils.readFileToByteArray(originalJpegFile);
////        XMPMeta xmpMeta = MotionPhotoParser.extractXmp(jpegBytes);
//
//        byte[] heic = HeifUtils.jpegToHeif(jpegBytes);
//        byte[] mp4 = FileUtils.readFileToByteArray(mp4File);
//
//        pack(heic, mp4, outFile);
//    }

    /**
     * Pack heicFile + mp4File into outFile, adding a top-level mpvd box.
     * This method will:
     * - verify input heic does not already contain 'mpvd' top-level box (best-effort)
     * - find XMP packet and update/insert MotionPhotoPadding with headerSize (8 or 16)
     */
//    public static void pack(byte[] heic, byte[] mp4, File outFile) throws IOException {
//        pack(heic, mp4, outFile);
//    }

    /**
     * Pack heicFile + mp4File into outFile, adding a top-level mpvd box.
     * This method will:
     * - verify input heic does not already contain 'mpvd' top-level box (best-effort)
     * - find XMP packet and update/insert MotionPhotoPadding with headerSize (8 or 16)
     */
    public static void pack(byte[] heic, byte[] mp4, XMPMeta xmpMeta, File outFile) throws IOException {

        // quick check: if heic already contains 'mpvd' at top-level tail, abort
        if (containsTopLevelMpvd(heic)) {
            throw new IOException("Input HEIC already contains top-level mpvd box");
        }


        // determine mpvd header size: normal 32-bit size -> header 8 bytes (4 size + 4 type)
        // Note: For typical mobile use cases, mp4 files are unlikely to exceed 4GB,
        // so useLargeSize will almost always be false, resulting in mpvdHeaderSize = 8
        long totalPayload = mp4.length;
        boolean useLargeSize = (totalPayload + 8L) > 0xFFFFFFFFL;
        int mpvdHeaderSize = useLargeSize ? 16 : 8; // header size bytes to be recorded as padding in XMP

        // modify XMP: set MotionPhotoPadding to header size
        byte[] newHeic = updateXmpMotionPhotoPadding(heic, mpvdHeaderSize);

        // write new file: newHeic + mpvd box header + mp4 bytes
        try (FileOutputStream fos = new FileOutputStream(outFile);
             BufferedOutputStream bos = new BufferedOutputStream(fos)) {

            // write heic prolog (modified)
            bos.write(newHeic);

            // construct mpvd box header
            long fullBoxSize = mpvdHeaderSize + totalPayload; // header + payload
            if (!useLargeSize) {
                // 32-bit size fits
                bos.write(intToBytes((int) fullBoxSize)); // 4 bytes size
                bos.write("mpvd".getBytes(StandardCharsets.US_ASCII)); // 4 bytes type
            } else {
                // write size=1 then type then 8-byte largesize
                bos.write(intToBytes(1)); // size field = 1 indicates largesize used
                bos.write("mpvd".getBytes(StandardCharsets.US_ASCII));
                bos.write(longToBytes(fullBoxSize)); // 8-byte largesize
            }

            // write payload (the entire mp4 file bytes)
            bos.write(mp4);

            bos.flush();
        }
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
