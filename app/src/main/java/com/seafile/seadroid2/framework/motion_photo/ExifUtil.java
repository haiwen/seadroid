package com.seafile.seadroid2.framework.motion_photo;

import java.io.ByteArrayInputStream;
import java.io.IOException;

/**
 * Simple EXIF extractor: scan JPEG markers for APP1 with "Exif\0\0" header and return its payload
 * (the APP1 content starting from "Exif\0\0" inclusive). Returns null if not found.
 *
 * This is conservative: it returns the full APP1 bytes (Exif header + TIFF data), which is suitable for HeifWriter.setExif().
 */
public final class ExifUtil {
    private ExifUtil(){}

    public static byte[] extractExifFromJpeg(byte[] jpeg) throws IOException {
        if (jpeg == null || jpeg.length < 4) return null;
        int idx = 2; // skip SOI 0xFF 0xD8
        while (idx + 4 < jpeg.length) {
            if ((jpeg[idx] & 0xFF) != 0xFF) break;
            int marker = jpeg[idx + 1] & 0xFF;
            idx += 2;
            // EOI or SOS
            if (marker == 0xDA || marker == 0xD9) break;
            int len = ((jpeg[idx] & 0xFF) << 8) | (jpeg[idx + 1] & 0xFF);
            idx += 2;
            if (len < 2) break;
            int dataLen = len - 2;
            if (marker == 0xE1 && dataLen >= 6) { // APP1
                // check "Exif\0\0"
                if (idx + 6 <= jpeg.length) {
                    String sig = new String(jpeg, idx, Math.min(6, dataLen), java.nio.charset.StandardCharsets.US_ASCII);
                    if (sig.startsWith("Exif")) {
                        // return EXIF APP1 data including "Exif\0\0" header
                        byte[] app1 = new byte[2 + 2 + dataLen]; // marker(2) + len(2) + data
                        // construct full APP1 segment with marker and length (for HeifWriter we only need data but keep full)
                        // Here return only data (without marker/len), HeifWriter expects EXIF bytes starting with "Exif\0\0"
                        byte[] data = new byte[dataLen];
                        System.arraycopy(jpeg, idx, data, 0, dataLen);
                        return data;
                    }
                }
            }
            idx += dataLen;
        }
        return null;
    }
}