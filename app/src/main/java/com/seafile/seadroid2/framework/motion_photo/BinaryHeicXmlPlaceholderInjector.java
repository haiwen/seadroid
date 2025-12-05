package com.seafile.seadroid2.framework.motion_photo;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

/**
 * BinaryHeicMetaExpander
 * <p>
 * - Insert or replace xml box in top-level meta box, expanding meta if needed.
 * - Parse iloc box inside meta and adjust absolute offsets (base_offset and extent_offset) by delta.
 * <p>
 * Limitations:
 * - Supports typical iloc versions (0 and 1) and common field sizes (offset/length/base offset sizes of 0..8 bytes).
 * - Tested against files produced by Android HeifWriter and similar layouts. Edge cases may need minor tweaks.
 */
public class BinaryHeicXmlPlaceholderInjector {
    private BinaryHeicXmlPlaceholderInjector() {
    }

    /**
     * Insert an xml box (header + payload) immediately after meta header.
     * payloadSize: number of bytes in xml payload (e.g. 4096)
     */
    public static void insertXmlPlaceholder(File inHeic, File outHeic, int payloadSize) throws IOException {
        byte[] xmlPayload = new byte[payloadSize]; // zero-filled payload

        ByteArrayOutputStream xmlBox = new ByteArrayOutputStream();
        long boxSize = 8L + payloadSize;
        xmlBox.write(intToBytes(boxSize));
        xmlBox.write("xml ".getBytes(StandardCharsets.US_ASCII));
        xmlBox.write(xmlPayload);
        byte[] xmlBoxBytes = xmlBox.toByteArray();

        try (RandomAccessFile raf = new RandomAccessFile(inHeic, "r");
             FileOutputStream fos = new FileOutputStream(outHeic)) {

            long fileLen = raf.length();
            long pos = 0;
            long metaStart = -1;
            long metaSize = -1;
            int metaHeaderSize = 0;

            while (pos + 8 <= fileLen) {
                raf.seek(pos);
                long size32 = readUInt32(raf);
                String type = read4cc(raf);
                long header = 8;
                long size;
                if (size32 == 1) {
                    size = readUInt64(raf);
                    header = 16;
                } else if (size32 == 0) {
                    size = fileLen - pos;
                } else {
                    size = size32;
                }
                if ("meta".equals(type)) {
                    metaStart = pos;
                    metaSize = size;
                    metaHeaderSize = (int) header;
                    break;
                }
                pos += size;
            }

            if (metaStart < 0) throw new IOException("meta box not found");

            long insertPos = metaStart + metaHeaderSize;

            // copy [0, insertPos)
            copyRange(raf, fos, 0, insertPos);

            // write xml placeholder
            fos.write(xmlBoxBytes);

            // copy [insertPos, EOF)
            copyRange(raf, fos, insertPos, fileLen);
        }
    }

    private static void copyRange(RandomAccessFile raf, OutputStream os, long start, long end) throws IOException {
        raf.seek(start);
        long remaining = end - start;
        byte[] buf = new byte[8192];
        while (remaining > 0) {
            int toRead = (int) Math.min(buf.length, remaining);
            int r = raf.read(buf, 0, toRead);
            if (r <= 0) break;
            os.write(buf, 0, r);
            remaining -= r;
        }
    }

    private static long readUInt32(RandomAccessFile raf) throws IOException {
        byte[] b = new byte[4];
        raf.readFully(b);
        return ((b[0] & 0xFFL) << 24) | ((b[1] & 0xFFL) << 16) | ((b[2] & 0xFFL) << 8) | (b[3] & 0xFFL);
    }

    private static long readUInt64(RandomAccessFile raf) throws IOException {
        byte[] b = new byte[8];
        raf.readFully(b);
        return ((b[0] & 0xFFL) << 56) | ((b[1] & 0xFFL) << 48) | ((b[2] & 0xFFL) << 40) | ((b[3] & 0xFFL) << 32)
                | ((b[4] & 0xFFL) << 24) | ((b[5] & 0xFFL) << 16) | ((b[6] & 0xFFL) << 8) | (b[7] & 0xFFL);
    }

    private static String read4cc(RandomAccessFile raf) throws IOException {
        byte[] t = new byte[4];
        raf.readFully(t);
        return new String(t, StandardCharsets.US_ASCII);
    }

    private static byte[] intToBytes(long v) {
        return new byte[]{
                (byte) ((v >> 24) & 0xFF),
                (byte) ((v >> 16) & 0xFF),
                (byte) ((v >> 8) & 0xFF),
                (byte) (v & 0xFF)
        };
    }
}