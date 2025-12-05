package com.seafile.seadroid2.framework.motion_photo;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.charset.StandardCharsets;

/**
 * BinaryHeicXmpRewriter
 *
 * Purpose:
 * - Safely replace the contents of an existing "xml " box inside the top-level meta box of a HEIC file
 *   without changing any box sizes or offsets.
 * - This avoids touching iloc/offsets and therefore prevents breaking HeifWriter-produced HEIC files.
 *
 * Behavior:
 * 1) Locate top-level "meta" box (supports 32-bit and 64-bit sizes).
 * 2) Detect whether meta uses QuickTime-style formatting (per isoparser MetaBox.parse logic).
 * 3) Find an existing child box with type "xml ". If not found, the method throws an exception.
 * 4) If the new XMP byte length <= existing xml box payload length, the method copies the file to `out`
 *    and overwrites the xml payload in-place (padding with zero bytes if the new payload is shorter).
 * 5) If the new XMP is larger than the existing payload, the method throws an IOException —
 *    this avoids changing meta size and therefore avoids corrupting offsets.
 *
 * Limitations / Rationale:
 * - This class intentionally does NOT attempt to expand or re-layout the meta box or adjust iloc offsets.
 *   Expanding meta requires a full re-write of offsets and risks breaking tile/grid HEICs produced by HeifWriter.
 * - Use this when you can ensure the generated XMP fits into the existing xml box space (or you are willing to
 *   trim/publish a compacted XMP).
 */
public final class BinaryHeicXmpRewriter {
    private BinaryHeicXmpRewriter() {}

    public static void replaceXmlBoxIfFits(File inHeic, String xmpXml, File outHeic) throws IOException {
        byte[] xmpBytes = xmpXml.getBytes(StandardCharsets.UTF_8);
        // Remove BOM if present
        if (xmpBytes.length >= 3 && xmpBytes[0] == (byte)0xEF && xmpBytes[1] == (byte)0xBB && xmpBytes[2] == (byte)0xBF) {
            byte[] tmp = new byte[xmpBytes.length - 3];
            System.arraycopy(xmpBytes, 3, tmp, 0, tmp.length);
            xmpBytes = tmp;
        }

        try (RandomAccessFile raf = new RandomAccessFile(inHeic, "r")) {
            long fileLen = raf.length();
            long top = 0;
            long metaBoxStart = -1;
            long metaBoxSize = -1;

            // iterate top-level boxes
            while (top + 8 <= fileLen) {
                raf.seek(top);
                long size32 = readUInt32(raf);
                String type = read4cc(raf);
                long headerSize = 8;
                long boxSize;
                if (size32 == 1) {
                    // largesize
                    long largesize = readUInt64(raf);
                    boxSize = largesize;
                    headerSize = 16;
                } else if (size32 == 0) {
                    boxSize = fileLen - top; // to end of file
                } else {
                    boxSize = size32;
                }

                if ("meta".equals(type)) {
                    metaBoxStart = top;
                    metaBoxSize = boxSize;
                    break;
                }

                if (boxSize <= 0) break;
                top += boxSize;
            }

            if (metaBoxStart < 0) {
                throw new IOException("meta box not found");
            }

            long metaHeaderSize = computeHeaderSizeAt(raf, metaBoxStart);
            long metaContentStart = metaBoxStart + metaHeaderSize;

            // Determine quickTimeFormat by reading 20 bytes starting at metaContentStart
            boolean quickTimeFormat = false;
            if (metaContentStart + 20 <= fileLen) {
                raf.seek(metaContentStart);
                byte[] sample = new byte[20];
                raf.readFully(sample);
                String second4cc = new String(sample, 4, 4, StandardCharsets.US_ASCII);
                String fifth4cc = new String(sample, 16, 4, StandardCharsets.US_ASCII);
                if ("hdlr".equals(second4cc) && "mdta".equals(fifth4cc)) {
                    quickTimeFormat = true;
                }
            }

            long childStart = metaContentStart + (quickTimeFormat ? 0 : 4); // skip version/flags when not quickTimeFormat
            long childPtr = childStart;
            long metaEnd = metaBoxStart + metaBoxSize;

            long xmlBoxPayloadOffset = -1;
            long xmlBoxPayloadLength = -1;
            long xmlBoxHeaderSize = -1;
            long xmlBoxTotalSize = -1;

            // iterate child boxes inside meta
            while (childPtr + 8 <= metaEnd) {
                raf.seek(childPtr);
                long childSize32 = readUInt32(raf);
                String childType = read4cc(raf);
                long childHeaderSize = 8;
                long childSize;
                if (childSize32 == 1) {
                    long largesize = readUInt64(raf);
                    childSize = largesize;
                    childHeaderSize = 16;
                } else if (childSize32 == 0) {
                    childSize = metaEnd - childPtr;
                } else {
                    childSize = childSize32;
                }

                if (childSize <= 0) break;

                if ("xml ".equals(childType)) {
                    xmlBoxHeaderSize = childHeaderSize;
                    xmlBoxTotalSize = childSize;
                    xmlBoxPayloadOffset = childPtr + childHeaderSize; // payload starts immediately after header
                    xmlBoxPayloadLength = childSize - childHeaderSize;
                    break;
                }

                childPtr += childSize;
            }

            if (xmlBoxPayloadOffset < 0) {
                throw new IOException("xml box not found inside meta");
            }

            if (xmpBytes.length > xmlBoxPayloadLength) {
                throw new IOException("new XMP size (" + xmpBytes.length + " bytes) exceeds existing xml payload length (" + xmlBoxPayloadLength + " bytes)." +
                        " This tool does not expand meta; produce a smaller XMP or use a full rewriter that rebuilds boxes.");
            }

            // safe to write: copy whole file to out then overwrite payload
            copyFile(inHeic, outHeic);

            try (RandomAccessFile rafOut = new RandomAccessFile(outHeic, "rw")) {
                rafOut.seek(xmlBoxPayloadOffset);
                rafOut.write(xmpBytes);
                long pad = xmlBoxPayloadLength - xmpBytes.length;
                if (pad > 0) {
                    // pad with zero bytes
                    byte[] zeros = new byte[(int) Math.min(pad, 8192)];
                    long remaining = pad;
                    while (remaining > 0) {
                        int w = (int) Math.min(zeros.length, remaining);
                        rafOut.write(zeros, 0, w);
                        remaining -= w;
                    }
                }
            }
        }
    }

    private static void copyFile(File src, File dst) throws IOException {
        try (FileInputStream fis = new FileInputStream(src); FileOutputStream fos = new FileOutputStream(dst, false)) {
            byte[] buf = new byte[8192];
            int r;
            while ((r = fis.read(buf)) != -1) fos.write(buf, 0, r);
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

    private static long computeHeaderSizeAt(RandomAccessFile raf, long boxStart) throws IOException {
        raf.seek(boxStart);
        long size32 = readUInt32(raf);
        // skip type
        raf.skipBytes(4);
        if (size32 == 1) {
            return 16;
        }
        return 8;
    }
}
