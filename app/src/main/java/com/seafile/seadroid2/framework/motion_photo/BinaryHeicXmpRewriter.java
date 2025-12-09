package com.seafile.seadroid2.framework.motion_photo;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.charset.StandardCharsets;

/**
 * Safe in-place XML payload replacer for HEIC files that already contain an XML box placeholder.
 *
 * Important: this class DOES NOT modify any box sizes, meta size or iloc offsets. It only
 * overwrites the payload bytes of an existing top-level meta/xml box if the new XMP fits the
 * current payload length. Use the XmlPlaceholderInjector (which adjusts meta/iloc) when you need
 * to insert a new xml box or change sizes.
 */
public final class BinaryHeicXmpRewriter {
    private BinaryHeicXmpRewriter() {}

    /**
     * Replace xml box payload in-place in a copy of inHeic -> outHeic.
     * Behavior:
     * - Finds top-level "meta" box, determines if meta content has version bytes,
     *   finds child box with type "xml " and replaces its payload region with xmpXml (UTF-8).
     * - Does not change any box size fields; new XMP MUST be <= existing xml payload length.
     * - Preserves trailing bytes by zero-padding if new XMP is shorter.
     *
     * @param inHeic input heic that already contains an xml box placeholder
     * @param xmpXml new XMP string (UTF-8)
     * @param outHeic output file (will be full copy with replaced payload)
     * @throws IOException if xml box not found or new XMP too big or IO error
     */
    public static void replaceXmlBoxIfFits(File inHeic, String xmpXml, File outHeic) throws IOException {
        if (inHeic == null || xmpXml == null || outHeic == null) throw new IllegalArgumentException("null arg");

        byte[] xmpBytes = xmpXml.getBytes(StandardCharsets.UTF_8);
        // remove UTF-8 BOM if present
        if (xmpBytes.length >= 3 && xmpBytes[0] == (byte)0xEF && xmpBytes[1] == (byte)0xBB && xmpBytes[2] == (byte)0xBF) {
            byte[] tmp = new byte[xmpBytes.length - 3];
            System.arraycopy(xmpBytes, 3, tmp, 0, tmp.length);
            xmpBytes = tmp;
        }

        // copy original file to output first (atomic approach: operate on the copy)
        copyFile(inHeic, outHeic);

        try (RandomAccessFile raf = new RandomAccessFile(outHeic, "rw")) {
            long fileLen = raf.length();
            long top = 0;
            long metaStart = -1;
            long metaBoxHeader = -1; // 8 or 16
            long metaDeclaredSize = -1;

            // locate top-level meta box
            while (top + 8 <= fileLen) {
                raf.seek(top);
                long size32 = readUInt32(raf);
                String type = read4cc(raf);
                long header = 8;
                long boxSize;
                if (size32 == 1) {
                    boxSize = readUInt64(raf);
                    header = 16;
                } else if (size32 == 0) {
                    // box extends to EOF
                    boxSize = fileLen - top;
                } else {
                    boxSize = size32;
                }

                if (boxSize <= 0 || top + boxSize > fileLen) {
                    // malformed box or truncated file
                    throw new IOException("Invalid or truncated top-level box at offset=" + top + " size32=" + size32 + " type=" + type);
                }

                if ("meta".equals(type)) {
                    metaStart = top;
                    metaBoxHeader = header;
                    metaDeclaredSize = boxSize;
                    break;
                }
                top += boxSize;
            }

            if (metaStart < 0) throw new IOException("meta box not found");

            long metaContentStart = metaStart + metaBoxHeader;
            long metaEnd = metaStart + metaDeclaredSize;

            // detect whether meta content begins with 4-byte version/flags
            // If the first child at metaContentStart is 'hdlr', then there is no version; otherwise assume version present.
            raf.seek(metaContentStart);
            byte[] probe = new byte[4];
            raf.readFully(probe);
            String probe4 = new String(probe, StandardCharsets.US_ASCII);
            boolean hasVersion = !"hdlr".equals(probe4);

            long childPtr = metaContentStart + (hasVersion ? 4 : 0);

            long xmlPayloadOffset = -1;
            long xmlPayloadLength = -1;
            long xmlBoxAbsoluteStart = -1;
            long xmlBoxHeaderSize = -1;

            // iterate children inside meta box to find "xml " box
            while (childPtr + 8 <= metaEnd) {
                raf.seek(childPtr);
                long childSize32 = readUInt32(raf);
                String childType = read4cc(raf);
                long childHeader = 8;
                long childSize;
                if (childSize32 == 1) {
                    childSize = readUInt64(raf);
                    childHeader = 16;
                } else if (childSize32 == 0) {
                    childSize = metaEnd - childPtr;
                } else {
                    childSize = childSize32;
                }

                if (childSize <= 0 || childPtr + childSize > metaEnd) break; // avoid infinite loop

                if ("xml ".equals(childType)) {
                    xmlBoxAbsoluteStart = childPtr;
                    xmlBoxHeaderSize = childHeader;
                    xmlPayloadOffset = childPtr + childHeader;
                    xmlPayloadLength = childSize - childHeader;
                    break;
                }

                childPtr += childSize;
            }

            if (xmlPayloadOffset < 0) throw new IOException("xml box not found inside meta");

            if (xmpBytes.length > xmlPayloadLength) {
                throw new IOException("new XMP size (" + xmpBytes.length + ") exceeds existing xml payload length (" + xmlPayloadLength + ")");
            }

            // safe overwrite: seek and write xmp bytes, then pad remaining payload bytes with zeros
            raf.seek(xmlPayloadOffset);
            raf.write(xmpBytes);
            long pad = xmlPayloadLength - xmpBytes.length;
            if (pad > 0) {
                byte[] zeros = new byte[8192];
                long rem = pad;
                while (rem > 0) {
                    int w = (int) Math.min(zeros.length, rem);
                    raf.write(zeros, 0, w);
                    rem -= w;
                }
            }
        }
    }

    private static void copyFile(File src, File dst) throws IOException {
        try (FileInputStream fis = new FileInputStream(src);
             FileOutputStream fos = new FileOutputStream(dst, false)) {
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
        return new String(t, java.nio.charset.StandardCharsets.US_ASCII);
    }
}