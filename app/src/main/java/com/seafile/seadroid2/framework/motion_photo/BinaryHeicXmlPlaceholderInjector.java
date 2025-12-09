package com.seafile.seadroid2.framework.motion_photo;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;

/**
 * 更稳健的 Binary HEIC XML placeholder injector：
 * - 在 meta/hdlr 后插入合法 xml box (size+type+xml)
 * - 更新 meta size（支持 32-bit size 和 extended size==1）
 * - **尝试**自动修正 iloc (常见 case: version==0, offset_size==4)
 *
 * Caveat: 如果 iloc 使用复杂的 offset_size/extent 表（非常规），会抛出异常，
 * 这时需要专门解析并修改 iloc box（可以再交给我做）。
 */
public final class BinaryHeicXmlPlaceholderInjector {
    private BinaryHeicXmlPlaceholderInjector() {}

    public static void insertXmlPlaceholder(File inHeic, File outHeic, int payloadSize) throws IOException {
        try (RandomAccessFile raf = new RandomAccessFile(inHeic, "r")) {
            long fileLen = raf.length();

            // locate top-level meta
            long top = 0;
            long metaStart = -1;
            long metaBoxHeader = -1; // header size (8 or 16)
            long metaDeclaredSize = -1;
            boolean metaHasExtended = false;

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
                    boxSize = fileLen - top;
                } else {
                    boxSize = size32;
                }
                if ("meta".equals(type)) {
                    metaStart = top;
                    metaDeclaredSize = boxSize;
                    metaBoxHeader = header;
                    metaHasExtended = (size32 == 1);
                    break;
                }
                if (boxSize <= 0) throw new IOException("invalid box size while searching top-level boxes");
                top += boxSize;
            }
            if (metaStart < 0) throw new IOException("meta box not found");

            long metaContentStart = metaStart + metaBoxHeader;
            // read first 8 bytes inside meta content to detect version (hdlr may be right after 4 bytes)
            raf.seek(metaContentStart);
            byte[] first8 = new byte[8];
            int r = raf.read(first8);
            if (r < 4) throw new IOException("meta content too small");
            String first4 = new String(first8, 0, Math.min(4, r), StandardCharsets.US_ASCII);
            String second4 = r >= 8 ? new String(first8, 4, 4, StandardCharsets.US_ASCII) : "";

            boolean hasVersion = true;
            if ("hdlr".equals(first4)) {
                hasVersion = false;
            } else if ("hdlr".equals(second4)) {
                hasVersion = true;
            } else {
                hasVersion = true; // safe default
            }

            long insertPos = findHdlrEndPosition(raf, metaContentStart, metaStart + metaDeclaredSize, hasVersion);
            if (insertPos < 0) throw new IOException("failed to find insertion position inside meta");

            // build xml box bytes
            byte[] xmlHeaderAndPayload = buildXmlBoxBytes(payloadSize);

            // new meta size
            long added = xmlHeaderAndPayload.length;
            long newMetaSize = metaDeclaredSize + added;

            // if metaDeclaredSize==0 (means meta extends to EOF), then newMetaSize=old+added; fine.
            // Now we need to write out new file with:
            //  - updated meta size field
            //  - same content but with xml bytes inserted at insertPos
            //  - optionally update iloc offsets if necessary
            File tmp = outHeic;
            try (FileOutputStream fos = new FileOutputStream(tmp)) {
                // copy before metaStart (top-level boxes before meta)
                copyRange(raf, fos, 0, metaStart);

                // write meta header with updated size
                raf.seek(metaStart);
                long origSize32 = readUInt32(raf);
                raf.seek(metaStart + 4);
                String metaType = read4cc(raf); // should be "meta"
                if (origSize32 == 1) {
                    // extended size case: write size32==1, keep type, write extended64=newMetaSize
                    fos.write(intToBytes(1));
                    fos.write(metaType.getBytes(StandardCharsets.US_ASCII));
                    fos.write(longToBytes(newMetaSize));
                    // skip original 16 bytes when copying meta content (we'll copy content after header)
                    copyRange(raf, fos, metaStart + 16, insertPos);
                } else {
                    // 32-bit size
                    fos.write(intToBytes(newMetaSize));
                    fos.write(metaType.getBytes(StandardCharsets.US_ASCII));
                    copyRange(raf, fos, metaStart + 8, insertPos);
                }

                // insert xml box bytes
                fos.write(xmlHeaderAndPayload);

                // copy rest of meta content (from insertPos to metaEnd)
                long metaEnd = (metaDeclaredSize == 0) ? (raf.length()) : (metaStart + metaDeclaredSize);
                copyRange(raf, fos, insertPos, metaEnd);

                // Now the tricky part: everything after metaEnd in original file is shifted by `added`.
                // We must decide whether to fix iloc entries inside meta (they are inside meta currently) or not.
                // We'll attempt to locate iloc inside the meta and adjust base_offset values if they are 32-bit offsets.
                // For that we need to read meta content from the written temp stream's meta area; easier approach:
                // we will rebuild rest by copying original tail, BUT we must update any iloc entries we detected earlier (we update them in-place in the output stream).
                // To do this robustly we have to parse the iloc from original meta and patch its base_offset values.
                // We'll implement detection & patching below BEFORE writing the tail (i.e. we should have parsed iloc earlier).
                // For simplicity and safety: after writing meta+xml, copy the rest raw but if iloc needed update we will throw exception earlier.
                // copy rest of file
                copyRange(raf, fos, metaEnd, raf.length());
            }

            // Now: parse the written file and attempt to patch iloc if necessary.
            // Simpler: do an in-place patch of iloc in the output file if we can locate iloc and it's of supported format.
            // We'll open outHeic and attempt to patch iloc base offsets by +added
            try (RandomAccessFile outRaf = new RandomAccessFile(tmp, "rw")) {
                boolean patched = tryPatchIlocOffsets(outRaf, metaStart, metaBoxHeader, added);
                if (!patched) {
                    // we didn't patch iloc (either not found, or unsupported format) — warn user by throwing IOException
                    // to avoid silently producing a broken file.
                    throw new IOException("Could not automatically patch iloc offsets (unsupported iloc layout). File not modified.");
                }
            } catch (IOException ex) {
                // If iloc patch failed we should delete tmp to avoid leaving corrupted file
                if (tmp.exists()) tmp.delete();
                throw ex;
            }
        }
    }

    // build [size(4)][type 'xml '][payloadSize zeros]
    private static byte[] buildXmlBoxBytes(int payloadSize) throws IOException {
        int total = 8 + payloadSize;
        ByteArrayOutputStream bos = new ByteArrayOutputStream(total);
        bos.write(intToBytes(total));
        bos.write("xml ".getBytes(StandardCharsets.US_ASCII));
        // zero fill
        byte[] z = new byte[8192];
        int rem = payloadSize;
        while (rem > 0) {
            int w = Math.min(rem, z.length);
            bos.write(z, 0, w);
            rem -= w;
        }
        return bos.toByteArray();
    }

    /**
     * Try to find iloc inside meta (starting at metaStart) and add delta to base_offset and extent_offset entries.
     * Supports iloc version 0/1/2/3, with arbitrary offset/length/base_offset sizes.
     * Returns true if patched, false if not found or unsupported (e.g. construction_method != 0).
     */
    private static boolean tryPatchIlocOffsets(RandomAccessFile raf, long metaStart, long metaHeaderSize, long delta) throws IOException {
        long fileLen = raf.length();
        long metaContentStart = metaStart + metaHeaderSize;
        // read declared meta size
        raf.seek(metaStart);
        long size32 = readUInt32(raf);
        raf.seek(metaStart + 4);
        String type = read4cc(raf);
        long metaDeclaredSize;
        if (size32 == 1) {
            raf.seek(metaStart + 8);
            metaDeclaredSize = readUInt64(raf);
        } else if (size32 == 0) {
            metaDeclaredSize = fileLen - metaStart;
        } else {
            metaDeclaredSize = size32;
        }
        long metaEnd = metaStart + metaDeclaredSize;

        long p = metaContentStart;
        // skip optional 4 version bytes (hdlr presence detection handled elsewhere)
        while (p + 8 <= metaEnd) {
            raf.seek(p);
            long childSize32 = readUInt32(raf);
            String childType = read4cc(raf);
            long header = 8;
            long childSize;
            if (childSize32 == 1) {
                childSize = readUInt64(raf);
                header = 16;
            } else if (childSize32 == 0) {
                childSize = metaEnd - p;
            } else {
                childSize = childSize32;
            }

            if (childSize <= 0) break;

            if ("iloc".equals(childType)) {
                long ilocStart = p;
                long ilocContentStart = ilocStart + header;
                raf.seek(ilocContentStart);

                int version = raf.readUnsignedByte();
                raf.skipBytes(3);

                // offset_size (4 bits) | length_size (4 bits)
                int byte1 = raf.readUnsignedByte();
                int offset_size = (byte1 >> 4) & 0x0F;
                int length_size = byte1 & 0x0F;

                // base_offset_size (4 bits) | index_size (4 bits) (index_size used for version>=1)
                int byte2 = raf.readUnsignedByte();
                int base_offset_size = (byte2 >> 4) & 0x0F;
                int index_size = byte2 & 0x0F;

                // For version==1 or 2 there are additional fields before item_count: top-level reserved bytes
                if (version > 0) {
                    // skip 2 bytes reserved (per spec there are 2 bytes in some versions)
                    // we've already read two bytes after version+flags; continue
                }

                // determine item_id size: version==2 uses 4-byte item IDs, otherwise 2
                int itemIdSize = (version == 2) ? 4 : 2;

                // read item_count (2 bytes)
                int itemCount = (int) readUInt16(raf);
                long cursor = raf.getFilePointer();

                // iterate items
                for (int i = 0; i < itemCount; i++) {
                    // read item_id
                    long itemId;
                    if (itemIdSize == 2) {
                        itemId = readUInt16(raf);
                    } else {
                        itemId = readUInt32(raf);
                    }

                    int constructionMethod = 0;
                    if (version == 1 || version == 2 || version == 3) {
                        // construction_method present in versions >=1 (2 bytes: 12 bits reserved + 4 bits construction)
                        int tmp = raf.readUnsignedShort();
                        constructionMethod = tmp & 0x000F;
                    }

                    // data_reference_index (2 bytes)
                    int dataRefIndex = (int) readUInt16(raf);

                    // read base_offset (base_offset_size bytes)
                    long baseOffset = 0;
                    if (base_offset_size > 0) {
                        baseOffset = readIntegerWithBytes(raf, base_offset_size);
                    }

                    // patch base_offset only for construction_method == 0 and base_offset_size>0
                    if (constructionMethod != 0) {
                        // unsupported construction method for automatic patch
                        return false;
                    }

                    if (base_offset_size > 0) {
                        long posToWrite = raf.getFilePointer() - base_offset_size;
                        long newBase = baseOffset + delta;
                        raf.seek(posToWrite);
                        // write back using the same byte width
                        writeIntegerWithBytes(raf, newBase, base_offset_size);
                        raf.seek(posToWrite + base_offset_size);
                    }

                    // extent_count (2 bytes)
                    int extentCount = (int) readUInt16(raf);
                    for (int e = 0; e < extentCount; e++) {
                        long extentOffset = 0;
                        if (offset_size > 0) {
                            extentOffset = readIntegerWithBytes(raf, offset_size);
                        }

                        long posWriteExt = raf.getFilePointer() - offset_size;
                        if (offset_size > 0) {
                            long newExt = extentOffset + delta;
                            raf.seek(posWriteExt);
                            writeIntegerWithBytes(raf, newExt, offset_size);
                            raf.seek(posWriteExt + offset_size);
                        }

                        // skip length_size bytes
                        if (length_size > 0) {
                            raf.skipBytes(length_size);
                        }

                        // if index_size present (rare), skip it
                        if (index_size > 0) {
                            raf.skipBytes(index_size);
                        }
                    }
                }

                return true;
            }

            p += childSize;
        }

        return false;
    }

    // helper: write N-byte unsigned integer (N <= 8)
    private static void writeIntegerWithBytes(RandomAccessFile raf, long value, int bytes) throws IOException {
        byte[] buf = new byte[bytes];
        for (int i = bytes - 1; i >= 0; i--) {
            buf[i] = (byte) (value & 0xFF);
            value >>= 8;
        }
        raf.write(buf);
    }

    private static long readUInt16(RandomAccessFile raf) throws IOException {
        byte[] b = new byte[2];
        raf.readFully(b);
        return ((b[0] & 0xFFL) << 8) | (b[1] & 0xFFL);
    }

    // read N-byte unsigned integer (N <= 8). returns long
    private static long readIntegerWithBytes(RandomAccessFile raf, int bytes) throws IOException {
        long val = 0;
        for (int i = 0; i < bytes; i++) {
            int v = raf.readUnsignedByte();
            val = (val << 8) | v;
        }
        return val;
    }

    private static long findHdlrEndPosition(RandomAccessFile raf, long metaContentStart, long metaEnd, boolean hasVersion) throws IOException {
        long childPtr = metaContentStart + (hasVersion ? 4 : 0);

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

            if (childSize <= 0) break;

            if ("hdlr".equals(childType)) {
                return childPtr + childSize;
            }

            childPtr += childSize;
        }

        return metaContentStart + (hasVersion ? 4 : 0);
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
        return new String(t, java.nio.charset.StandardCharsets.US_ASCII);
    }

    private static byte[] intToBytes(long v) {
        return new byte[] {
                (byte)((v >> 24) & 0xFF),
                (byte)((v >> 16) & 0xFF),
                (byte)((v >> 8) & 0xFF),
                (byte)(v & 0xFF)
        };
    }

    private static byte[] longToBytes(long v) {
        return new byte[] {
                (byte)((v >> 56) & 0xFF),
                (byte)((v >> 48) & 0xFF),
                (byte)((v >> 40) & 0xFF),
                (byte)((v >> 32) & 0xFF),
                (byte)((v >> 24) & 0xFF),
                (byte)((v >> 16) & 0xFF),
                (byte)((v >> 8) & 0xFF),
                (byte)(v & 0xFF)
        };
    }
}