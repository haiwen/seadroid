package com.seafile.seadroid2.framework.motion_photo;

import java.nio.ByteBuffer;

public final class MpvdBox {
    private MpvdBox(){}

    /**
     * Build mpvd top-level box: [size(4)] ['mpvd'] [data...]
     */
    public static byte[] buildMpvdBox(byte[] mp4Bytes) {
        long total = 8L + mp4Bytes.length;
        if (total > 0xFFFFFFFFL) {
            // If too big, will need largesize box. For simplicity throw.
            throw new IllegalArgumentException("mp4 too large for 32-bit size mpvd box");
        }
        ByteBuffer bb = ByteBuffer.allocate((int)total);
        bb.putInt((int)total);
        bb.put("mpvd".getBytes(java.nio.charset.StandardCharsets.US_ASCII));
        bb.put(mp4Bytes);
        return bb.array();
    }
}