package com.seafile.seadroid2.framework.util;

import android.media.MediaMetadataRetriever;

import java.io.File;

public class MimeUtils {

    public static String getMimeType(File file) {
        String mime = "text/plain";
        String filePath = file.getAbsolutePath();
        MediaMetadataRetriever mmr = new MediaMetadataRetriever();
        try {
            mmr.setDataSource(filePath);
            mime = mmr.extractMetadata(MediaMetadataRetriever.METADATA_KEY_MIMETYPE);
        } catch (RuntimeException e) {
            return mime;
        }
        return mime;
    }
}
