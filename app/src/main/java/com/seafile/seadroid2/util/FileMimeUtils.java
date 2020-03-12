package com.seafile.seadroid2.util;

import android.media.MediaMetadataRetriever;

import java.io.File;
import java.util.HashMap;

/**
 * Function:
 * Author:      Saud
 * Create:      2017/7/1
 * Modtime:     2017/7/1
 */
public class FileMimeUtils {

    private static final HashMap<String, String> needUpdateFileMime = new HashMap<>();

    static {
        //{fileType，    MIME}
        needUpdateFileMime.put(".doc", "application/msword");
        needUpdateFileMime.put(".dot", "application/msword");
        needUpdateFileMime.put(".docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document");
        needUpdateFileMime.put(".dotx", "application/vnd.openxmlformats-officedocument.wordprocessingml.template");
        needUpdateFileMime.put(".docm", "application/vnd.ms-word.document.macroEnabled.12");
        needUpdateFileMime.put(".dotm", "application/vnd.ms-word.template.macroEnabled.12");
        needUpdateFileMime.put(".xls", "application/vnd.ms-excel");
        needUpdateFileMime.put(".xlt", "application/vnd.ms-excel");
        needUpdateFileMime.put(".xla", "application/vnd.ms-excel");
        needUpdateFileMime.put(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        needUpdateFileMime.put(".xltx", "application/vnd.openxmlformats-officedocument.spreadsheetml.template");
        needUpdateFileMime.put(".xlsm", "application/vnd.ms-excel.sheet.macroEnabled.12");
        needUpdateFileMime.put(".xltm", "application/vnd.ms-excel.template.macroEnabled.12");
        needUpdateFileMime.put(".xlam", "application/vnd.ms-excel.addin.macroEnabled.12");
        needUpdateFileMime.put(".xlsb", "application/vnd.ms-excel.sheet.binary.macroEnabled.12");
        needUpdateFileMime.put(".ppt", "application/vnd.ms-powerpoint");
        needUpdateFileMime.put(".pot", "application/vnd.ms-powerpoint");
        needUpdateFileMime.put(".pps", "application/vnd.ms-powerpoint");
        needUpdateFileMime.put(".ppa", "application/vnd.ms-powerpoint");
        needUpdateFileMime.put(".pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation");
        needUpdateFileMime.put(".potx", "application/vnd.openxmlformats-officedocument.presentationml.template");
        needUpdateFileMime.put(".ppsx", "application/vnd.openxmlformats-officedocument.presentationml.slideshow");
        needUpdateFileMime.put(".ppam", "application/vnd.ms-powerpoint.addin.macroEnabled.12");
        needUpdateFileMime.put(".pptm", "application/vnd.ms-powerpoint.presentation.macroEnabled.12");
        needUpdateFileMime.put(".ppsm", "application/vnd.ms-powerpoint.slideshow.macroEnabled.12");
        needUpdateFileMime.put(".wps", "application/vnd.ms-works");
    }


    public static boolean isOfficeOrTextFile(String mime) {
        return needUpdateFileMime.containsValue(mime);
    }

    public static String getMimeType(File file) {
        String mime = "text/plain";
        String filePath = file.getAbsolutePath();
        MediaMetadataRetriever mmr = new MediaMetadataRetriever();
        if (filePath != null) {
            try {
                mmr.setDataSource(filePath);
                mime = mmr.extractMetadata(MediaMetadataRetriever.METADATA_KEY_MIMETYPE);
            } catch (IllegalStateException e) {
                return mime;
            } catch (IllegalArgumentException e) {
                return mime;
            } catch (RuntimeException e) {
                return mime;
            }
        }
        return mime;
    }
}
