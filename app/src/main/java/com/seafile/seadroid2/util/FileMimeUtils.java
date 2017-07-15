package com.seafile.seadroid2.util;

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
        needUpdateFileMime.put(".c", "text/plain");
        needUpdateFileMime.put(".conf", "text/plain");
        needUpdateFileMime.put(".cpp", "text/plain");
        needUpdateFileMime.put(".h", "text/plain");
        needUpdateFileMime.put(".java", "text/plain");
        needUpdateFileMime.put(".log", "text/plain");
        needUpdateFileMime.put(".htm", "text/html");
        needUpdateFileMime.put(".html", "text/html");
        needUpdateFileMime.put(".prop", "text/plain");
        needUpdateFileMime.put(".rc", "text/plain");
        needUpdateFileMime.put(".sh", "text/plain");
        needUpdateFileMime.put(".txt", "text/plain");
        needUpdateFileMime.put(".md", "text/markdown");
        needUpdateFileMime.put(".markdown", "text/x-markdown");
        needUpdateFileMime.put(".md1", "text/vnd.daringfireball.markdown");
        needUpdateFileMime.put(".md2", " text/x-web-markdown");
        needUpdateFileMime.put(".wps", "application/vnd.ms-works");
    }


    private static String[][] MIME_MapTable = {
            //{last name，    MIME type}
            {".3gp", "video/3gpp"},
            {".apk", "application/vnd.android.package-archive"},
            {".asf", "video/x-ms-asf"},
            {".avi", "video/x-msvideo"},
            {".bin", "application/octet-stream"},
            {".bmp", "image/bmp"},
            {".c", "text/plain"},
            {".class", "application/octet-stream"},
            {".conf", "text/plain"},
            {".cpp", "text/plain"},
            {".doc", "application/msword"},
            {".exe", "application/octet-stream"},
            {".gif", "image/gif"},
            {".gtar", "application/x-gtar"},
            {".gz", "application/x-gzip"},
            {".h", "text/plain"},
            {".htm", "text/html"},
            {".html", "text/html"},
            {".jar", "application/java-archive"},
            {".java", "text/plain"},
            {".jpeg", "image/jpeg"},
            {".jpg", "image/jpeg"},
            {".js", "application/x-javascript"},
            {".log", "text/plain"},
            {".m3u", "audio/x-mpegurl"},
            {".m4a", "audio/mp4a-latm"},
            {".m4b", "audio/mp4a-latm"},
            {".m4p", "audio/mp4a-latm"},
            {".m4u", "video/vnd.mpegurl"},
            {".m4v", "video/x-m4v"},
            {".mov", "video/quicktime"},
            {".mp2", "audio/x-mpeg"},
            {".mp3", "audio/x-mpeg"},
            {".mp4", "video/mp4"},
            {".mpc", "application/vnd.mpohun.certificate"},
            {".mpe", "video/mpeg"},
            {".mpeg", "video/mpeg"},
            {".mpg", "video/mpeg"},
            {".mpg4", "video/mp4"},
            {".mpga", "audio/mpeg"},
            {".msg", "application/vnd.ms-outlook"},
            {".ogg", "audio/ogg"},
            {".pdf", "application/pdf"},
            {".png", "image/png"},
            {".pps", "application/vnd.ms-powerpoint"},
            {".ppt", "application/vnd.ms-powerpoint"},
            {".prop", "text/plain"},
            {".rar", "application/x-rar-compressed"},
            {".rc", "text/plain"},
            {".rmvb", "audio/x-pn-realaudio"},
            {".rtf", "application/rtf"},
            {".sh", "text/plain"},
            {".tar", "application/x-tar"},
            {".tgz", "application/x-compressed"},
            {".txt", "text/plain"},
            {".wav", "audio/x-wav"},
            {".wma", "audio/x-ms-wma"},
            {".wmv", "audio/x-ms-wmv"},
            {".wps", "application/vnd.ms-works"},
//            {".xml", "text/xml"},
            {".xml", "text/plain"},
            {".z", "application/x-compress"},
            {".zip", "application/zip"},
            {".md", "text/markdown"},
            {".markdown", "text/x-markdown"},
            {"", "*/*"}
    };

    public static String getFileMime(File file) {
        String type = "*/*";
        String fName = file.getName();
        //get  "."  index in file name
        int dotIndex = fName.lastIndexOf(".");
        if (dotIndex < 0) {
            return type;
        }
         /* get last name */
        String end = fName.substring(dotIndex, fName.length()).toLowerCase();
        if (end == "")
            return type;
        // find MIME file type 。
        for (int i = 0; i < MIME_MapTable.length; i++) {
            if (end.equals(MIME_MapTable[i][0]))
                type = MIME_MapTable[i][1];
        }
        return type;

    }


    public static boolean isOfficeOrTextFile(String mime) {
        return needUpdateFileMime.containsValue(mime);
    }


}
