package com.seafile.seadroid2.ui.selector.folder_selector;

import android.os.Environment;

import java.util.HashMap;
import java.util.Map;

public class Constants {

    public static String DEFAULT_ROOTPATH;
    public static String PATH_ANRROID_DATA;
    public static String PATH_ANRROID_OBB;
    public static final int SORT_NAME_ASC = 0;
    public static final int SORT_NAME_DESC = 1;
    public static final int SORT_TIME_ASC = 2;
    public static final int SORT_TIME_DESC = 3;
    public static final int SORT_SIZE_ASC = 4;
    public static final int SORT_SIZE_DESC = 5;
    public static final int TYPE_CUSTOM_VIEW_NULL = -1;
    public static Map<String, String> mimeTypeMap = null;

    static {
        DEFAULT_ROOTPATH = Environment.getExternalStorageDirectory().getAbsolutePath();
        PATH_ANRROID_DATA = DEFAULT_ROOTPATH + "/Android/data";
        PATH_ANRROID_OBB = DEFAULT_ROOTPATH + "/Android/obb";
    }

    static {
        mimeTypeMap = new HashMap<>();
        mimeTypeMap.put("apk", "application/vnd.android.package-archive");
        mimeTypeMap.put("asf", "video/x-ms-asf");
        mimeTypeMap.put("avi", "video/x-msvideo");
        mimeTypeMap.put("bin", "application/octet-stream");
        mimeTypeMap.put("bmp", "image/bmp");
        mimeTypeMap.put("c", "text/plain");
        mimeTypeMap.put("class", "application/octet-stream");
        mimeTypeMap.put("conf", "text/plain");
        mimeTypeMap.put("cpp", "text/plain");
        mimeTypeMap.put("doc", "application/msword");
        mimeTypeMap.put("docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document");
        mimeTypeMap.put("xls", "application/vnd.ms-excel");
        mimeTypeMap.put("xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        mimeTypeMap.put("exe", "application/octet-stream");
        mimeTypeMap.put("gif", "image/gif");
        mimeTypeMap.put("gtar", "application/x-gtar");
        mimeTypeMap.put("gz", "application/x-gzip");
        mimeTypeMap.put("h", "text/plain");
        mimeTypeMap.put("htm", "text/html");
        mimeTypeMap.put("html", "text/html");
        mimeTypeMap.put("jar", "application/java-archive");
        mimeTypeMap.put("java", "text/plain");
        mimeTypeMap.put("jpeg", "image/jpeg");
        mimeTypeMap.put("jpg", "image/jpeg");
        mimeTypeMap.put("js", "application/x-javascript");
        mimeTypeMap.put("log", "text/plain");
        mimeTypeMap.put("m3u", "audio/x-mpegurl");
        mimeTypeMap.put("m4a", "audio/mp4a-latm");
        mimeTypeMap.put("m4b", "audio/mp4a-latm");
        mimeTypeMap.put("m4p", "audio/mp4a-latm");
        mimeTypeMap.put("m4u", "video/vnd.mpegurl");
        mimeTypeMap.put("m4v", "video/x-m4v");
        mimeTypeMap.put("mov", "video/quicktime");
        mimeTypeMap.put("mp2", "audio/x-mpeg");
        mimeTypeMap.put("mp3", "audio/mpeg");
        mimeTypeMap.put("mp4", "video/mp4");
        mimeTypeMap.put("mpc", "application/vnd.mpohun.certificate");
        mimeTypeMap.put("mpe", "video/mpeg");
        mimeTypeMap.put("mpeg", "video/mpeg");
        mimeTypeMap.put("mpg", "video/mpeg");
        mimeTypeMap.put("mpg4", "video/mp4");
        mimeTypeMap.put("mpga", "audio/mpeg");
        mimeTypeMap.put("msg", "application/vnd.ms-outlook");
        mimeTypeMap.put("ogg", "audio/ogg");
        mimeTypeMap.put("pdf", "application/pdf");
        mimeTypeMap.put("png", "image/png");
        mimeTypeMap.put("pps", "application/vnd.ms-powerpoint");
        mimeTypeMap.put("ppt", "application/vnd.ms-powerpoint");
        mimeTypeMap.put("pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation");
        mimeTypeMap.put("prop", "text/plain");
        mimeTypeMap.put("rc", "text/plain");
        mimeTypeMap.put("rmvb", "audio/x-pn-realaudio");
        mimeTypeMap.put("rtf", "application/rtf");
        mimeTypeMap.put("sh", "text/plain");
        mimeTypeMap.put("tar", "application/x-tar");
        mimeTypeMap.put("tgz", "application/x-compressed");
        mimeTypeMap.put("txt", "text/plain");
        mimeTypeMap.put("wav", "audio/x-wav");
        mimeTypeMap.put("wma", "audio/x-ms-wma");
        mimeTypeMap.put("wmv", "audio/x-ms-wmv");
        mimeTypeMap.put("wps", "application/vnd.ms-works");
        mimeTypeMap.put("xml", "text/plain");
        mimeTypeMap.put("z", "application/x-compress");
        mimeTypeMap.put("zip", "application/x-zip-compressed");
        mimeTypeMap.put("", "*/*");
    }
}
