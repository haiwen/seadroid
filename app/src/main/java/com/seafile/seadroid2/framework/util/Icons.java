package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;

import java.util.HashMap;
import java.util.List;
import java.util.Objects;

public class Icons {
    private static HashMap<String, Integer> suffixIconMap = null;

    private static final List<String> codes = CollectionUtils.newArrayList(
            "java", "class", "jar", "html", "htm", "xml", "json", "js", "css",
            "scss", "sass", "less", "ts", "rb", "php", "sh", "yml", "yaml",
            "ini", "conf", "cfg", "sql", "properties", "toml", "c",
            "cs", "swift", "go", "dart", "sh", "bat", "h", "vue", "ejs", "jsx", "kt", "py", "lua",
            "py", "cpp", "h");

    public static int getFileIcon(String name) {
//        return getFileOldIcon(name);
        String suffix = FileUtils.getFileExtension(name);
        if (TextUtils.isEmpty(suffix)) {
            return R.drawable.icon_format_file;
        }

        if (codes.contains(suffix)) {
            return R.drawable.icon_format_css;
        }

        if (getSuffixIconMap().containsKey(suffix)) {
            return Objects.requireNonNull(getSuffixIconMap().get(suffix));
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return getResIdForMimetype(mime);
    }


    private static synchronized HashMap<String, Integer> getSuffixIconMap() {
        if (suffixIconMap != null)
            return suffixIconMap;

        suffixIconMap = Maps.newHashMap();
        suffixIconMap.put("pdf", R.drawable.icon_format_pdf);
        suffixIconMap.put("doc", R.drawable.icon_format_word);
        suffixIconMap.put("docx", R.drawable.icon_format_word);
        suffixIconMap.put("md", R.drawable.icon_format_md);
        suffixIconMap.put("markdown", R.drawable.icon_format_md);
        suffixIconMap.put("txt", R.drawable.icon_format_txt);
        suffixIconMap.put("png", R.drawable.icon_format_pic);
        suffixIconMap.put("gif", R.drawable.icon_format_pic);
        suffixIconMap.put("psd", R.drawable.icon_format_psd);
        suffixIconMap.put("ppt", R.drawable.icon_format_ppt);
        suffixIconMap.put("sdoc", R.drawable.icon_format_sdoc);
        suffixIconMap.put("xls", R.drawable.icon_format_excel);
        suffixIconMap.put("mp3", R.drawable.icon_format_music);
        suffixIconMap.put("mov", R.drawable.icon_format_video);
        suffixIconMap.put("css", R.drawable.icon_format_css);

        //code
        return suffixIconMap;
    }

    /**
     * https://stackoverflow.com/questions/4212861/what-is-a-correct-mime-type-for-docx-pptx-etc
     */
    private static int getResIdForMimetype(String mimetype) {
        if (TextUtils.isEmpty(mimetype)) {
            return R.drawable.icon_format_file;
        }

        if (mimetype.contains("pdf")) {
            return R.drawable.icon_format_pdf;
        } else if (mimetype.contains("image/")) {
            return R.drawable.icon_format_pic;
        } else if (mimetype.contains("text/")) {
            return R.drawable.icon_format_txt;
        } else if (mimetype.contains("audio/")) {
            return R.drawable.icon_format_music;
        } else if (mimetype.contains("video/")) {
            return R.drawable.icon_format_video;
        } else if (mimetype.contains("msword") || mimetype.contains("ms-word")) {
            return R.drawable.icon_format_word;
        } else if (mimetype.contains("mspowerpoint") || mimetype.contains("ms-powerpoint")) {
            return R.drawable.icon_format_ppt;
        } else if (mimetype.contains("msexcel") || mimetype.contains("ms-excel")) {
            return R.drawable.icon_format_excel;
        } else if (mimetype.contains("openxmlformats-officedocument")) {
            if (mimetype.contains("wordprocessingml")) {
                return R.drawable.icon_format_word;
            } else if (mimetype.contains("spreadsheetml")) {
                return R.drawable.icon_format_excel;
            } else if (mimetype.contains("presentationml")) {
                return R.drawable.icon_format_ppt;
            }
        } else if (mimetype.equals("application/vnd.android.package-archive")) {
            return R.drawable.icon_format_file;
        } else if (mimetype.equals("application/zip")) {
            return R.drawable.icon_format_zip;
        } else if (mimetype.equals("application/x-iwork-numbers-sffnumbers")) {
            return R.drawable.icon_format_file;
        } else if (mimetype.equals("application/x-iwork-pages-sffpages")) {
            return R.drawable.icon_format_file;
        }

        return R.drawable.icon_format_file;
    }
}
