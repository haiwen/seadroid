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
            return R.drawable.icon_extended_file;
        }

        if (codes.contains(suffix)) {
            return R.drawable.icon_extended_css;
        }

        if (getSuffixIconMap().containsKey(suffix)) {
            return Objects.requireNonNull(getSuffixIconMap().get(suffix));
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return getResIdForMimetype(mime);
    }

    public static int getFileOldIcon(String name) {

        String suffix = FileUtils.getFileExtension(name);
        if (TextUtils.isEmpty(suffix)) {
            return R.drawable.file;
        }

        if (getSuffixIconMapOld().containsKey(suffix)) {
            return getSuffixIconMapOld().get(suffix);
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return getResIdForMimetypeOld(mime);
    }

    private static synchronized HashMap<String, Integer> getSuffixIconMap() {
        if (suffixIconMap != null)
            return suffixIconMap;

        suffixIconMap = Maps.newHashMap();
        suffixIconMap.put("pdf", R.drawable.icon_extended_pdf);
        suffixIconMap.put("doc", R.drawable.icon_extended_doc);
        suffixIconMap.put("docx", R.drawable.icon_extended_doc);
        suffixIconMap.put("md", R.drawable.icon_extended_md);
        suffixIconMap.put("markdown", R.drawable.icon_extended_md);
        suffixIconMap.put("txt", R.drawable.icon_extended_txt);
        suffixIconMap.put("png", R.drawable.icon_extended_png);
        suffixIconMap.put("gif", R.drawable.icon_extended_png);
        suffixIconMap.put("psd", R.drawable.icon_extended_psd);
        suffixIconMap.put("ppt", R.drawable.icon_extended_ppt);
        suffixIconMap.put("sdoc", R.drawable.icon_extended_sdoc);
        suffixIconMap.put("xls", R.drawable.icon_extended_xls);
        suffixIconMap.put("mp3", R.drawable.icon_extended_mp3);
        suffixIconMap.put("mov", R.drawable.icon_extended_mov);
        suffixIconMap.put("css", R.drawable.icon_extended_css);

        //code
        return suffixIconMap;
    }


    private static synchronized HashMap<String, Integer> getSuffixIconMapOld() {
        if (suffixIconMap != null)
            return suffixIconMap;

        suffixIconMap = Maps.newHashMap();
        suffixIconMap.put("pdf", R.drawable.file_pdf);
        suffixIconMap.put("doc", R.drawable.file_ms_word);
        suffixIconMap.put("docx", R.drawable.file_ms_word);
        suffixIconMap.put("md", R.drawable.file_text);
        suffixIconMap.put("markdown", R.drawable.file_text);
        return suffixIconMap;
    }

    /**
     * https://stackoverflow.com/questions/4212861/what-is-a-correct-mime-type-for-docx-pptx-etc
     */
    private static int getResIdForMimetype(String mimetype) {
        if (TextUtils.isEmpty(mimetype)) {
            return R.drawable.icon_extended_file;
        }

        if (mimetype.contains("pdf")) {
            return R.drawable.icon_extended_pdf;
        } else if (mimetype.contains("image/")) {
            return R.drawable.icon_extended_png;
        } else if (mimetype.contains("text/")) {
            return R.drawable.icon_extended_txt;
        } else if (mimetype.contains("audio/")) {
            return R.drawable.icon_extended_mp3;
        } else if (mimetype.contains("video/")) {
            return R.drawable.icon_extended_mov;
        } else if (mimetype.contains("msword") || mimetype.contains("ms-word")) {
            return R.drawable.icon_extended_doc;
        } else if (mimetype.contains("mspowerpoint") || mimetype.contains("ms-powerpoint")) {
            return R.drawable.icon_extended_ppt;
        } else if (mimetype.contains("msexcel") || mimetype.contains("ms-excel")) {
            return R.drawable.icon_extended_xls;
        } else if (mimetype.contains("openxmlformats-officedocument")) {
            if (mimetype.contains("wordprocessingml")) {
                return R.drawable.icon_extended_doc;
            } else if (mimetype.contains("spreadsheetml")) {
                return R.drawable.icon_extended_xls;
            } else if (mimetype.contains("presentationml")) {
                return R.drawable.icon_extended_ppt;
            }
        } else if (mimetype.equals("application/vnd.android.package-archive")) {
            return R.drawable.icon_extended_file;
        } else if (mimetype.equals("application/zip")) {
            return R.drawable.icon_extended_zip;
        } else if (mimetype.equals("application/x-iwork-numbers-sffnumbers")) {
            return R.drawable.icon_extended_file;
        } else if (mimetype.equals("application/x-iwork-pages-sffpages")) {
            return R.drawable.icon_extended_file;
        }

        return R.drawable.icon_extended_file;
    }

    public static int getResIdForMimetypeOld(String mimetype) {
        if (mimetype == null)
            return R.drawable.file;

        if (mimetype.contains("image/")) {
            return R.drawable.file_image;
        } else if (mimetype.contains("text")) {
            return R.drawable.file_text;
        } else if (mimetype.contains("audio")) {
            return R.drawable.file_audio;
        } else if (mimetype.contains("video")) {
            return R.drawable.file_video;
        } else if (mimetype.contains("pdf")) {
            return R.drawable.file_pdf;
        } else if (mimetype.contains("msword") || mimetype.contains("ms-word")) {
            return R.drawable.file_ms_word;
        } else if (mimetype.contains("mspowerpoint") || mimetype.contains("ms-powerpoint")) {
            return R.drawable.file_ms_ppt;
        } else if (mimetype.contains("msexcel") || mimetype.contains("ms-excel")) {
            return R.drawable.file_ms_excel;
        } else if (mimetype.contains("openxmlformats-officedocument")) {
            if (mimetype.contains("wordprocessingml")) {
                return R.drawable.file_ms_word;
            } else if (mimetype.contains("spreadsheetml")) {
                return R.drawable.file_ms_excel;
            } else if (mimetype.contains("presentationml")) {
                return R.drawable.file_ms_ppt;
            }
            // } else if (mimetype.contains("application")) {
            //     return R.drawable.file_binary;
        }

        return R.drawable.file;
    }

}
