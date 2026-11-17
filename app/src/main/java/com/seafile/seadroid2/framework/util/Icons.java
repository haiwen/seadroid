package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.R;

import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Objects;

public class Icons {
    private static HashMap<String, Integer> suffixIconMap = null;

    /**
     * Source code, scripts, and configuration files should all use the same filename icon (code file icon).
     */
    private static final List<String> CODE_EXTENSIONS = CollectionUtils.newArrayList(
            // JVM series
            "java", "kt", "kts", "groovy", "gradle", "scala",
            "class", "jar", "war", "dex", "smali",
            // web series
            "html", "htm", "xhtml", "vue", "svelte", "ejs", "jsx", "tsx",
            "js", "mjs", "cjs", "ts", "css", "scss", "sass", "less", "styl",
            // data config series
            "xml", "xsl", "xslt", "xsd", "dtd", "json", "jsonc", "json5",
            "yml", "yaml", "toml", "ini", "conf", "cfg", "properties", "env",
            "sql", "graphql", "gql",
            // script series
            "sh", "bash", "zsh", "fish", "bat", "cmd", "ps1", "psm1",
            "rb", "erb", "php", "phtml", "pl", "pm", "py", "pyw", "pyx",
            "lua", "r", "jl", "dart", "go", "rs", "swift",
            // C series
            "c", "h", "cc", "cpp", "cxx", "hpp", "hxx", "cs", "fs", "vb",
            "m", "mm", "asm", "s");

    public static int getFileIcon(String name) {
        String suffix = FileUtils.getFileExtension(name);
        if (TextUtils.isEmpty(suffix)) {
            return R.drawable.icon_format_file;
        }

        if (CODE_EXTENSIONS.contains(suffix)) {
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
        suffixIconMap.put("xlsx", R.drawable.icon_format_excel);
        suffixIconMap.put("mp3", R.drawable.icon_format_music);
        suffixIconMap.put("mov", R.drawable.icon_format_video);
        suffixIconMap.put("css", R.drawable.icon_format_css);
        suffixIconMap.put("draw", R.drawable.icon_format_draw);
        suffixIconMap.put("exdraw", R.drawable.icon_format_draw);
        suffixIconMap.put("pages", R.drawable.icon_format_ppt);
        suffixIconMap.put("csv", R.drawable.icon_format_excel);

        //code
        return suffixIconMap;
    }

    /**
     * mimetype:
     * <a href="https://www.iana.org/assignments/media-types/media-types.xhtml">media-types.xhtml</a>
     * <br>
     * office:
     * <a href="https://stackoverflow.com/questions/4212861/what-is-a-correct-mime-type-for-docx-pptx-etc">What is a correct MIME type for .docx, .pptx, etc.?</a>
     */
    private static int getResIdForMimetype(String mimetype) {
        if (TextUtils.isEmpty(mimetype)) {
            return R.drawable.icon_format_file;
        }

        String mime = mimetype.toLowerCase(Locale.ROOT);

        // PDF
        if (mime.contains("pdf")) {
            return R.drawable.icon_format_pdf;
        }

        // image/*, text/*, audio/*, video/*
        int slashIndex = mime.indexOf('/');
        if (slashIndex > 0) {
            String topLevelType = mime.substring(0, slashIndex);
            switch (topLevelType) {
                case "image" -> {
                    return R.drawable.icon_format_pic;
                }
                case "text" -> {
                    return R.drawable.icon_format_txt;
                }
                case "audio" -> {
                    return R.drawable.icon_format_music;
                }
                case "video" -> {
                    return R.drawable.icon_format_video;
                }
            }
        }

        // Office document suite (compatible with legacy binary and OOXML formats)
        if (mime.contains("word")) {
            return R.drawable.icon_format_word;
        } else if (mime.contains("excel") || mime.contains("spreadsheet")) {
            return R.drawable.icon_format_excel;
        } else if (mime.contains("powerpoint") || mime.contains("presentation")) {
            return R.drawable.icon_format_ppt;
        }

        // exact match for specific complete mime types
        return switch (mime) {
            case "application/zip" -> R.drawable.icon_format_zip;
            case "application/vnd.chemdraw+xml" -> R.drawable.icon_format_draw;
            default -> R.drawable.icon_format_file;
        };
    }

}
