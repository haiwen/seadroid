package com.seafile.seadroid2.framework.util;

import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.net.http.SslCertificate;
import android.os.Bundle;
import android.provider.OpenableColumns;
import android.text.TextUtils;
import android.util.Log;
import android.util.Pair;
import android.view.View;
import android.webkit.MimeTypeMap;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.motionphoto.MotionPhotoDescriptor;
import com.seafile.seadroid2.framework.motionphoto.MotionPhotoDetector;
import com.seafile.seadroid2.context.GlobalNavContext;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.net.MalformedURLException;
import java.net.URL;
import java.security.cert.Certificate;
import java.security.cert.CertificateException;
import java.security.cert.CertificateFactory;
import java.security.cert.X509Certificate;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Locale;
import java.util.Set;

public class Utils {
    public static final String MIME_APPLICATION_OCTET_STREAM = "application/octet-stream";
    public static final String AUTHORITY = BuildConfig.APPLICATION_ID;

    // public static final String NOGROUP = "$nogroup";
    public static final String PERSONAL_REPO = "personal_repo";
    public static final String SHARED_REPO = "shared_repo";
    public static final String TRANSFER_PHOTO_TAG = "camera_upload";
    public static final String TRANSFER_FOLDER_TAG = "folder_backup";
    private static final String DEBUG_TAG = "Utils";
    private static final String HIDDEN_PREFIX = ".";
    private static HashMap<String, Integer> suffixIconMap = null;

    private Utils() {
    }

    public static boolean isSDoc(String fileName) {
        if (TextUtils.isEmpty(fileName)) {
            return false;
        }
        String suffix = FilenameUtils.getExtension(fileName);
        if (TextUtils.isEmpty(suffix)) {
            return false;
        }

        return StringUtils.equals("sdoc", suffix.toLowerCase());
    }

    public static String getVendorNormalized() {
        String raw = android.os.Build.MANUFACTURER;
        if (raw == null) return "";
        String v = raw.trim().toLowerCase(Locale.ROOT);
        return switch (v) {
            case "oppo", "oplus" -> "oppo";
            case "huawei" -> "huawei";
            case "xiaomi" -> "xiaomi";
            case "vivo" -> "vivo";
            case "samsung" -> "samsung";
            default -> v;
        };
    }

    /**
     * <pre>
     * /a/b/c.txt -> /a/b/c/
     * /a/b/c     -> /a/b/
     * /a/b/c/    -> /a/b/c/
     * </pre>
     *
     */
    public static String getFullPath(String path) {
        if (path == null) {
            // the caller should not give null
            Log.w(DEBUG_TAG, "path is null");
            return null;
        }

        if (!path.contains("/")) {
            return "/";
        }

        if (path.endsWith("/")) {
            return path;
        }

        String parent = FilenameUtils.getFullPath(path);
        if (parent.isEmpty()) {
            return "/";
        } else
            return parent;
    }

    public static String getParentPath(String path) {
        if (path == null) {
            // the caller should not give null
            Log.w(DEBUG_TAG, "path is null");
            return null;
        }
        String s = File.separator;
        if (!path.contains(s)) {
            return "/";
        }

        if (s.equals(path)) {
            return "/";
        }

        if (path.endsWith(s)) {
            path = path.substring(0, path.lastIndexOf(s));
        }

        String parent = path.substring(0, path.lastIndexOf(s));
        if (parent.isEmpty()) {
            return "/";
        } else
            return parent;
    }

    public static String getFileNameFromPath(String path) {
        if (path == null) {
            // the caller should not give null
            Log.w(DEBUG_TAG, "null in getParentPath");
            return null;
        }

        return FilenameUtils.getName(path);
    }

    public static final String[] _units = new String[]{"B", "KB", "MB", "GB", "TB"};
    public static final DecimalFormat _decimalFormat = new DecimalFormat("#,##0.#");


    public static String readableFileSize(long size) {
        if (size <= 0) return "0 KB";

        int digitGroups = (int) (Math.log10(size) / Math.log10(1000));
        int maxUnitIndex = _units.length - 1;

        if (digitGroups > maxUnitIndex) {
            // When the unit range is exceeded, only the largest unit (such as TB) is displayed,
            // but the value continues to grow
            double adjustedSize = size / Math.pow(1000, maxUnitIndex);
            return _decimalFormat.format(adjustedSize) + " " + _units[maxUnitIndex];
        }

        return _decimalFormat.format(size / Math.pow(1000, digitGroups)) + " " + _units[digitGroups];
    }

    public static boolean isJpeg(String name) {
        String suffix = FilenameUtils.getExtension(name);
        if (TextUtils.isEmpty(suffix)) {
            return false;
        }
        suffix = suffix.toLowerCase(Locale.ROOT);
        if (TextUtils.equals("jpg", suffix) || TextUtils.equals("jpeg", suffix)) {
            return true;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return TextUtils.equals(mime, "image/jpeg");
    }

    public static boolean isHeic(String name) {
        String suffix = FilenameUtils.getExtension(name);
        if (TextUtils.isEmpty(suffix)) {
            return false;
        }
        suffix = suffix.toLowerCase(Locale.ROOT);
        if (TextUtils.equals("heic", suffix) || TextUtils.equals("heif", suffix)) {
            return true;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return TextUtils.equals(mime, "image/heic")
                || TextUtils.equals(mime, "image/heic-sequence");
        // not support heif
        // || TextUtils.equals(mime, "image/heif")
        // || TextUtils.equals(mime, "image/heif-sequence");

    }

    public static boolean isGif(String fileName) {
        if (TextUtils.isEmpty(fileName)) {
            return false;
        }

        String suffix = FilenameUtils.getExtension(fileName);
        if (TextUtils.isEmpty(suffix)) {
            return false;
        }

        suffix = suffix.toLowerCase(Locale.ROOT);
        if (TextUtils.equals("gif", suffix)) {
            return true;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return TextUtils.equals(mime, "image/gif");
    }

    public static boolean availableThumbnail(String fileName) {
        if (isViewableImage(fileName)) {
            return true;
        }

        if (isVideoFile(fileName)) {
            return true;
        }

        if (isSDoc(fileName)) {
            return true;
        }

        if (isPdfFile(fileName)) {
            return true;
        }

        return false;
    }

    public static boolean isViewableImage(String name) {
        String suffix = FilenameUtils.getExtension(name);

        if (TextUtils.isEmpty(suffix)) {
            return false;
        }

        suffix = suffix.toLowerCase(Locale.ROOT);

        switch (suffix) {
            case "svg" -> {
                return false;// don't support svg preview
            }
            case "psd" -> {
                return false;// don't support psd preview
            }
            case "tif" -> {
                return false;// don't support tiff preview
            }
            case "tiff" -> {
                return false;// don't support tiff preview
            }
        }

        String mime = getMimeType(suffix);

        if (mime == null)
            return false;
        return mime.contains("image/");
    }

    public static String getMimeTypeFromUri(Context context, Uri uri) {
        return context.getContentResolver().getType(uri);
    }

    public static String getMimeType(String extension) {
        if (TextUtils.isEmpty(extension)) {
            return null;
        }

        extension = extension.toLowerCase(Locale.ROOT);

        // For devices with lower versions of Android, handle heic and heif explicitly
        if ("heic".equals(extension)) {
            return "image/heic";
        }

        if ("heif".equals(extension)) {
            return "image/heif";
        }

        return MimeTypeMap.getSingleton().getMimeTypeFromExtension(extension);
    }

    public static boolean isVideoFile(String name) {
        String suffix = FilenameUtils.getExtension(name);
        if (TextUtils.isEmpty(suffix)) {
            return false;
        }

        if (suffix.equals("flv")) {
            return true;
        }
        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        if (mime == null)
            return false;
        return mime.contains("video/");
    }

    private static final Set<String> SUPPORTED_OFFICE_EXTENDS = new HashSet<>(Arrays.asList(
            "doc", "docx", "dotm", "docm", "dotx",
            "odt", "ods", "odp", "ott", "ots",
            "xlsx", "xls", "xlt", "xlsb", "xlsm", "xltm", "xltx",
            "ppt", "pptx", "ppsx", "potx", "pptm", "ppsm",
            "pages",
            "csv"
    ));

    public static boolean isSupportOfficeFilePreview(String name) {
        if (StringUtils.isEmpty(name)) {
            return false;
        }

        name = name.trim().toLowerCase(Locale.ROOT);
        if (!name.contains(".")) {
            return false;
        }

        String suffix = FileUtils.getFileExtension(name);
        if (StringUtils.isEmpty(suffix)) {
            return false;
        }

        return SUPPORTED_OFFICE_EXTENDS.contains(suffix);
    }

    public static boolean isPdfFile(String name) {
        if (TextUtils.isEmpty(name)) {
            return false;
        }

        String suffix = FilenameUtils.getExtension(name);
        if (TextUtils.isEmpty(suffix)) {
            return false;
        }
        suffix = suffix.toLowerCase(Locale.ROOT);
        if (TextUtils.equals("pdf", suffix)) {
            return true;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return TextUtils.equals(mime, "application/pdf");
    }

    @NonNull
    public static String pathJoin(String first, String... rest) {
        if (first == null) {
            first = "";
        }

        if (rest == null || rest.length == 0) {
            return first;
        }

        StringBuilder result = new StringBuilder(first);
        for (String b : rest) {
            if (TextUtils.isEmpty(b)) {
                continue;
            }

            boolean resultEndsWithSlash = result.toString().endsWith("/");
            boolean bStartWithSlash = b.startsWith("/");
            if (resultEndsWithSlash && bStartWithSlash) {
                result.append(b.substring(1));
            } else if (resultEndsWithSlash || bStartWithSlash) {
                result.append(b);
            } else {
                result.append("/");
                result.append(b);
            }
        }

        return result.toString();
    }

    public static String removeLastPathSeparator(String path) {
        if (TextUtils.isEmpty(path)) return null;

        int size = path.length();
        if (path.endsWith("/")) {
            return path.substring(0, size - 1);
        } else
            return path;
    }

    /**
     * Strip leading and trailing slashes
     */
    public static String stripSlashes(String a) {
        return a.replaceAll("^[/]*|[/]*$", "");
    }

    public static boolean isJson(String str) {
        if (TextUtils.isEmpty(str)) {
            return false;
        }

        try {
            Object value = new JSONTokener(str).nextValue();
            return value instanceof JSONObject || value instanceof JSONArray;
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Translate commit time to human readable time description
     */
    public static String translateCommitTime(long timestampInMillis) {
        long now = System.currentTimeMillis();
        if (now <= timestampInMillis) {
            return SeadroidApplication.getAppString(R.string.just_now);
        }

        long delta = (now - timestampInMillis) / 1000;

        long secondsPerDay = 24 * 60 * 60;

        long days = delta / secondsPerDay;
        long seconds = delta % secondsPerDay;

        if (days >= 14) {
            Date d = new Date(timestampInMillis);
            SimpleDateFormat fmt = new SimpleDateFormat("yyyy-MM-dd");
            return fmt.format(d);
        } else if (days > 0) {
            return SeadroidApplication.getAppString(R.string.days_ago, days);
        } else if (seconds >= 60 * 60) {
            long hours = seconds / 3600;
            return SeadroidApplication.getAppString(R.string.hours_ago, hours);
        } else if (seconds >= 60) {
            long minutes = seconds / 60;
            return SeadroidApplication.getAppString(R.string.minutes_ago, minutes);
        } else if (seconds > 0) {
            return SeadroidApplication.getAppString(R.string.seconds_ago, seconds);
        } else {
            return SeadroidApplication.getAppString(R.string.just_now);
        }
    }

    public static long now() {
        return System.currentTimeMillis();
    }

    public static String getFileMimeType(File file) {
        return getFileMimeType(file.getPath());
    }

    public static String getFileMimeType(String path) {
        String name = getFileNameFromPath(path);
        if (TextUtils.isEmpty(name)) {
            return MIME_APPLICATION_OCTET_STREAM;
        }

        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (TextUtils.isEmpty(suffix)) {
            return MIME_APPLICATION_OCTET_STREAM;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        if (TextUtils.isEmpty(mime)) {
            return MIME_APPLICATION_OCTET_STREAM;
        }
        return mime;
    }


    @Nullable
    public static String getFilenameFromUri(Context context, Uri uri) {

        if (uri == null) {
            return null;
        }

        ContentResolver resolver = context.getContentResolver();
        Cursor cursor = resolver.query(uri, null, null, null, null);
        String displayName = null;
        if (cursor != null && cursor.moveToFirst()) {

            // Note it's called "Display Name".  This is
            // provider-specific, and might not necessarily be the file name.
            int index = cursor.getColumnIndexOrThrow(OpenableColumns.DISPLAY_NAME);
            displayName = cursor.getString(index);
            cursor.close();
        } else if ("file".equalsIgnoreCase(uri.getScheme())) {
            displayName = uri.getPath().replaceAll(".*/", "");
        } else displayName = "unknown filename";
        return displayName;
    }

    public static String assembleUserName(String name, String email, String server) {
        if (name == null || email == null || server == null)
            return null;

        if (TextUtils.isEmpty(name) || TextUtils.isEmpty(email) || TextUtils.isEmpty(server))
            return "";

        if (server.startsWith(Constants.Protocol.HTTPS)) {
            server = server.substring(8);
        }

        if (server.startsWith(Constants.Protocol.HTTP)) {
            server = server.substring(7);
        }

        // strip port, like :8000 in 192.168.1.116:8000
        if (server.contains(":")) {
            server = server.substring(0, server.indexOf(':'));
        }
//        String info = String.format("%s (%s)", email, server);//settingFragmeng set account name
        String info = String.format("%s (%s)", name, server);
        info = info.replaceAll("[^\\w\\d\\.@\\(\\) ]", "_");
        return info;
    }


    public static String cleanServerURL(String serverURL) throws MalformedURLException {
        if (!serverURL.endsWith("/")) {
            serverURL = serverURL + "/";
        }

        // XXX: android 4.0.3 ~ 4.0.4 can't handle urls with underscore (_) in the host field.
        // See https://github.com/nostra13/Android-Universal-Image-Loader/issues/256 , and
        // https://code.google.com/p/android/issues/detail?id=24924
        //
        new URL(serverURL); // will throw MalformedURLException if serverURL not valid
        return serverURL;
    }

    /**
     * Determine if the current system language environment is Chinese.
     * <p>
     * It is compatible with cases where only the language (zh) is set without specifying the region,
     * as well as cases involving Mainland China, Taiwan, Hong Kong, and Macau.
     */
    public static boolean isInChina() {
        Locale locale = Locale.getDefault();

        if (TextUtils.equals(Locale.CHINESE.getLanguage(), locale.getLanguage())) {
            return true;
        }

        String country = locale.getCountry();
        return TextUtils.equals(Locale.CHINA.getCountry(), country)
                || TextUtils.equals(Locale.TAIWAN.getCountry(), country)
                || TextUtils.equals("HK", country)
                || TextUtils.equals("MO", country);
    }

    /**
     * File extensions that can be opened as plain text/code
     */
    public static final Set<String> TEXT_EXTENSIONS = new HashSet<>(Arrays.asList(
            // plain text and documents
            "txt", "text", "log", "csv", "tsv", "diff", "patch", "rej", "org", "rst", "adoc", "asciidoc", "tex",
            // Markdown
            "md", "markdown", "mdx",
            // Web
            "htm", "html", "xhtml", "css", "less", "scss", "sass", "js", "mjs", "cjs", "jsx", "ts", "tsx",
            "vue", "svelte", "json", "json5", "jsonc", "xml", "xsd", "xsl", "xslt", "dtd", "yml", "yaml",
            "graphql", "gql",
            // JVM
            "java", "kt", "kts", "groovy", "gradle", "scala", "sc", "clj", "cljs", "cljc", "edn",
            // C 系
            "c", "h", "cc", "cpp", "cxx", "hpp", "hxx", "m", "mm", "cs", "fs", "fsx", "fsi",
            // script
            "py", "rb", "php", "pl", "pm", "lua", "r", "jl", "sh", "bash", "zsh", "fish", "ksh",
            "bat", "cmd", "ps1", "psm1", "psd1", "awk", "el", "vim", "vi", "scd", "schelp", "pde", "script",
            // other
            "go", "rs", "swift", "dart", "hs", "lhs", "ml", "mli", "ex", "exs", "erl", "hrl",
            "nim", "zig", "pas", "asm", "s", "sol", "tf", "tfvars", "hcl", "proto",
            "sql", "plsql", "v", "sv", "vhd", "vhdl",
            // config and build
            "ini", "cfg", "conf", "config", "properties", "toml", "env", "lock", "mod", "sum",
            "make", "cmake", "dockerfile", "containerfile", "gitignore", "gitattributes", "gitmodules",
            "editorconfig", "dockerignore", "npmignore", "ac", "am"
    ));

    /**
     * Common filenames without a file extension, but clearly text files.
     */
    private static final Set<String> TEXT_FILE_NAMES = new HashSet<>(Arrays.asList(
            "dockerfile", "containerfile", "makefile", "gnumakefile", "gemfile", "rakefile", "vagrantfile",
            "jenkinsfile", "procfile", "brewfile", "license", "licence", "readme", "changelog",
            "authors", "contributors", "copying", "notice", "todo", "version", "go.mod", "go.sum"
    ));

    public static boolean isTextMimeType(String fileName) {
        if (TextUtils.isEmpty(fileName)) {
            return false;
        }

        String name = fileName.trim().toLowerCase(Locale.ROOT);

        // Dockerfile.dev、.env.local 等变体
        if (name.startsWith("dockerfile") || name.startsWith(".env")) {
            return true;
        }

        int index = name.lastIndexOf('.');
        String baseName = index >= 0 ? name.substring(0, index) : name;
        String suffix = index >= 0 ? name.substring(index + 1) : "";

        if (TEXT_FILE_NAMES.contains(name) || TEXT_FILE_NAMES.contains(baseName)) {
            return true;
        }

        return !TextUtils.isEmpty(suffix) && TEXT_EXTENSIONS.contains(suffix);
    }


    /**
     * SslCertificate class does not has a public getter for the underlying
     * X509Certificate, we can only do this by hack. This only works for android 4.0+
     *
     * @see https://groups.google.com/forum/#!topic/android-developers/eAPJ6b7mrmg
     */
    public static X509Certificate getX509CertFromSslCertHack(SslCertificate sslCert) {
        X509Certificate x509Certificate = null;

        Bundle bundle = SslCertificate.saveState(sslCert);
        byte[] bytes = bundle.getByteArray("x509-certificate");

        if (bytes == null) {
            x509Certificate = null;
        } else {
            try {
                CertificateFactory certFactory = CertificateFactory.getInstance("X.509");
                Certificate cert = certFactory.generateCertificate(new ByteArrayInputStream(bytes));
                x509Certificate = (X509Certificate) cert;
            } catch (CertificateException e) {
                x509Certificate = null;
            }
        }

        return x509Certificate;
    }

    public static boolean isSameCert(SslCertificate sslCert, X509Certificate x509Cert) {
        if (sslCert == null || x509Cert == null) {
            return false;
        }

        X509Certificate realCert = getX509CertFromSslCertHack(sslCert);
        if (realCert != null) {
            // for android 4.0+
            return realCert.equals(x509Cert);
        } else {
            // for andorid < 4.0
            return SslCertificateComparator.compare(sslCert,
                    new SslCertificate(x509Cert));
        }
    }

    /**
     * Compare SslCertificate objects for android before 4.0
     */
    public static class SslCertificateComparator {
        private SslCertificateComparator() {
        }

        public static boolean compare(SslCertificate cert1, SslCertificate cert2) {
            return isSameDN(cert1.getIssuedTo(), cert2.getIssuedTo())
                    && isSameDN(cert1.getIssuedBy(), cert2.getIssuedBy())
                    && isSameDate(cert1.getValidNotBeforeDate(), cert2.getValidNotBeforeDate())
                    && isSameDate(cert1.getValidNotAfterDate(), cert2.getValidNotAfterDate());
        }

        private static boolean isSameDate(Date date1, Date date2) {
            if (date1 == null && date2 == null) {
                return true;
            } else if (date1 == null || date2 == null) {
                return false;
            }

            return date1.equals(date2);
        }

        private static boolean isSameDN(SslCertificate.DName dName1, SslCertificate.DName dName2) {
            if (dName1 == null && dName2 == null) {
                return true;
            } else if (dName1 == null || dName2 == null) {
                return false;
            }

            return dName1.getDName().equals(dName2.getDName());
        }
    }

    public static final String EXCEPTION_TYPE_CRASH = "crash_exception";

    /**
     * Compare two server versions.
     *
     * @return 0: equal, -1: version1 < version2, 1: version1 > version2
     */
    public static int compareVersion(@Nullable String version1, @Nullable String version2) {
        if (TextUtils.equals(version1, version2)) {
            return 0;
        }
        if (TextUtils.isEmpty(version1)) {
            return TextUtils.isEmpty(version2) ? 0 : -1;
        }
        if (TextUtils.isEmpty(version2)) {
            return 1;
        }

        String[] v1Parts = version1.split("\\.");
        String[] v2Parts = version2.split("\\.");
        int maxLen = Math.max(v1Parts.length, v2Parts.length);
        for (int i = 0; i < maxLen; i++) {
            int p1 = parseVersionPart(v1Parts, i);
            int p2 = parseVersionPart(v2Parts, i);
            if (p1 < p2) {
                return -1;
            }
            if (p1 > p2) {
                return 1;
            }
        }
        return 0;
    }

    private static int parseVersionPart(String[] parts, int index) {
        if (index >= parts.length) {
            return 0;
        }
        String part = parts[index];
        if (TextUtils.isEmpty(part)) {
            return 0;
        }
        try {
            return Integer.parseInt(part);
        } catch (NumberFormatException e) {
            String numericPart = part.replaceAll("[^0-9]", "");
            if (TextUtils.isEmpty(numericPart)) {
                return 0;
            }
            try {
                return Integer.parseInt(numericPart);
            } catch (NumberFormatException ignored) {
                return 0;
            }
        }
    }

}
