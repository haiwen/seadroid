package com.seafile.seadroid2.framework.util;

import android.app.Activity;
import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.net.http.SslCertificate;
import android.os.Build;
import android.os.Bundle;
import android.os.LocaleList;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import android.provider.OpenableColumns;
import android.text.TextUtils;
import android.text.format.DateFormat;
import android.util.Log;
import android.view.View;
import android.webkit.MimeTypeMap;

import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.annotation.NotSupport;
import com.seafile.seadroid2.config.Constants;

import org.json.JSONObject;
import org.json.JSONTokener;

import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URISyntaxException;
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
import java.util.Locale;

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
    private static final int JOB_ID = 0;

    private Utils() {
    }

    public static JSONObject parseJsonObject(String json) {
        if (json == null) {
            // the caller should not give null
            Log.w(DEBUG_TAG, "null in parseJsonObject");
            return null;
        }

        try {
            return (JSONObject) new JSONTokener(json).nextValue();
        } catch (Exception e) {
            return null;
        }
    }

    public static String getPathFromFullPath(String path) {
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

        String parent = path.substring(0, path.lastIndexOf("/"));
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

        if (!path.contains("/")) {
            return "/";
        }

        if ("/".equals(path)) {
            return "/";
        }

        if (path.endsWith("/")) {
            path = path.substring(0, path.lastIndexOf("/"));
        }

        String parent = path.substring(0, path.lastIndexOf("/"));
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

        return path.substring(path.lastIndexOf("/") + 1);
    }

    public static final String[] _units = new String[]{"B", "KB", "MB", "GB", "TB"};
    public static final DecimalFormat _decimalFormat = new DecimalFormat("#,##0.#");

    public static String readableFileSize(long size) {
        if (size <= 0) return "0 KB";
        int digitGroups = (int) (Math.log10(size) / Math.log10(1000));
        return _decimalFormat.format(size / Math.pow(1000, digitGroups)) + " " + _units[digitGroups];
    }

    public static boolean isViewableImage(String name) {
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (suffix.isEmpty())
            return false;

        if (suffix.equals("svg"))
            // don't support svg preview
            return false;

        if (suffix.equals("psd"))
            // don't support psd preview
            return false;

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        if (mime == null)
            return false;
        return mime.contains("image/");
    }

    public static boolean isVideoFile(String name) {
        if (name == null)
            return false;
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (TextUtils.isEmpty(suffix))
            return false;
        if (suffix.equals("flv")) {
            return true;
        }
        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        if (mime == null)
            return false;
        return mime.contains("video/");
    }

    public static boolean isTextFile(File file) {
        if (file != null) {
            String fileName = file.getName();
            if (!TextUtils.isEmpty(fileName)) {
                String suffix = fileName.substring(fileName.lastIndexOf('.') + 1).toLowerCase();
                if (!TextUtils.isEmpty(suffix)) {
                    String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
                    if (!TextUtils.isEmpty(mime)) {
                        return mime.contains("text/") || FileMimeUtils.isOfficeOrTextFile(mime);
                    }
                }
            }
        }
        return false;
    }

    public static String pathJoin(String first, String... rest) {
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

    public static String getCurrentHourMinute() {
        return (String) DateFormat.format("hh:mm", new Date());
    }

    /**
     * Translate commit time to human readable time description
     */
    public static String translateCommitTime(long timestampInMillis) {
        long now = System.currentTimeMillis();
        if (now <= timestampInMillis) {
            return SeadroidApplication.getAppContext().getString(R.string.just_now);
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
            return SeadroidApplication.getAppContext().getString(R.string.days_ago, days);
        } else if (seconds >= 60 * 60) {
            long hours = seconds / 3600;
            return SeadroidApplication.getAppContext().getString(R.string.hours_ago, hours);
        } else if (seconds >= 60) {
            long minutes = seconds / 60;
            return SeadroidApplication.getAppContext().getString(R.string.minutes_ago, minutes);
        } else if (seconds > 0) {
            return SeadroidApplication.getAppContext().getString(R.string.seconds_ago, seconds);
        } else {
            return SeadroidApplication.getAppContext().getString(R.string.just_now);
        }
    }

    public static long now() {
        return System.currentTimeMillis();
    }

    public static String getFileMimeType(String path) {
        String name = getFileNameFromPath(path);
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (suffix.isEmpty()) {
            return MIME_APPLICATION_OCTET_STREAM;
        } else {
            String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
            if (mime != null) {
                return mime;
            } else {
                return MIME_APPLICATION_OCTET_STREAM;
            }
        }
    }

    public static String getFileMimeType(File file) {
        return getFileMimeType(file.getPath());
    }

    public static String getFilenameFromUri(Context context, Uri uri) {

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

    @NotSupport
    @Deprecated
    public static String getPath(Context context, Uri uri) throws URISyntaxException {
        if ("content".equalsIgnoreCase(uri.getScheme())) {
            String[] projection = {"_data"};
//            String[] projection = {MediaStore.Files.FileColumns.DATA};

            Cursor cursor = context.getContentResolver().query(uri, projection, null, null, null);
            if (cursor != null) {
                try {
                    if (cursor.moveToFirst()) {
                        int column_index = cursor.getColumnIndexOrThrow(MediaStore.Files.FileColumns.DATA);
                        return cursor.getString(column_index);
                    }
                } catch (Exception e) {
                    // Eat it
                    SLogs.e(e);
                } finally {
                    cursor.close();
                }
            }


        } else if ("file".equalsIgnoreCase(uri.getScheme())) {
            return uri.getPath();
        }

        return null;
    }

    public static String getMimeType(Context context, Uri uri) {
        return context.getContentResolver().getType(uri);
    }

    public static long getFileSize(Context context, Uri uri) {
        long size = -1;
        Cursor cursor = context.getContentResolver().query(uri, null, null, null, null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    int sizeIndex = cursor.getColumnIndex(OpenableColumns.SIZE);
                    if (!cursor.isNull(sizeIndex)) {
                        size = cursor.getLong(sizeIndex);
                    }
                }
            } finally {
                cursor.close();
            }
        }
        return size;
    }

    public static long getFileCreateTime(Context context, Uri uri) {
        String[] projection = {DocumentsContract.Document.COLUMN_LAST_MODIFIED};
        Cursor cursor = context.getContentResolver().query(uri, projection, null, null, null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    int lastModifiedIndex = cursor.getColumnIndex(DocumentsContract.Document.COLUMN_LAST_MODIFIED);
                    if (!cursor.isNull(lastModifiedIndex)) {
                        return cursor.getLong(lastModifiedIndex);
                    }
                }
            } finally {
                cursor.close();
            }
        }
        return -1; // 如果无法获取创建时间，返回 -1
    }

    public static String getStackTrace(Exception e) {
        StringWriter buffer = new StringWriter();
        PrintWriter writer = new PrintWriter(buffer);
        e.printStackTrace(writer);
        return buffer.toString();
    }

    public static int calculateInSampleSize(BitmapFactory.Options options, int reqWidth, int reqHeight) {
        // Raw height and width of image
        final int height = options.outHeight;
        final int width = options.outWidth;
        int inSampleSize = 1;

        if (height > reqHeight || width > reqWidth) {

            final int halfHeight = height / 2;
            final int halfWidth = width / 2;

            // Calculate the largest inSampleSize value that is a power of 2 and keeps both
            // height and width larger than the requested height and width.
            while ((halfHeight / inSampleSize) > reqHeight
                    && (halfWidth / inSampleSize) > reqWidth) {
                inSampleSize *= 2;
            }
        }

        return inSampleSize;
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
     * use compare user system  is chinese
     */
    public static boolean isInChina() {
        Locale locale;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            locale = LocaleList.getDefault().get(0);
        } else {
            locale = Locale.getDefault();
        }
        String language = locale.getCountry();
        return TextUtils.equals("CN", language) || TextUtils.equals("TW", language);
    }


    public final static boolean isValidEmail(CharSequence target) {
        return !TextUtils.isEmpty(target) && android.util.Patterns.EMAIL_ADDRESS.matcher(target).matches();
    }

    public static boolean isTextMimeType(String fileName) {
        if (!fileName.contains(".")) {
            return false;
        }

        String suffix = fileName.substring(fileName.lastIndexOf('.') + 1).toLowerCase();
        if (TextUtils.isEmpty(suffix)) {
            return false;
        }

        //file is markdown or txt
        String[] array = {"ac", "am", "adoc", "asciidoc", "bat", "c", "cc", "cmake", "conf", "cpp", "cs", "css", "csv", "diff",
                "el", "go", "groovy", "h", "htm", "html", "java", "js", "json", "less", "log", "make",
                "markdown", "md", "org", "patch", "pde", "php", "pl", "properties", "py", "rb", "rst",
                "sc", "scala", "scd", "schelp", "script", "sh", "sql", "text", "tex", "txt", "vi", "vim",
                "xhtml", "xml", "yml", "adoc"};
        return Arrays.asList(array).contains(suffix);
    }


    /**
     * SslCertificate class does not has a public getter for the underlying
     * X509Certificate, we can only do this by hack. This only works for andorid 4.0+
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


    public static void hideSystemNavigationBar(Activity activity) {
        if (activity == null) {
            return;
        }
        if (Build.VERSION.SDK_INT < 19) {
            View view = activity.getWindow().getDecorView();
            view.setSystemUiVisibility(View.GONE);
        } else if (Build.VERSION.SDK_INT >= 19) {
            View decorView = activity.getWindow().getDecorView();
            int uiOptions = View.SYSTEM_UI_FLAG_HIDE_NAVIGATION | View.SYSTEM_UI_FLAG_IMMERSIVE_STICKY | View.SYSTEM_UI_FLAG_FULLSCREEN;
            decorView.setSystemUiVisibility(uiOptions);
        }
    }


    public static void startCameraSyncJob(Context context) {
//        JobScheduler mJobScheduler = (JobScheduler) context.getSystemService(Context.JOB_SCHEDULER_SERVICE);
//        JobInfo.Builder builder = new JobInfo.Builder(JOB_ID, new ComponentName(context.getPackageName(), MediaSchedulerService.class.getName()));
//        builder.setMinimumLatency(5 * 1000);// Set to execute after at least 15 minutes delay
//        builder.setOverrideDeadline(60 * 60 * 1000);// The setting is delayed by 20 minutes,
//        builder.setRequiresCharging(false);
//        builder.setRequiresDeviceIdle(false);
//        builder.setRequiredNetworkType(JobInfo.NETWORK_TYPE_ANY);
//        builder.setPersisted(true);
//        mJobScheduler.schedule(builder.build());
    }

    public static String getSyncCompletedTime() {
        SimpleDateFormat formatter = new SimpleDateFormat("MM-dd HH:mm");
        Date date = new Date(System.currentTimeMillis());
        String completedTime = formatter.format(date);
        return completedTime;
    }


    public static String getRealPathFromURI(Context context, Uri contentUri, String media) {
        Cursor cursor = null;
        try {
            if (media.equals("images")) {//image
                String[] proj = {MediaStore.Images.Media.DATA};
                cursor = context.getContentResolver().query(contentUri, proj, null, null, null);
                int column_index = cursor.getColumnIndexOrThrow(MediaStore.Images.Media.DATA);
                cursor.moveToFirst();
                return cursor.getString(column_index);
            } else {//Video
                String[] proj = {MediaStore.Video.Media.DATA};
                cursor = context.getContentResolver().query(contentUri, proj, null, null, null);
                int column_index = cursor.getColumnIndexOrThrow(MediaStore.Video.Media.DATA);
                cursor.moveToFirst();
                return cursor.getString(column_index);
            }

        } catch (Exception e) {
            e.printStackTrace();
            return null;
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }
    }

    public static final String EXCEPTION_TYPE_CRASH = "crash_exception";
}

