package com.seafile.seadroid2;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.TreeMap;

import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.net.NetworkInfo.DetailedState;
import android.util.Log;
import android.webkit.MimeTypeMap;

import com.seafile.seadroid2.data.SeafRepo;

public class Utils {
    public static final String MIME_APPLICATION_OCTET_STREAM = "application/octet-stream";

    public static JSONObject parseJsonObject(String json) {
        if (json == null) {
            // the caller should not give null
            Log.w("Utils", "null in parseJsonObject");
            return null;
        }

        try {
            return (JSONObject) new JSONTokener(json).nextValue();
        } catch (Exception e) {
            return null;
        }
    }

    public static JSONArray parseJsonArray(String json) {
        if (json == null) {
         // the caller should not give null
            Log.w("Utils", "null in parseJsonObject");
            return null;
        }

        try {
            return (JSONArray) new JSONTokener(json).nextValue();
        } catch (Exception e) {
            return null;
        }
    }

    /** Read input stream and convert the content to string.
     */
    public static String readIt(InputStream stream) throws IOException,
            UnsupportedEncodingException {
        Reader reader = new InputStreamReader(stream, "UTF-8");
        char[] buffer = new char[1024];
        StringBuilder responseStrBuilder = new StringBuilder();

        while (true) {
            int len = reader.read(buffer, 0, 1024);
            if (len == -1)
                break;
            responseStrBuilder.append(buffer, 0, len);
        }
        return responseStrBuilder.toString();
    }

    public static String readFile(File file) {
        Reader reader = null;
        try {
            try {
                // TODO: detect a file's encoding
                reader = new InputStreamReader(new FileInputStream(file), "UTF-8");
            } catch (UnsupportedEncodingException e) {
                return null;
            }

            char[] buffer = new char[1024];
            StringBuilder responseStrBuilder = new StringBuilder();

            while (true) {
                int len = reader.read(buffer, 0, 1024);
                if (len == -1)
                    break;
                responseStrBuilder.append(buffer, 0, len);
            }
            return responseStrBuilder.toString();
        } catch (IOException e) {
            return null;
        } finally {
            try {
                if (reader != null)
                    reader.close();
            } catch (Exception e) {

            }
        }
    }

    public static String getParentPath(String path) {
        if (path == null) {
            // the caller should not give null
            Log.w("Utils", "null in getParentPath");
            return null;
        }

        String parent = path.substring(0, path.lastIndexOf("/"));
        if (parent.equals("")) {
            return "/";
        } else
            return parent;
    }

    public static String fileNameFromPath(String path) {
        if (path == null) {
            // the caller should not give null
            Log.w("Utils", "null in getParentPath");
            return null;
        }

        return path.substring(path.lastIndexOf("/") + 1);
    }

    public static String readableFileSize(long size) {
        if(size <= 0) return "0 KB";
        final String[] units = new String[] { "B", "KB", "MB", "GB", "TB" };
        int digitGroups = (int) (Math.log10(size)/Math.log10(1024));
        return new DecimalFormat("#,##0.#").format(size/Math.pow(1024, digitGroups)) + " " + units[digitGroups];
    }

    public static void writeFile(File file, String content) throws IOException {
        OutputStream os = null;
        try {
            os = new FileOutputStream(file);
            os.write(content.getBytes());
        } finally {
            try {
                if (os != null)
                    os.close();
            } catch (Exception e) {
                // ignore
            }
        }
    }

    public static String NOGROUP = "$nogroup";

    public static TreeMap<String, List<SeafRepo>> groupRepos(List<SeafRepo> repos) {
        TreeMap<String, List<SeafRepo>> map = new TreeMap<String, List<SeafRepo>>();
        for (SeafRepo repo : repos) {
            List<SeafRepo> l;
            String groupName = repo.isGroupRepo ? repo.owner : NOGROUP;
            l = map.get(groupName);
            if (l == null) {
                l = new ArrayList<SeafRepo>();
                map.put(groupName, l);
            }
            l.add(repo);
        }
        return map;
    }

    public static int getResIdforMimetype(String mimetype) {
        if (mimetype == null)
            return R.drawable.file;

        if (mimetype.contains("pdf")) {
            return R.drawable.file_pdf;
        } else if (mimetype.contains("image")) {
            return R.drawable.file_image;
        } else if (mimetype.contains("text")) {
            return R.drawable.file_text;
        } else if (mimetype.contains("audio")) {
            return R.drawable.file_audio;
        } else if (mimetype.contains("video")) {
            return R.drawable.file_video;
        } if (mimetype.contains("pdf")) {
            return R.drawable.file_pdf;
        } else if (mimetype.contains("msword") || mimetype.contains("ms-word")) {
            return R.drawable.file_ms_word;
        } else if (mimetype.contains("mspowerpoint") || mimetype.contains("ms-powerpoint")) {
            return R.drawable.file_ms_ppt;
        } else if (mimetype.contains("msexcel") || mimetype.contains("ms-excel")) {
            return R.drawable.file_ms_excel;
        } else if (mimetype.contains("openxmlformats-officedocument")) {
            // see http://stackoverflow.com/questions/4212861/what-is-a-correct-mime-type-for-docx-pptx-etc
            if (mimetype.contains("wordprocessingml")) {
                return R.drawable.file_ms_word;
            } else if (mimetype.contains("spreadsheetml")) {
                return R.drawable.file_ms_excel;
            } else if (mimetype.contains("presentationml")) {
                return R.drawable.file_ms_ppt;
            }
        } else if (mimetype.contains("application")) {
            return R.drawable.file_binary;
        }

        return R.drawable.file;
    }

    static HashMap<String, Integer> suffixIconMap = null;

    static private HashMap<String, Integer> getSuffixIconMap() {
        if (suffixIconMap != null)
            return suffixIconMap;

        suffixIconMap = new HashMap<String, Integer>();
        suffixIconMap.put("pdf", R.drawable.file_pdf);
        suffixIconMap.put("doc", R.drawable.file_ms_word);
        suffixIconMap.put("docx", R.drawable.file_ms_word);
        suffixIconMap.put("md", R.drawable.file_text);
        suffixIconMap.put("markdown", R.drawable.file_text);
        return suffixIconMap;
    }

    public static int getFileIcon(String name) {
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (suffix.length() == 0) {
            return R.drawable.file;
        }

        HashMap<String, Integer> map = getSuffixIconMap();
        Integer i = map.get(suffix);
        if (i != null)
            return i;

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        return getResIdforMimetype(mime);
    }

    public static boolean isViewableImage(String name) {
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (suffix.length() == 0)
            return false;
        if (suffix.equals("svg"))
            // don't support svg preview
            return false;

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        if (mime == null)
            return false;
        return mime.contains("image");
    }

    public static boolean isNetworkOn() {
        ConnectivityManager connMgr = (ConnectivityManager)
                SeadroidApplication.getAppContext().getSystemService(
                        Context.CONNECTIVITY_SERVICE);

        NetworkInfo wifi = connMgr.getNetworkInfo(ConnectivityManager.TYPE_WIFI);
        if(wifi != null && wifi.isAvailable()
           && wifi.getDetailedState() == DetailedState.CONNECTED) {
            return true;
        }

        NetworkInfo mobile = connMgr.getNetworkInfo(ConnectivityManager.TYPE_MOBILE);
        if(mobile != null && mobile.isAvailable()
           && mobile.getDetailedState() == DetailedState.CONNECTED) {
            return true;
        }

        return false;
    }

    public static String pathJoin (String first, String... rest) {
        String path = first;
        for (String b: rest) {
            if (path.endsWith("/") && b.startsWith("/")) {
                path = path + b.substring(1);
            } else if (path.endsWith("/") || b.startsWith("/")) {
                path += b;
            } else {
                path += "/" + b;
            }
        }

        return path;
    }

    /**
     * Strip leading and trailing slashes
     */
    public static String stripSlashes(String a) {
        return a.replaceAll("^[/]*|[/]*$", "");
    }

    /**
     * Translate commit time to human readable time description
     */
    public static String translateCommitTime(long timestampInMillis) {
        long now = Calendar.getInstance().getTimeInMillis();
        if (now <= timestampInMillis) {
            return "Just now";
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
            return days + SeadroidApplication.getAppContext().getString(R.string.days_ago);
        } else if (seconds >= 60 * 60) {
            long hours = seconds / 3600;
            return hours + SeadroidApplication.getAppContext().getString(R.string.hours_ago);
        } else if (seconds >= 60) {
            long minutes = seconds / 60;
            return minutes + SeadroidApplication.getAppContext().getString(R.string.minutes_ago);
        } else if (seconds > 0) {
            return seconds + SeadroidApplication.getAppContext().getString(R.string.seconds_ago);
        } else {
            return SeadroidApplication.getAppContext().getString(R.string.just_now);
        }
    }

    public static long now() {
        return Calendar.getInstance().getTimeInMillis();
    }

    public static String getFileMimeType(String path) {
        String name = fileNameFromPath(path);
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();
        if (suffix.length() == 0) {
            return MIME_APPLICATION_OCTET_STREAM;
        } else {
            String mime =  MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
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

    public static void copyFile(File src, File dst) throws IOException {
        InputStream in = new BufferedInputStream(new FileInputStream(src));
        OutputStream out = new BufferedOutputStream(new FileOutputStream(dst));

        // Transfer bytes from in to out
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
    }
}
