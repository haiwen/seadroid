package com.seafile.seadroid;

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
import java.util.ArrayList;
import java.util.List;
import java.util.TreeMap;
import java.util.HashMap;

import org.json.*;

import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.util.Log;
import android.webkit.MimeTypeMap;

import com.seafile.seadroid.data.SeafRepo;

public class Utils {
    
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
        
        if (mimetype.contains("pdf"))
            return R.drawable.file_pdf;
        if (mimetype.contains("application")) {
            return R.drawable.file_application;
        } else if (mimetype.contains("image")) {
            return R.drawable.file_image;
        } else if (mimetype.contains("text")) {
            return R.drawable.ic_text;
        } else if (mimetype.contains("audio")) {
            return R.drawable.file_audio;
        } else if (mimetype.contains("video")) {
            return R.drawable.file_video;
        } if (mimetype.contains("pdf")) {
            return R.drawable.file_pdf;
        } else {
            return R.drawable.file;
        }
    }
    
    public static int getResIdforMimetypeLarge(String mimetype) {
        if (mimetype == null)
            return R.drawable.file;
        
        if (mimetype.contains("pdf"))
            return R.drawable.file_pdf;
        else if (mimetype.contains("application")) {
            return R.drawable.file_application;
        } else if (mimetype.contains("image")) {
            return R.drawable.image;
        } else if (mimetype.contains("text")) {
            return R.drawable.text;
        } else if (mimetype.contains("audio")) {
            return R.drawable.audio;
        } else if (mimetype.contains("video")) {
            return R.drawable.video;
        } else {
            return R.drawable.file;
        }
    }
    
    static HashMap<String, Integer> suffixIconMap = null;
    
    static private HashMap<String, Integer> getSuffixIconMap() {
        if (suffixIconMap != null) 
            return suffixIconMap;
        
        suffixIconMap = new HashMap<String, Integer>();
        suffixIconMap.put("pdf", R.drawable.file_pdf);
        suffixIconMap.put("doc", R.drawable.file_doc);
        suffixIconMap.put("docx", R.drawable.file_doc);
        return suffixIconMap;
    }
    
    public static int getFileIcon(String name) {   
        String suffix = name.substring(name.lastIndexOf('.') + 1);
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
    
    public static boolean isNetworkOn() {
        ConnectivityManager connMgr = (ConnectivityManager) 
                SeadroidApplication.getAppContext().getSystemService(
                        Context.CONNECTIVITY_SERVICE);
        NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();

        if (networkInfo != null && networkInfo.isConnected()) {
            return true;
        } else
            return false;
    }
    
}
