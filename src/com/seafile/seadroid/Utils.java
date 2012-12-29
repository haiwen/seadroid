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

import org.json.*;

class Utils {

    public static JSONObject parseJsonObjectInArray0(String json) {
        try {
            JSONArray array = (JSONArray) new JSONTokener(json).nextValue();
            return array.getJSONObject(0);
        } catch (Exception e) {
            return null;
        }
    }
    
    public static JSONObject parseJsonObject(String json) throws JSONException {
    	return (JSONObject) new JSONTokener(json).nextValue();
    }
    
    public static JSONArray parseJsonArray(String json) throws JSONException {
        return (JSONArray) new JSONTokener(json).nextValue();
    }
    
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
    
    public static String fileNameFromPath(String path) {
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
    
}
