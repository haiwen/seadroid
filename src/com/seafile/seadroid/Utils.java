package com.seafile.seadroid;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.text.DecimalFormat;

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
    
    public static String fileNameFromPath(String path) {
        return path.substring(path.lastIndexOf("/") + 1);
    }
    
    public static String readableFileSize(long size) {
        if(size <= 0) return "0";
        final String[] units = new String[] { "B", "KB", "MB", "GB", "TB" };
        int digitGroups = (int) (Math.log10(size)/Math.log10(1024));
        return new DecimalFormat("#,##0.#").format(size/Math.pow(1024, digitGroups)) + " " + units[digitGroups];
    }
    
}
