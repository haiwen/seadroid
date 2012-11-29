package com.seafile.seadroid;

import org.apache.http.NameValuePair;
import java.net.URLEncoder;
import java.net.ProtocolException;
import java.util.List;

import java.net.HttpURLConnection;
import java.net.URL;
import java.io.*;

class Utils {
    
    public static String encodePostParams(List<NameValuePair> params)
            throws UnsupportedEncodingException {
        StringBuilder result = new StringBuilder();
        boolean first = true;

        for (NameValuePair pair : params) {
            if (first)
                first = false;
            else
                result.append("&");
            
            result.append(URLEncoder.encode(pair.getName(), "UTF-8"));
            result.append("=");
            result.append(URLEncoder.encode(pair.getValue(), "UTF-8"));
        }

        return result.toString();
    }

    public static void doPost(HttpURLConnection conn, List<NameValuePair> params) 
            throws IOException, ProtocolException, UnsupportedEncodingException {
        conn.setRequestMethod("POST");
        conn.setDoInput(true);
        conn.setDoOutput(true);
        
        OutputStream os = conn.getOutputStream();
        BufferedWriter writer = new BufferedWriter(
            new OutputStreamWriter(os, "UTF-8"));
        writer.write(encodePostParams(params));
        writer.close();
        os.close();
        
        conn.connect();
    }

}
