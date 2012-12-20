package com.seafile.seadroid;

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLEncoder;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.json.JSONArray;
import org.json.JSONObject;

import android.util.Log;

/**
 * SeafConnection encapsulates Seafile Web API
 * @author plt
 */
public class SeafConnection {
    
    private static final String DEBUG_TAG = "SeafConnection";
    
    private static Hashtable<String, SeafConnection> seafConnections
        = new Hashtable<String, SeafConnection>();
    
    public static synchronized SeafConnection getSeafConnection(String server) {
        SeafConnection sc = seafConnections.get(server);
        if (sc == null) {
            sc = new SeafConnection(server);
            seafConnections.put(server, sc);
            return sc;
        }
        return sc;
    }
    
    // The full URL of the server, like 'http://gonggeng.org/seahub/'
    private String server;
    
    private boolean loginOK = false;
    private String token = null;
    
    private SeafConnection(String server) {
        if (!server.endsWith("/"))
            this.server = server + "/";
        else
            this.server = server;
    }
    
    public boolean isLogin() {
        return loginOK;
    }
	
    
    private HttpURLConnection prepareGet(String apiPath, boolean withToken)
            throws IOException {
        URL url = new URL(server + apiPath);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setReadTimeout(10000);
        conn.setConnectTimeout(15000);
    
        conn.setRequestMethod("GET");
        conn.setDoInput(true);
        if (withToken)
            conn.addRequestProperty("Authorization", "Token " + token);
        
        return conn;
    }
    
    private HttpURLConnection prepareGet(String apiPath) throws IOException {
        return prepareGet(apiPath, true);
    }
    
    private  HttpURLConnection preparePost(String apiPath, boolean withToken)
            throws IOException {
        URL url = new URL(server + apiPath);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setReadTimeout(10000 /* milliseconds */);
        conn.setConnectTimeout(15000 /* milliseconds */);
        
        conn.setRequestMethod("POST");
        conn.setDoInput(true);
        conn.setDoOutput(true);
        if (withToken)
            conn.addRequestProperty("Authorization", "Token " + token);
        return conn;
    }
    
    private  HttpURLConnection preparePost(String apiPath) throws IOException {
        return preparePost(apiPath, true);
    }
    
    private String encodePostParams(List<NameValuePair> params)
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
    
    private void doPost(HttpURLConnection conn, List<NameValuePair> params) 
            throws IOException, ProtocolException, UnsupportedEncodingException {  
        OutputStream os = conn.getOutputStream();
        BufferedWriter writer = new BufferedWriter(
            new OutputStreamWriter(os, "UTF-8"));
        writer.write(encodePostParams(params));
        writer.close();
        os.close();
        
        conn.connect();
    }
    
    
    /**
     * Login into the server
     * @param username
     * @param passwd
     * @return true if login success, false otherwise
     * @throws IOException
     */
    public boolean doLogin(String username, String passwd) {
        InputStream is = null;

        try {
            HttpURLConnection conn = preparePost("api2/auth-token/", false);

            Log.d(DEBUG_TAG, "Login to " + server + "api2/auth-token/");
            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("username", username));
            params.add(new BasicNameValuePair("password", passwd));
            doPost(conn, params);

            if (conn.getResponseCode() != 200) {
                loginOK = false;
                token = null;
                return false;
            }

            is = conn.getInputStream();
            String contentAsString = readIt(is);
            JSONObject obj = Utils.parseJsonObject(contentAsString);
            token = obj.getString("token");
            loginOK = true;
            return true;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return false;
        } finally {
            // Makes sure that the InputStream is closed after the app is
            // finished using it.
            try {
                if (is != null) {
                    is.close();
                }
            } catch (Exception e) {
                // ignore
            }
        }
    }
    
    public boolean authPing() {
        InputStream is = null;
        try {
            
            HttpURLConnection conn = prepareGet("api2/auth/ping/");
            
            // Starts the query
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                return false;
            
            is = conn.getInputStream();
            String result = readIt(is);
            if (result.equals("\"pong\""))
                return true;
            else
                return false;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return false;
        } finally {
            try {
                if (is != null) {
                    is.close();
                }
            } catch (Exception e) {
                // ignore
            }
        }
    }
    
    public boolean ping() {
        InputStream is = null;
        try {     
            HttpURLConnection conn = prepareGet("api2/ping/");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                return false;
            
            is = conn.getInputStream();
            String result = readIt(is);
            if (result.equals("\"pong\""))
                return true;
            else
                return false;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return false;
        } finally {
            try {
                if (is != null) {
                    is.close();
                }
            } catch (Exception e) {
                // ignore
            }
        }
    }
    
    
    public List<SeafRepo> getRepos() {
        InputStream is = null;
        try {
            HttpURLConnection conn = prepareGet("api2/repos/");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                return null;
            
            is = conn.getInputStream();
            String result = readIt(is);
            JSONArray array = Utils.parseJsonArray(result);
            ArrayList<SeafRepo> repos = new ArrayList<SeafRepo>();
            for (int i = 0; i < array.length(); i++) {
                JSONObject obj = array.getJSONObject(i);
                SeafRepo repo = SeafRepo.fromJson(obj);
                if (repo != null)
                    repos.add(repo);
            }
            return repos;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return null;
        } finally {
            try {
                if (is != null) {
                    is.close();
                }
            } catch (Exception e) {
                // ignore
            }
        }
    }
    
    public List<SeafDirent> getDirents(String repo_id, String path) {
        InputStream is = null;
        try {
            HttpURLConnection conn = prepareGet("api2/repos/" + repo_id + "/dirents/" + "?p=" + path);
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200) {
                Log.d(DEBUG_TAG, "Wrong response " + response);
                return null;
            }
            
            is = conn.getInputStream();
            String result = readIt(is);
            JSONArray array = Utils.parseJsonArray(result);
            ArrayList<SeafDirent> dirents = new ArrayList<SeafDirent>();
            for (int i = 0; i < array.length(); i++) {
                JSONObject obj = array.getJSONObject(i);
                SeafDirent de = SeafDirent.fromJson(obj);
                if (de != null)
                    dirents.add(de);
            }
            return dirents;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return null;
        } finally {
            try {
                if (is != null) {
                    is.close();
                }
            } catch (Exception e) {
                // ignore
            }
        }
    }
    
    private String readIt(InputStream stream) throws IOException,
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

}
