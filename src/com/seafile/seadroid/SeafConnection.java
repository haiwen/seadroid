package com.seafile.seadroid;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileOutputStream;
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

import com.seafile.seadroid.account.Account;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.data.DataManager.ProgressMonitor;

import android.content.Context;
import android.util.Log;

/**
 * SeafConnection encapsulates Seafile Web API
 * @author plt
 */
public class SeafConnection {
    
    private static final String DEBUG_TAG = "SeafConnection";

    private Account account;
    
    public SeafConnection(Account act) {
        account = act;
    }
    
    public Account getAccount() {
        return account;
    }
    
    private HttpURLConnection prepareGet(String apiPath, boolean withToken)
            throws IOException {
        URL url = new URL(account.server + apiPath);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setReadTimeout(30000);
        conn.setConnectTimeout(15000);
    
        conn.setRequestMethod("GET");
        conn.setDoInput(true);
        if (withToken)
            conn.addRequestProperty("Authorization", "Token " + account.token);
        
        Log.d(DEBUG_TAG, "get from " + url.getPath());
        return conn;
    }
    
    private HttpURLConnection prepareGet(String apiPath) throws IOException {
        return prepareGet(apiPath, true);
    }
    
    private HttpURLConnection prepareFileGet(String urlString)
            throws IOException {
        URL url = new URL(urlString);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setConnectTimeout(15000);
        conn.setRequestMethod("GET");
        conn.setDoInput(true);
        return conn;
    }
    
    private  HttpURLConnection preparePost(String apiPath, boolean withToken)
            throws IOException {
        URL url = new URL(account.server + apiPath);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setReadTimeout(10000 /* milliseconds */);
        conn.setConnectTimeout(15000 /* milliseconds */);
        
        conn.setRequestMethod("POST");
        conn.setDoInput(true);
        conn.setDoOutput(true);
        if (withToken)
            conn.addRequestProperty("Authorization", "Token " + account.token);
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
     * @return true if login success, false otherwise
     * @throws IOException
     */
    public boolean doLogin() {
        InputStream is = null;

        try {
            HttpURLConnection conn = preparePost("api2/auth-token/", false);

            Log.d(DEBUG_TAG, "Login to " + account.server + "api2/auth-token/");
            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("username", account.email));
            params.add(new BasicNameValuePair("password", account.passwd));
            doPost(conn, params);

            if (conn.getResponseCode() != 200) {
                return false;
            }

            is = conn.getInputStream();
            String contentAsString = Utils.readIt(is);
            JSONObject obj = Utils.parseJsonObject(contentAsString);
            account.token = obj.getString("token");
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
            String result = Utils.readIt(is);
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
            String result = Utils.readIt(is);
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
    
    
    public String getRepos() {
        Log.d(DEBUG_TAG, "get repos from server");
        InputStream is = null;
        try {
            HttpURLConnection conn = prepareGet("api2/repos/");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                return null;
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            return result;
        } catch (Exception e) {
            //Log.d(DEBUG_TAG, e.getMessage());
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
    
    public String getDirents(String repoID, String path) {
        InputStream is = null;
        try {
            String encPath = URLEncoder.encode(path, "UTF-8");
            HttpURLConnection conn = prepareGet("api2/repos/" + repoID + "/dir/" + "?p=" + encPath);
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200) {
                Log.d(DEBUG_TAG, "Wrong response " + response);
                return null;
            }
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            return result;
        } catch (Exception e) {
            // Log.d(DEBUG_TAG, e.getMessage());
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
    
    private String getDownloadLink(String repoID, String path) {
        InputStream is = null;
        try {
            String encPath = URLEncoder.encode(path, "UTF-8");
            HttpURLConnection conn = prepareGet("api2/repos/" + repoID + "/file/" + "?p=" 
                    + encPath + "&op=download");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200) {
                Log.d(DEBUG_TAG, "Wrong response " + response);
                return null;
            }
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
            if (result.startsWith("\"http")) {
                return result.substring(1, result.length()-1);
            } else
                return null;
        } catch (Exception e) {
            String msg = e.getMessage();
            if (msg != null)
                Log.d(DEBUG_TAG, msg);
            else
                Log.d(DEBUG_TAG, "download error");
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
    
    public File getFile(String repoID, String path, String oid, ProgressMonitor monitor) {
        String dlink = getDownloadLink(repoID, path);
        if (dlink == null)
            return null;
       
        File file = DataManager.getFileForFileCache(path, oid);
        
        InputStream is = null;
        OutputStream os = null;
        try {
            int i = dlink.lastIndexOf('/');
            String quoted = dlink.substring(0, i) + "/" + 
                    URLEncoder.encode(dlink.substring(i+1), "UTF-8");
            HttpURLConnection conn = prepareFileGet(quoted);
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200) {
                Log.d(DEBUG_TAG, "Wrong response " + response + " to url " + dlink);
                return null;
            }
            
            File tmp = DataManager.getTempFile(path, oid);
            Log.d(DEBUG_TAG, "write to " + tmp.getAbsolutePath());
            
            is = conn.getInputStream();
            os = new FileOutputStream(tmp);
            long nextUpdate = System.currentTimeMillis() + 1000;
            long total = 0;
            byte[] data = new byte[1024];
            while (true) {
                int len = is.read(data, 0, 1024);
                if (Thread.currentThread().isInterrupted())
                    return null;
                
                if (len == -1)
                    break;
                os.write(data, 0, len);
                total += len;
                if (monitor != null)
                    if (monitor.isCancelled())
                        return null;
                    
                if (System.currentTimeMillis() > nextUpdate) {
                    if (monitor != null) monitor.onProgressNotify(total);
                    nextUpdate = System.currentTimeMillis() + 1000;
                }
            }
            
            if (tmp.renameTo(file) == false) {
                Log.d(DEBUG_TAG, "Rename file error");
                return null;
            }
            return file;
        } catch (Exception e) {
            // Log.d(DEBUG_TAG, e.getMessage());
            return null;
        } finally {
            try {
                if (is != null)
                    is.close();
                if (os != null)
                    os.close();
            } catch (Exception e) {
                // ignore
            }
        }
    }
    
}
