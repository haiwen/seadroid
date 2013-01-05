package com.seafile.seadroid;

import java.io.BufferedWriter;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Reader;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
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
    public boolean doLogin() throws SeafException {
        InputStream is = null;

        try {
            HttpURLConnection conn = preparePost("api2/auth-token/", false);

            Log.d(DEBUG_TAG, "Login to " + account.server + "api2/auth-token/");
            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("username", account.email));
            params.add(new BasicNameValuePair("password", account.passwd));
            doPost(conn, params);

            if (conn.getResponseCode() != 200)
                throw new SeafException(conn.getResponseCode(), conn.getResponseMessage());

            is = conn.getInputStream();
            String contentAsString = Utils.readIt(is);
            JSONObject obj = Utils.parseJsonObject(contentAsString);
            account.token = obj.getString("token");
            return true;
        } catch (SeafException e) {
            throw e;
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
    
    public boolean authPing() throws SeafException {
        InputStream is = null;
        try {
            
            HttpURLConnection conn = prepareGet("api2/auth/ping/");
            
            // Starts the query
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                throw new SeafException(response, conn.getResponseMessage());
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            if (result.equals("\"pong\""))
                return true;
            else
                return false;
        } catch (SeafException e) {
            throw e;
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
    
    public boolean ping() throws SeafException {
        InputStream is = null;
        try {     
            HttpURLConnection conn = prepareGet("api2/ping/");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                throw new SeafException(response, conn.getResponseMessage());
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            if (result.equals("\"pong\""))
                return true;
            else
                return false;
        } catch (SeafException e) {
            throw e;
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
    
    
    public String getRepos() throws SeafException {
        Log.d(DEBUG_TAG, "get repos from server");
        InputStream is = null;
        try {
            HttpURLConnection conn = prepareGet("api2/repos/");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                throw new SeafException(response, conn.getResponseMessage());
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            return result;
        } catch (SeafException e) {
            throw e;
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
    
    public String getDirents(String repoID, String path) throws SeafException {
        InputStream is = null;
        try {
            String encPath = URLEncoder.encode(path, "UTF-8");
            HttpURLConnection conn = prepareGet("api2/repos/" + repoID + "/dir/" + "?p=" + encPath);
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200) {
                throw new SeafException(response, conn.getResponseMessage());
            }
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            return result;
        } catch (SeafException e) {
            throw e;
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
    
    private String getDownloadLink(String repoID, String path) throws SeafException {
        InputStream is = null;
        try {
            String encPath = URLEncoder.encode(path, "UTF-8");
            HttpURLConnection conn = prepareGet("api2/repos/" + repoID + "/file/" + "?p=" 
                    + encPath + "&op=download");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                throw new SeafException(response, conn.getResponseMessage());
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
            if (result.startsWith("\"http")) {
                return result.substring(1, result.length()-1);
            } else
                return null;
        } catch (SeafException e) {
            throw e;
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
    
    public File getFile(String repoID, String path, String oid, ProgressMonitor monitor) 
            throws SeafException {
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
            if (response != 200)
                throw new SeafException(response, conn.getResponseMessage());
            
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
        } catch (SeafException e) {
            throw e;
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

    // set password for an encrypted repo
    public void setPassword(String repoID, String passwd) throws SeafException {
        try {
            HttpURLConnection conn = preparePost("api2/repos/" + repoID + "/", true);

            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("password", passwd));
            doPost(conn, params);
            if (conn.getResponseCode() != 200) {
                throw new SeafException(conn.getResponseCode(),
                        conn.getResponseMessage());
            }
        } catch (SeafException e) {
            throw e;
        } catch (Exception e) {
            // Log.d(DEBUG_TAG, e.getMessage());
            return;
        }
    }
    
    private String getUploadLink(String repoID) throws SeafException {
        InputStream is = null;
        try {
            HttpURLConnection conn = prepareGet("api2/repos/" + repoID + "/upload-link/");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                throw new SeafException(response, conn.getResponseMessage());
            
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
            if (result.startsWith("\"http")) {
                // remove the starting and trailing quote
                return result.substring(1, result.length()-1);
            } else
                return null;
        } catch (SeafException e) {
            throw e;
        } catch (Exception e) {
            String msg = e.getMessage();
            if (msg != null)
                Log.d(DEBUG_TAG, msg);
            else
                Log.d(DEBUG_TAG, "get upload link error");
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
    
    String attachmentName = "file";
    String crlf = "\r\n";
    String twoHyphens = "--";
    String boundary = "----SeafileAndroidBound$_$";
    
    public void uploadFile(String repoID, String dir, String filePath) throws SeafException {
        String uploadLink = getUploadLink(repoID);
        DataOutputStream request = null;
        HttpURLConnection conn = null;
        File file = new File(filePath);
        if (!file.exists())
            return;
        
        try {
            URL url = new URL(uploadLink + "?X-Progress-ID=randomID123213");
            Log.d(DEBUG_TAG, "Upload to " + uploadLink + "?X-Progress-ID=randomID123213");

            conn = (HttpURLConnection) url.openConnection();
            conn.setConnectTimeout(15000);
            conn.setRequestMethod("POST");
            conn.setDoInput(true);
            conn.setDoOutput(true);
            //conn.setChunkedStreamingMode(0);
            
            Log.d(DEBUG_TAG, "upload to " + dir);
            int totalLen = 0;
            
            // write the parent dir
            StringBuilder builder = new StringBuilder();
            builder.append(this.twoHyphens + this.boundary + this.crlf);  // line 1, ------SeafileAndroidBound$_$
            builder.append("Content-Disposition: form-data; name=\"parent_dir\"" + this.crlf); // line 2
            builder.append(this.crlf); // line 3, an empty line
            builder.append(dir + this.crlf); // line 4
            byte[] dirParam = builder.toString().getBytes("UTF-8");
            totalLen += dirParam.length;
            
            String l1 = this.twoHyphens + this.boundary + this.crlf; // line 1
            byte[] l2 = new String("Content-Disposition: form-data; name=\"file\";filename=\"" 
                    + file.getName() + "\"" + this.crlf).getBytes("UTF-8"); // line 2, 
            String l3 = "Content-Type: text/plain" + this.crlf; // line 3
            String l4 = this.crlf; // line 4
            totalLen += l1.length() + l2.length + l3.length() + l4.length() + file.length() + 2;
            
            
            String end = this.twoHyphens + this.boundary + this.twoHyphens + this.crlf;
            totalLen += end.length();
            
            Log.d(DEBUG_TAG, "content length is " + totalLen);
            conn.setFixedLengthStreamingMode(totalLen);
            conn.setRequestProperty("Connection", "Keep-Alive");
            conn.setRequestProperty("Cache-Control", "no-cache");
            conn.setRequestProperty("Content-Type", "multipart/form-data;boundary=" + this.boundary);
            
            request = new DataOutputStream(conn.getOutputStream());
            request.write(dirParam);
            request.writeBytes(l1);
            request.write(l2);
            request.writeBytes(l3);
            request.writeBytes(l4);
            FileInputStream in = new FileInputStream(file);
            while (true) {
                byte[] buffer = new byte[4096];
                int len = in.read(buffer, 0, 4096);
                if (len == -1)
                    break;
                request.write(buffer, 0, len);
            }
            request.writeBytes(this.crlf);
            request.writeBytes(end);
            
            
            // write file content
            /*
            request.writeBytes(this.twoHyphens + this.boundary + this.crlf);
            request.writeBytes("Content-Disposition: form-data; name=\"" + this.attachmentName + "\";filename=\"" + this.attachmentFileName + "\"" + this.crlf);
            request.writeBytes("Content-Type: text/plain" + this.crlf);
            request.writeBytes(this.crlf);
            request.writeBytes("string test");
            request.writeBytes(this.crlf);
            
            // end
            request.writeBytes(this.twoHyphens + this.boundary + this.twoHyphens + this.crlf);
            */
            
            Log.d(DEBUG_TAG, "finish");
            request.flush();
            request.close();
            
            InputStream is = conn.getInputStream();
            String result = Utils.readIt(is);
            if (result != null)
                Log.d(DEBUG_TAG, "result is " + result);
            is.close();
        } catch (Exception e) {
            return;
        } finally {
            if (conn != null) conn.disconnect();
        }
    }
    
}
