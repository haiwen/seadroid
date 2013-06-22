package com.seafile.seadroid2;

import java.io.BufferedWriter;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.ProtocolException;
import java.net.URL;
import java.net.URLEncoder;
import java.security.KeyManagementException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLException;
import javax.net.ssl.SSLSession;

import org.apache.http.NameValuePair;
import org.apache.http.message.BasicNameValuePair;
import org.json.JSONException;
import org.json.JSONObject;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DataManager.ProgressMonitor;

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

    class MyHostnameVerifier implements javax.net.ssl.HostnameVerifier {
        public boolean verify(String urlHostName, String certHostName){
            return true;
        }

        public boolean verify(String urlHost, SSLSession sslSession){
            return true;
        }
    }

    private void prepareSSL(HttpURLConnection conn, boolean secure) {
        if (conn instanceof HttpsURLConnection) {
            try {
                HttpsURLConnection sc = (HttpsURLConnection) conn;
                SSLContext context;

                context = SSLContext.getInstance("TLS");
                // if (secure)
                //     context.init(null, TrustManagerFactory.getTrustManagers(), null);
                // else {
                //     MyHostnameVerifier verifier = new MyHostnameVerifier();
                //     sc.setHostnameVerifier(verifier);
                //     context.init(null, TrustManagerFactory.getUnsecureTrustManagers(), null);
                // }

                /**
                 * XXX: For convience, all SSL ceritificates are trusted
                 */
                MyHostnameVerifier verifier = new MyHostnameVerifier();
                sc.setHostnameVerifier(verifier);
                context.init(null, TrustManagerFactory.getUnsecureTrustManagers(), null);

                sc.setSSLSocketFactory(context.getSocketFactory());
            } catch (NoSuchAlgorithmException e) {
                e.printStackTrace();
            } catch (KeyManagementException e) {
                e.printStackTrace();
            }
        }
    }

    private HttpURLConnection prepareGet(String apiPath, boolean withToken)
            throws IOException {
        URL url = new URL(account.server + apiPath);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        prepareSSL(conn, true);
        conn.setReadTimeout(30000);
        conn.setConnectTimeout(15000);

        conn.setRequestMethod("GET");
        conn.setDoInput(true);
        if (withToken)
            conn.addRequestProperty("Authorization", "Token " + account.token);

        //Log.d(DEBUG_TAG, "get from " + url.getPath());
        return conn;
    }

    private HttpURLConnection prepareGet(String apiPath) throws IOException {
        return prepareGet(apiPath, true);
    }

    private HttpURLConnection prepareFileGet(String urlString)
            throws IOException {
        URL url = new URL(urlString);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        prepareSSL(conn, false);
        conn.setConnectTimeout(15000);
        conn.setRequestMethod("GET");
        conn.setDoInput(true);
        return conn;
    }

    private  HttpURLConnection preparePost(String apiPath, boolean withToken)
            throws IOException {
        URL url = new URL(account.server + apiPath);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        prepareSSL(conn, true);

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
        OutputStream os = null;
        try {
            os = conn.getOutputStream();
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(
                    os, "UTF-8"));
            writer.write(encodePostParams(params));
            writer.close(); // must call this to actually write to the output
            conn.connect();
        } finally {
            try {
                if (os != null)
                    os.close();
            } catch (Exception e) {
                // ignore
            }
        }
    }


    /**
     * Login into the server
     * @return true if login success, false otherwise
     * @throws IOException
     */
    private boolean realLogin() throws SeafException {
        InputStream is = null;

        try {
            HttpURLConnection conn = preparePost("api2/auth-token/", false);

            Log.d(DEBUG_TAG, "Login to " + account.server + "api2/auth-token/");
            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("username", account.email));
            params.add(new BasicNameValuePair("password", account.passwd));
            doPost(conn, params);

            if (conn.getResponseCode() != 200) {
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(conn.getResponseCode(), conn.getResponseMessage());
            }
            
            is = conn.getInputStream();
            String contentAsString = Utils.readIt(is);
            JSONObject obj = Utils.parseJsonObject(contentAsString);
            if (obj == null)
                return false;
            account.token = obj.getString("token");
            return true;
        } catch (SeafException e) {
            throw e;
        } catch (SSLException e) {
            //Log.d("test", e.getMessage());
            throw SeafException.sslException;
        } catch (IOException e) {
            e.printStackTrace();
            throw SeafException.networkException;
        } catch (JSONException e) {
            throw SeafException.illFormatException;
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
    
    public boolean doLogin() throws SeafException {
        try {
            return realLogin();
        } catch (Exception e) {
            // do again
            return realLogin();
        }
    }

    public boolean authPing() throws SeafException {
        InputStream is = null;
        try {

            HttpURLConnection conn = prepareGet("api2/auth/ping/");

            // Starts the query
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200) {
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(response, conn.getResponseMessage());
            }

            is = conn.getInputStream();
            String result = Utils.readIt(is);
            if (result.equals("\"pong\""))
                return true;
            else
                return false;
        } catch (SeafException e) {
            throw e;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, "Other exception in authPing");
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
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
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
            Log.d(DEBUG_TAG, "Other exception in ping");
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
        InputStream is = null;
        try {
            HttpURLConnection conn = prepareGet("api2/repos/");
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(response, conn.getResponseMessage());

            is = conn.getInputStream();
            String result = Utils.readIt(is);
            return result;
        } catch (SeafException e) {
            throw e;
        } catch (IOException e) {
            throw SeafException.networkException;
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
            if (response != 200)
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(response, conn.getResponseMessage());

            is = conn.getInputStream();
            String result = Utils.readIt(is);
            return result;
        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            throw SeafException.networkException;
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
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
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
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            throw SeafException.networkException;
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
    
    private File getFileFromLink(String dlink, String path, String localPath, 
                String oid, ProgressMonitor monitor)
            throws SeafException {
        if (dlink == null)
            return null;

        File file = new File(localPath);

        InputStream is = null;
        OutputStream os = null;
        HttpURLConnection conn = null;
        try {
            int i = dlink.lastIndexOf('/');
            String quoted = dlink.substring(0, i) + "/" +
                    URLEncoder.encode(dlink.substring(i+1), "UTF-8");
            conn = prepareFileGet(quoted);
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200)
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(response, conn.getResponseMessage());
                
            File tmp = DataManager.getTempFile(path, oid);
            // Log.d(DEBUG_TAG, "write to " + tmp.getAbsolutePath());

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
                Log.w(DEBUG_TAG, "Rename file error");
                return null;
            }
            return file;
        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            e.printStackTrace();
            throw SeafException.networkException;
        } finally {
            if (conn != null)
                conn.disconnect();
            // do not need to close is and os
        }
    }

    public File getFile(String repoID, String path, String localPath, String oid, ProgressMonitor monitor)
            throws SeafException {
        try {
            String dlink = getDownloadLink(repoID, path);
            return getFileFromLink(dlink, path, localPath, oid, monitor);
        } catch (SeafException e) {
            // do again
            String dlink = getDownloadLink(repoID, path);
            return getFileFromLink(dlink, path, localPath, oid, monitor);
        }
    }

    // set password for an encrypted repo
    public void setPassword(String repoID, String passwd) throws SeafException {
        try {
            HttpURLConnection conn = preparePost("api2/repos/" + repoID + "/", true);

            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("password", passwd));
            doPost(conn, params);
            if (conn.getResponseCode() != 200)
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(conn.getResponseCode(), conn.getResponseMessage());
        } catch (SeafException e) {
            Log.d(DEBUG_TAG, "Set Password err: " + e.getCode());
            throw e;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, "Exception in setPassword ");
            e.printStackTrace();
            return;
        }
    }

    private String getUploadLink(String repoID, boolean isUpdate) throws SeafException {
        InputStream is = null;
        try {
            String url;
            if (isUpdate) {
                url = "api2/repos/" + repoID + "/update-link/";
            } else {
                url = "api2/repos/" + repoID + "/upload-link/";
            }
            HttpURLConnection conn = prepareGet(url);
            conn.connect();
            int response = conn.getResponseCode();
            if (response != 200) {
                Log.d("Upload", "Failed to get upload link " + response);
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(conn.getResponseCode(), conn.getResponseMessage());
            }
                
            is = conn.getInputStream();
            String result = Utils.readIt(is);
            // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
            if (result.startsWith("\"http")) {
                // remove the starting and trailing quote
                return result.substring(1, result.length()-1);
            } else
                throw SeafException.unknownException;
        } catch (SeafException e) {
            throw e;
        } catch (Exception e) {
            String msg = e.getMessage();
            if (msg != null)
                Log.d(DEBUG_TAG, msg);
            else
                Log.d(DEBUG_TAG, "get upload link error");
            throw SeafException.unknownException;
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

    /**
     * Upload a file to update an existing file
     */
    public String updateFile(String repoID, String dir, String filePath, ProgressMonitor monitor)
                                throws SeafException {
        try {
            String url = getUploadLink(repoID, true);
            return uploadFileCommon(url, repoID, dir, filePath, monitor, true);
        } catch (SeafException e) {
            // do again
            String url = getUploadLink(repoID, true);
            return uploadFileCommon(url, repoID, dir, filePath, monitor, true);
        }
    }

    /**
     * Upload a new file
     */
    public void uploadFile(String repoID, String dir, String filePath, ProgressMonitor monitor)
                            throws SeafException {
        try {
            String url = getUploadLink(repoID, false);
            uploadFileCommon(url, repoID, dir, filePath, monitor, false);
        } catch (SeafException e) {
            // do again
            String url = getUploadLink(repoID, false);
            uploadFileCommon(url, repoID, dir, filePath, monitor, false);
        }
    }

    private String uploadFileCommon(String link, String repoID, String dir,
                                    String filePath, ProgressMonitor monitor, boolean isUpdate)
                                        throws SeafException {
        DataOutputStream request = null;
        HttpURLConnection conn = null;
        File file = new File(filePath);
        if (!file.exists())
            throw new SeafException(SeafException.OTHER_EXCEPTION, "File not exists");

        try {
            URL url = new URL(link);
            Log.d(DEBUG_TAG, "Upload to " + link);

            conn = (HttpURLConnection) url.openConnection();
            prepareSSL(conn, false);
            conn.setConnectTimeout(15000);
            conn.setRequestMethod("POST");
            //conn.setChunkedStreamingMode(0);

            int totalLen = 0;

            // write the parent dir
            StringBuilder builder = new StringBuilder();
            builder.append(this.twoHyphens + this.boundary + this.crlf);  // line 1, ------SeafileAndroidBound$_$
            builder.append("Content-Disposition: form-data; name=\"parent_dir\"" + this.crlf); // line 2
            builder.append(this.crlf); // line 3, an empty line
            builder.append(dir + this.crlf); // line 4
            byte[] dirParam = builder.toString().getBytes("UTF-8");
            totalLen += dirParam.length;

            byte[] targetFileParam = {};
            if (isUpdate) {
                builder = new StringBuilder();
                builder.append(this.twoHyphens + this.boundary + this.crlf);  // line 1, ------SeafileAndroidBound$_$
                builder.append("Content-Disposition: form-data; name=\"target_file\"" + this.crlf); // line 2
                builder.append(this.crlf); // line 3, an empty line
                String targetFilePath = Utils.pathJoin(dir, file.getName());
                builder.append(targetFilePath + this.crlf); // line 4
                targetFileParam = builder.toString().getBytes("UTF-8");
                totalLen += targetFileParam.length;
            }

            String l1 = this.twoHyphens + this.boundary + this.crlf; // line 1
            byte[] l2 = new String("Content-Disposition: form-data; name=\"file\";filename=\""
                    + file.getName() + "\"" + this.crlf).getBytes("UTF-8"); // line 2,
            String l3 = "Content-Type: text/plain" + this.crlf; // line 3
            String l4 = this.crlf; // line 4
            totalLen += l1.length() + l2.length + l3.length() + l4.length() + file.length() + 2;


            String end = this.twoHyphens + this.boundary + this.twoHyphens + this.crlf;
            totalLen += end.length();

            Log.d(DEBUG_TAG, "Total len is " + totalLen);
            conn.setFixedLengthStreamingMode(totalLen);
            conn.setDoInput(true);
            conn.setDoOutput(true);
            // disable keep-alive, otherwise read the returned file ID will be failed.
            conn.setRequestProperty("Connection", "close");
            conn.setRequestProperty("Cache-Control", "no-cache");
            conn.setRequestProperty("Content-Type", "multipart/form-data;boundary=" + this.boundary);

            request = new DataOutputStream(conn.getOutputStream());
            request.write(dirParam);
            if (isUpdate) {
                request.write(targetFileParam);
            }
            request.writeBytes(l1);
            request.write(l2);
            request.writeBytes(l3);
            request.writeBytes(l4);
            FileInputStream in = new FileInputStream(file);
            int total = 0;
            long nextUpdate = System.currentTimeMillis() + 1000;
            while (true) {
                byte[] buffer = new byte[4096];
                int len = in.read(buffer, 0, 4096);
                if (len == -1)
                    break;
                request.write(buffer, 0, len);
                total += len;
                if (System.currentTimeMillis() > nextUpdate) {
                    if (monitor != null) monitor.onProgressNotify(total);
                    nextUpdate = System.currentTimeMillis() + 1000;
                    request.flush(); // seems to have to call this to prevent buffer in android 2.2
                }
            }
            request.writeBytes(this.crlf);
            request.writeBytes(end);

            request.flush();
            request.close();
            Log.d(DEBUG_TAG, "finish write");

            // if we use https, only when we read input the data will be sent out
            InputStream is = conn.getInputStream();
            if (!isUpdate) {
                is.close();
                return null;
            } else {
                String new_file_id = Utils.readIt(is);
                is.close();
                return new_file_id;
            }
        } catch (Exception e) {
            e.printStackTrace();
            String msg = e.getMessage();
            if (msg != null)
                Log.d(DEBUG_TAG, msg);
            else
                msg = "";
            throw new SeafException(SeafException.OTHER_EXCEPTION, msg);
        } finally {
            if (conn != null) conn.disconnect();
        }
    }

    public List<String> createNewDir(String repoID,
                                     String parentDir,
                                     String dirName) throws SeafException {

        InputStream is = null;
        HttpURLConnection conn = null;
        try {
            String fullPath = Utils.pathJoin(parentDir, dirName);
            String encPath = URLEncoder.encode(fullPath, "UTF-8");
            conn = preparePost("api2/repos/" + repoID +
                               "/dir/" + "?p=" + encPath +
                               "&reloaddir=true");

            List<NameValuePair> params = new ArrayList<NameValuePair>();
            params.add(new BasicNameValuePair("operation", "mkdir"));
            doPost(conn, params);

            int response = conn.getResponseCode();
            if (response != 200)
                if (conn.getResponseMessage() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(response, conn.getResponseMessage());

            String newDirID = conn.getHeaderField("oid");
            if (newDirID == null) {
                return null;
            }

            is = conn.getInputStream();
            String content = Utils.readIt(is);
            if (content == null) {
                return null;
            }
            
            List<String> ret = new ArrayList<String>();
            ret.add(newDirID);
            ret.add(content);
            return ret;

        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            throw SeafException.networkException;
        } finally {
            if (is != null) {
                try {
                    is.close();
                } catch (Exception e) {
                }
            }

            if (conn != null) {
                conn.disconnect();
            }
        }
    }
}