package com.seafile.seadroid2;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.Map;

import javax.net.ssl.SSLException;

import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

import com.github.kevinsawicki.http.HttpRequest;
import com.github.kevinsawicki.http.HttpRequest.HttpRequestException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DataManager.ProgressMonitor;
import com.seafile.seadroid2.data.TwoTuple;

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

    private HttpRequest prepareApiGetRequest(String apiPath, Map<String, ?> params) throws IOException {
        return HttpRequest.get(account.server + apiPath, params, true).
                    trustAllCerts().trustAllHosts().
                    readTimeout(30000).connectTimeout(15000).
                    header("Authorization", "Token " + account.token);
    }

    private HttpRequest prepareApiGetRequest(String apiPath) throws IOException {
        return prepareApiGetRequest(apiPath, null);
    }

    private HttpRequest prepareApiPostRequest(String apiPath, boolean withToken, Map<String, ?> params)
                                            throws IOException {
        HttpRequest req = HttpRequest.post(account.server + apiPath, params, true).
                            trustAllCerts().trustAllHosts().
                            connectTimeout(15000);

        if (withToken) {
            req.header("Authorization", "Token " + account.token);
        }

        return req;
    }

    private HttpRequest prepareApiFileGetRequest(String url) throws IOException {
        return HttpRequest.get(url).
                trustAllCerts().trustAllHosts().
                connectTimeout(15000);
    }

    /**
     * Login into the server
     * @return true if login success, false otherwise
     * @throws SeafException
     */
    private boolean realLogin() throws SeafException {
        try {
            HttpRequest req = prepareApiPostRequest("api2/auth-token/", false, null);
            Log.d(DEBUG_TAG, "Login to " + account.server + "api2/auth-token/");

            req.form("username", account.email);
            req.form("password", account.passwd);

            if (req.code() != 200) {
                if (req.message() == null) {
                    throw SeafException.networkException;
                } else  {
                    throw new SeafException(req.code(), req.message());
                }
            }

            String contentAsString = new String(req.bytes(), "UTF-8");
            JSONObject obj = Utils.parseJsonObject(contentAsString);
            if (obj == null)
                return false;
            account.token = obj.getString("token");
            return true;
        } catch (SeafException e) {
            throw e;
        } catch (SSLException e) {
            throw SeafException.sslException;
        } catch (HttpRequestException e) {
            throw SeafException.networkException;
        } catch (IOException e) {
            e.printStackTrace();
            throw SeafException.networkException;
        } catch (JSONException e) {
            throw SeafException.illFormatException;
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

    public String getRepos() throws SeafException {
        try {
            HttpRequest req = prepareApiGetRequest("api2/repos/");
            if (req.code() != 200) {
                if (req.message() == null) {
                    throw SeafException.networkException;
                } else  {
                    throw new SeafException(req.code(), req.message());
                }
            }

            String result = new String(req.bytes(), "UTF-8");
            return result;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw SeafException.networkException;
        } catch (IOException e) {
            throw SeafException.networkException;
        }
    }

    public TwoTuple<String, String> getDirents(String repoID, String path) throws SeafException {
        InputStream is = null;
        try {
            String apiPath = String.format("api2/repos/%s/dir/", repoID);
            Map<String, Object> params = new HashMap<String, Object>();
            params.put("p", path);
            HttpRequest req = prepareApiGetRequest(apiPath, params);
            if (req.code() != 200)
                if (req.message() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(req.code(), req.message());

            byte[] rawBytes = req.bytes();
            String content = new String(rawBytes, "UTF-8");
            String dirID = req.header("oid");
            if (content == null || dirID == null) {
                throw SeafException.unknownException;
            }

            return TwoTuple.newInstance(dirID, content);

        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (HttpRequestException e) {
            throw SeafException.networkException;
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
        try {
            String apiPath = String.format("api2/repos/%s/file/", repoID);
            Map<String, Object> params = new HashMap<String, Object>();
            params.put("p", path);
            params.put("op", "download");
            HttpRequest req = prepareApiGetRequest(apiPath, params);
            if (req.code() != 200)
                if (req.message() == null)
                    throw SeafException.networkException;
                else
                    throw new SeafException(req.code(), req.message());

            String result = new String(req.bytes(), "UTF-8");
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
        } catch (HttpRequestException e) {
            throw SeafException.networkException;
        }
    }

    private File getFileFromLink(String dlink, String path, String localPath,
                                 String oid, ProgressMonitor monitor)
                                    throws SeafException {
        if (dlink == null)
            return null;

        File file = new File(localPath);

        try {
            int i = dlink.lastIndexOf('/');
            String quoted = dlink.substring(0, i) + "/" +
                    URLEncoder.encode(dlink.substring(i+1), "UTF-8");

            HttpRequest req = prepareApiFileGetRequest(quoted);
            if (req.code() != 200) {
                if (req.message() == null) {
                    throw SeafException.networkException;
                } else {
                    throw new SeafException(req.code(), req.message());
                }
            }

            File tmp = DataManager.getTempFile(path, oid);
            // Log.d(DEBUG_TAG, "write to " + tmp.getAbsolutePath());
            if (monitor != null) {
                req.receive(tmp);
            } else {
                req.receive(new MonitoredFileOutputStream(tmp, monitor));
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
        } catch (HttpRequestException e) {
            if (e.getCause() instanceof MonitorCancelledException) {
                Log.d(DEBUG_TAG, "download is cancelled by user");
                return null;
            } else {
                throw SeafException.networkException;
            }
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
            HttpRequest req = prepareApiPostRequest("api2/repos/" + repoID + "/", true, null);

            req.form("password", passwd);

            if (req.code() != 200) {
                if (req.message() == null) {
                    throw SeafException.networkException;
                } else {
                    throw new SeafException(req.code(), req.message());
                }
            }
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
        try {
            String apiPath;
            if (isUpdate) {
                apiPath = "api2/repos/" + repoID + "/update-link/";
            } else {
                apiPath = "api2/repos/" + repoID + "/upload-link/";
            }

            HttpRequest req = prepareApiGetRequest(apiPath);
            if (req.code() != 200) {
                Log.d("Upload", "Failed to get upload link " + req.code());
                if (req.message() == null) {
                    throw SeafException.networkException;
                } else {
                    throw new SeafException(req.code(), req.message());
                }
            }

            String result = new String(req.bytes(), "UTF-8");
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
        }
    }

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

        try {
            File file = new File(filePath);
            if (!file.exists()) {
                throw new SeafException(SeafException.OTHER_EXCEPTION, "File not exists");
            }


            HttpRequest req = HttpRequest.post(link).
                                trustAllCerts().trustAllHosts().
                                connectTimeout(15000);

            if (isUpdate) {
                String targetFilePath = Utils.pathJoin(dir, file.getName());
                req.part("target_file", targetFilePath);
            } else {
                req.part("parent_dir", dir);
            }

            String contentType = "text/plain";
            if (monitor != null) {
                req.bufferSize(MonitoredFileInputStream.BUFFER_SIZE);
                req.part("file", file.getName(), contentType,
                         new MonitoredFileInputStream(file, monitor));
            } else {
                req.part("file", file.getName(), contentType,
                         new FileInputStream(file));
            }

            if (req.code() != 200)
                if (req.message() == null) {
                    throw SeafException.networkException;
                } else {
                    throw new SeafException(req.code(), req.message());
                }

            if (isUpdate) {
                return new String(req.bytes(), "UTF-8");
            } else {
                return null;
            }
        } catch (IOException e) {
            throw SeafException.networkException;

        } catch (HttpRequestException e) {
            if (e.getCause() instanceof MonitorCancelledException) {
                Log.d(DEBUG_TAG, "upload is cancelled by user");
                return null;
            } else {
                throw SeafException.networkException;
            }
        }
    }

    public TwoTuple<String, String> createNewDir(String repoID,
                                                 String parentDir,
                                                 String dirName) throws SeafException {

        try {
            String fullPath = Utils.pathJoin(parentDir, dirName);
            Map<String, Object> params = new HashMap<String, Object>();
            params.put("p", fullPath);
            params.put("reloaddir", "true");

            HttpRequest req = prepareApiPostRequest("api2/repos/" + repoID + "/dir/", true, params);

            req.form("operation", "mkdir");

            if (req.code() != 200) {
                if (req.message() == null) {
                    throw SeafException.networkException;
                } else {
                    throw new SeafException(req.code(), req.message());
                }
            }

            String newDirID = req.header("oid");
            if (newDirID == null) {
                return null;
            }

            String content = new String(req.bytes(), "UTF-8");
            if (content.length() == 0) {
                return null;
            }

            return TwoTuple.newInstance(newDirID, content);

        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            throw SeafException.networkException;
        } catch (HttpRequestException e) {
            throw SeafException.networkException;
        }
    }

    private class MonitoredFileInputStream extends InputStream {
        public static final int BUFFER_SIZE = 4096;

        private ProgressMonitor monitor;
        private InputStream src;
        private long bytesRead = 0;
        private long nextUpdate = 0;

        public MonitoredFileInputStream(File file, ProgressMonitor monitor) throws IOException {
            this.src = new FileInputStream(file);
            this.monitor = monitor;
        }

        @Override
        public int read(byte[] buffer) throws IOException {
            int read = src.read(buffer);
            if (read != -1) {
                bytesRead += read;
                checkMonitor();
            }

            return read;
        }

        @Override
        public int read() throws IOException {
            int ret = src.read();
            if (ret != -1) {
                ++bytesRead;
                if (bytesRead % BUFFER_SIZE == 0) {
                    checkMonitor();
                }
            }

            return ret;
        }

        private void checkMonitor() throws MonitorCancelledException {
            if (monitor.isCancelled() ||
                Thread.currentThread().isInterrupted()) {
                throw new MonitorCancelledException();
            }

            if (System.currentTimeMillis() > nextUpdate) {
                monitor.onProgressNotify(bytesRead);
                nextUpdate = System.currentTimeMillis() + 1000;
            }
        }
    }

    private class MonitoredFileOutputStream extends OutputStream {
        public static final int BUFFER_SIZE = 4096;

        private ProgressMonitor monitor;
        private OutputStream dst;
        private long bytesWritten = 0;
        private long nextUpdate = 0;

        public MonitoredFileOutputStream(File file, ProgressMonitor monitor) throws IOException {
            this.dst = new FileOutputStream(file);
            this.monitor = monitor;
        }

        @Override
        public void write(byte[] buffer) throws IOException {
            dst.write(buffer);
            bytesWritten += buffer.length;
            checkMonitor();
        }

        @Override
        public void write(int b) throws IOException {
            dst.write(b);
            ++bytesWritten;
            if (bytesWritten % BUFFER_SIZE == 0) {
                checkMonitor();
            }
        }

        private void checkMonitor() throws MonitorCancelledException {
            if (monitor.isCancelled() ||
                Thread.currentThread().isInterrupted()) {
                throw new MonitorCancelledException();
            }

            if (System.currentTimeMillis() > nextUpdate) {
                monitor.onProgressNotify(bytesWritten);
                nextUpdate = System.currentTimeMillis() + 1000;
            }

        }
    }


    private class MonitorCancelledException extends IOException {
        private static final long serialVersionUID = -1170466989781746232L;

        @Override
        public String toString() {
            return "the upload/download task has been cancelled";
        }
    }
}
