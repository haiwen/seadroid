package com.seafile.seadroid2;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Build;
import android.provider.Settings.Secure;
import android.util.Log;
import android.util.Pair;

import com.github.kevinsawicki.http.HttpRequest;
import com.github.kevinsawicki.http.HttpRequest.HttpRequestException;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.Block;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.FileBlocks;
import com.seafile.seadroid2.data.ProgressMonitor;
import com.seafile.seadroid2.ssl.SSLTrustManager;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.UnsupportedEncodingException;
import java.net.HttpURLConnection;
import java.net.URLEncoder;
import java.util.List;
import java.util.Map;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLHandshakeException;

/**
 * SeafConnection encapsulates Seafile Web API
 * @author plt
 */
public class SeafConnection {
    public static final int HTTP_STATUS_REPO_PASSWORD_REQUIRED = 440;

    private static final String DEBUG_TAG = "SeafConnection";
    private static final int CONNECTION_TIMEOUT = 15000;
    private static final int READ_TIMEOUT = 30000;

    private Account account;

    public SeafConnection(Account act) {
        account = act;
    }

    public Account getAccount() {
        return account;
    }

    private HttpRequest prepareApiGetRequest(String apiPath, Map<String, ?> params) throws IOException {
        HttpRequest req = HttpRequest.get(account.server + apiPath, params, false);
        setRequestCommon(req);
        return req;
    }

    private HttpRequest prepareApiPutRequest(String apiPath, Map<String, ?> params) throws IOException {
        HttpRequest req = HttpRequest.put(account.server + apiPath, params, false);
        setRequestCommon(req);
        return req;
    }

    private void setRequestCommon(HttpRequest req) {
        req.readTimeout(READ_TIMEOUT)
            .connectTimeout(CONNECTION_TIMEOUT)
            .followRedirects(true)
            .header("Authorization", "Token " + account.token);

        prepareHttpsCheck(req);
    }

    private HttpRequest prepareHttpsCheck(HttpRequest req) {
        HttpURLConnection conn = req.getConnection();
        if (conn instanceof HttpsURLConnection) {
            // Tell HttpRequest to trust all hosts, and then the user will get a dialog
            // where he needs to confirm the SSL certificate for the account,
            // and the accepted certificate will be stored, so he is not prompted to accept later on.
            // This is handled by SSLTrustManager and CertsManager
            req.trustAllHosts();
            HttpsURLConnection sconn = (HttpsURLConnection)conn;
            sconn.setSSLSocketFactory(SSLTrustManager.instance().getSSLSocketFactory(account));
        }

        return req;
    }

    private HttpRequest prepareApiGetRequest(String apiPath) throws IOException {
        return prepareApiGetRequest(apiPath, null);
    }

    private HttpRequest prepareApiFileGetRequest(String url) throws HttpRequestException {
        HttpRequest req =  HttpRequest.get(url).connectTimeout(CONNECTION_TIMEOUT).followRedirects(true);

        return prepareHttpsCheck(req);
    }

    /** Prepare a post request.
     *  @param apiPath The path of the http request
     *  @param withToken
     *  @param params The query param to be appended to the request url
     *  @throws IOException
     */
    private HttpRequest prepareApiPostRequest(String apiPath, boolean withToken, Map<String, ?> params)
                                            throws HttpRequestException {
        return prepareApiPostRequest(apiPath, withToken, params, false);
    }

    /** Prepare a post request.
     *  @param apiPath The path of the http request
     *  @param withToken
     *  @param params The query param to be appended to the request url
     *  @param encode true to encode the full URL
     *  @throws IOException
     */
    private HttpRequest prepareApiPostRequest(String apiPath, boolean withToken, Map<String, ?> params, boolean encode)
                                            throws HttpRequestException {
        HttpRequest req = HttpRequest.post(account.server + apiPath, params, encode)
            .followRedirects(true)
            .connectTimeout(CONNECTION_TIMEOUT);

        if (withToken) {
            req.header("Authorization", "Token " + account.token);
        }

        return prepareHttpsCheck(req);
    }

    private HttpRequest prepareApiDeleteRequest(String apiPath, Map<String, ?> params)
            throws HttpRequestException {
        HttpRequest req = HttpRequest.delete(account.server + apiPath, params, false)
            .followRedirects(true)
            .connectTimeout(CONNECTION_TIMEOUT);

        req.header("Authorization", "Token " + account.token);

        return prepareHttpsCheck(req);
    }

    /**
     * Login into the server
     * @return true if login success, false otherwise
     * @throws SeafException
     */
    private boolean realLogin(String passwd) throws SeafException {
        HttpRequest req = null;
        try {
            req = prepareApiPostRequest("api2/auth-token/", false, null);
            // Log.d(DEBUG_TAG, "Login to " + account.server + "api2/auth-token/");

            req.form("username", account.email);
            req.form("password", passwd);

            String appVersion = "";
            Context context = SeadroidApplication.getAppContext();
            try {
                PackageInfo pInfo = context.getPackageManager().
                    getPackageInfo(context.getPackageName(), 0);
                appVersion = pInfo.versionName;
            } catch (NameNotFoundException e) {
                // ignore
            }

            String deviceId = Secure.getString(context.getContentResolver(),
                                               Secure.ANDROID_ID);

            req.form("platform", "android");
            req.form("device_id", deviceId);
            req.form("device_name", Build.MODEL);
            req.form("client_version", appVersion);
            req.form("platform_version", Build.VERSION.RELEASE);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String contentAsString = new String(req.bytes(), "UTF-8");
            JSONObject obj = Utils.parseJsonObject(contentAsString);
            if (obj == null)
                return false;
            account.token = obj.getString("token");
            return true;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            e.printStackTrace();
            throw SeafException.networkException;
        } catch (JSONException e) {
            throw SeafException.illFormatException;
        }
    }

    /**
     * <p>
     *     get Account info, which consists of three fields, usage, total and email.
     * </p>
     * use GET to send HTTP request.
     *
     * @return
     * @throws SeafException
     */
    public String getAccountInfo() throws SeafException {

        String result;
        try {
            HttpRequest req = prepareApiGetRequest("api2/account/info/");
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);
            result = new String(req.bytes(), "UTF-8");
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }

        return result;
    }

    public String getServerInfo() throws SeafException {

        String result;
        try {
            HttpRequest  req = prepareApiGetRequest("api2/server-info/");
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);
            result = new String(req.bytes(), "UTF-8");
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }
        return result;
    }

    public boolean doLogin(String passwd) throws SeafException {
        try {
            return realLogin(passwd);
        } catch (Exception e) {
            // do again
            return realLogin(passwd);
        }
    }

    public String getRepos() throws SeafException {
        HttpRequest req = null;
        try {
            req = prepareApiGetRequest("api2/repos/");
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String result = new String(req.bytes(), "UTF-8");
            return result;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }
    }

    public String getEvents(int start) throws SeafException {
        try {
            String apiPath = String.format("api2/events/");

            Map<String, Object> params = Maps.newHashMap();
            params.put("start", start);
            HttpRequest req = prepareApiGetRequest(apiPath, params);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String result = new String(req.bytes(), "UTF-8");
            return result;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }
    }

    public String getHistoryChanges(String repoID, String commitId) throws SeafException {
        try {
            String apiPath = String.format("api2/repo_history_changes/%s/", repoID);
            Map<String, Object> params = Maps.newHashMap();
            params.put("commit_id", commitId);
            HttpRequest req = prepareApiGetRequest(apiPath, params);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String result = new String(req.bytes(), "UTF-8");

            return result;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }
    }

    public String getStarredFiles() throws SeafException {
        try {
            HttpRequest req = prepareApiGetRequest("api2/starredfiles/");
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String result = new String(req.bytes(), "UTF-8");
            return result;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }
    }
    public String getAvatar(String email, int size) throws SeafException  {
        try {
            String apiPath = String.format("api2/avatars/user/%s/resized/%d", email, size);
            HttpRequest req = prepareApiGetRequest(apiPath);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);
            
            String result = new String(req.bytes(), "UTF-8");
            return result;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }
    }

    public String searchLibraries(String query, int page) throws SeafException {

        try {
            Map<String, Object> params = Maps.newHashMap();
            params.put("q", encodeUriComponent(query));

            if (page > 0)
                params.put("per_page", page);

            HttpRequest req = prepareApiGetRequest("api2/search/", params);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);
            String result = new String(req.bytes(), "UTF-8");
            return result;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }

    }

    private static String encodeUriComponent(String src) throws UnsupportedEncodingException {
        return URLEncoder.encode(src, "UTF-8");
    }

    /**
     * Get the contents of a directory.
     * @param repoID
     * @param path
     * @param cachedDirID The local cached dirID.
     * @return A non-null Pair of (dirID, content). If the local cache is up to date, the "content" is null.
     * @throws SeafException
     */
    public Pair<String, String> getDirents(String repoID, String path, String cachedDirID)
                                        throws SeafException {
        try {
            String apiPath = String.format("api2/repos/%s/dir/", repoID);
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent(path));
            if (cachedDirID != null) {
                params.put("oid", cachedDirID);
            }
            HttpRequest req = prepareApiGetRequest(apiPath, params);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String dirID = req.header("oid");
            String content;
            if (dirID == null) {
                throw SeafException.unknownException;
            }

            if (dirID.equals(cachedDirID)) {
                // local cache is valid
                // Log.d(DEBUG_TAG, String.format("dir %s is cached", path));
                content = null;
            } else {
                /*Log.d(DEBUG_TAG,
                      String.format("dir %s will be downloaded from server, latest %s, local cache %s",
                                    path, dirID, cachedDirID != null ? cachedDirID : "null"));*/
                byte[] rawBytes = req.bytes();
                if (rawBytes == null) {
                    throw SeafException.unknownException;
                }
                content = new String(rawBytes, "UTF-8");
            }

            return new Pair<String, String>(dirID, content);

        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (IOException e) {
            throw SeafException.networkException;
        }
    }

    private Pair<String, String> getDownloadLink(String repoID, String path) throws SeafException {
        try {
            String apiPath = String.format("api2/repos/%s/file/", repoID);
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent(path));
            params.put("op", "download");
            HttpRequest req = prepareApiGetRequest(apiPath, params);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String result = new String(req.bytes(), "UTF-8");
            String fileID = req.header("oid");

            // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
            if (result.startsWith("\"http") && fileID != null) {
                String url = result.substring(1, result.length() - 1);
                return new Pair<String, String>(url, fileID);
            } else {
                throw SeafException.illFormatException;
            }
        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            throw SeafException.networkException;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    public String getBlockDownloadList(String repoID, String path) throws SeafException, IOException {
        try {
            String apiPath = String.format("api2/repos/%s/file/", repoID);
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent(path));
            params.put("op", "downloadblks");
            HttpRequest req = prepareApiGetRequest(apiPath, params);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String result = new String(req.bytes(), "UTF-8");
            return result;
        } catch (SeafException | IOException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    /**
     * get file server link for downloading a block
     *
     * @param repoID
     * @param fileId
     * @param blockId
     * @return
     * @throws SeafException
     * @throws IOException
     */
    private String getBlockDownloadLink(String repoID, String fileId, String blockId) throws SeafException, IOException {
        try {
            String apiPath = String.format("api2/repos/%s/files/%s/blks/%s/download-link/", repoID, fileId, blockId);
            HttpRequest req = prepareApiGetRequest(apiPath, null);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            return new String(req.bytes(), "UTF-8");
        } catch (SeafException | IOException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    /**
     * Get the latest version of the file from server
     * @param repoID
     * @param fileBlocks
     * @param blockId
     * @param localPath
     * @param monitor
     * @return A two tuple of (fileID, file). If the local cached version is up to date, the returned file is null.
     */
    public Pair<String, File> getBlock(String repoID,
                                       FileBlocks fileBlocks,
                                       String blockId,
                                       String localPath,
                                       ProgressMonitor monitor) throws SeafException, IOException, JSONException {

        String dlink = getBlockDownloadLink(repoID, fileBlocks.fileID, blockId).replaceAll("\"", "");

        File block = getBlockFromLink(dlink, fileBlocks, blockId, localPath, monitor);
        if (block != null) {
            return new Pair<>(blockId, block);
        } else {
            throw SeafException.unknownException;
        }
    }

    public String uploadByBlocks(String repoID, String dir, String filePath, List<Block> blocks, ProgressMonitor monitor) throws IOException, SeafException {
        try {
            String url = getUploadLink(repoID, false, true);
            Log.d(DEBUG_TAG, "UploadLink " + url);
            return uploadBlocksCommon(url, repoID, dir, filePath, blocks, monitor, false);
        } catch (SeafException e) {
            // do again
            String url = getUploadLink(repoID, false, true);
            Log.d(DEBUG_TAG, "do again UploadLink " + url);
            return uploadBlocksCommon(url, repoID, dir, filePath, blocks, monitor, false);
        }
    }

    public String updateByBlocks(String repoID, String dir, String filePath, List<Block> blocks, ProgressMonitor monitor) throws IOException, SeafException {
        try {
            String url = getUploadLink(repoID, true, true);
            Log.d(DEBUG_TAG, "UpdateLink " + url);
            return uploadBlocksCommon(url, repoID, dir, filePath, blocks, monitor, true);
        } catch (SeafException e) {
            // do again
            String url = getUploadLink(repoID, true, true);
            Log.d(DEBUG_TAG, "do again UpdateLink " + url);
            return uploadBlocksCommon(url, repoID, dir, filePath, blocks, monitor, true);
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
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            if (monitor != null) {
                /*if (req.header(HttpRequest.HEADER_CONTENT_LENGTH) == null) {
                    throw SeafException.illFormatException;
                }
                Long size = Long.parseLong(req.header(HttpRequest.HEADER_CONTENT_LENGTH));*/
                if (req.contentLength() > 0) {
                    Long size =  Long.valueOf(req.contentLength());
                    monitor.onProgressNotify(size, false, false);
                }
            }

            File tmp = DataManager.createTempFile();
            // Log.d(DEBUG_TAG, "write to " + tmp.getAbsolutePath());
            if (monitor == null) {
                req.receive(tmp);
            } else {
                req.bufferSize(MonitoredFileOutputStream.BUFFER_SIZE);
                req.receive(new MonitoredFileOutputStream(tmp, monitor));
            }

            if (!tmp.renameTo(file)) {
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
                // Log.d(DEBUG_TAG, "download is cancelled");
                throw SeafException.userCancelledException;
            } else {
                throw getSeafExceptionFromHttpRequestException(e);
            }
        }
    }

    private File getBlockFromLink(String dlink, FileBlocks fileBlocks, String blkId, String localPath, ProgressMonitor monitor)
                                    throws SeafException {
        if (dlink == null)
            return null;

        try {

            HttpRequest req = prepareApiFileGetRequest(dlink);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            if (monitor != null) {
                /*if (req.header(HttpRequest.HEADER_CONTENT_LENGTH) == null) {
                    throw SeafException.illFormatException;
                }
                Long size = Long.parseLong(req.header(HttpRequest.HEADER_CONTENT_LENGTH));*/
                if (req.contentLength() > 0) {
                    fileBlocks.getBlock(blkId).size = req.contentLength();
                    fileBlocks.decrease();
                    monitor.onProgressNotify(fileBlocks.getSize(), true, fileBlocks.remaining());
                }
            }

            File block = new File(localPath);
            // Log.d(DEBUG_TAG, "write to " + block.getAbsolutePath());
            if (monitor == null) {
                req.receive(block);
            } else {
                req.bufferSize(DataManager.BUFFER_SIZE);
                req.receive(new MonitoredFileOutputStream(fileBlocks, blkId, block, monitor));
            }

            return block;

        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            e.printStackTrace();
            throw SeafException.networkException;
        } catch (HttpRequestException e) {
            if (e.getCause() instanceof MonitorCancelledException) {
                // Log.d(DEBUG_TAG, "download is cancelled");
                throw SeafException.userCancelledException;
            } else {
                throw getSeafExceptionFromHttpRequestException(e);
            }
        }
    }

    /**
     * Get the latest version of the file from server
     * @param repoID
     * @param path
     * @param localPath
     * @param cachedFileID The file id of the local cached version
     * @param monitor
     * @return A two tuple of (fileID, file). If the local cached version is up to date, the returned file is null.
     */
    public Pair<String, File> getFile(String repoID,
                                          String path,
                                          String localPath,
                                          String cachedFileID,
                                          ProgressMonitor monitor) throws SeafException {
        Pair<String, String> ret = getDownloadLink(repoID, path);
        String dlink = ret.first;
        String fileID = ret.second;

        if (fileID.equals(cachedFileID)) {
            // cache is valid
            // Log.d(DEBUG_TAG, String.format("file %s is cached", path));
            return new Pair<String, File>(fileID, null);
        } else {
            /*Log.d(DEBUG_TAG,
                  String.format("file %s will be downloaded from server, latest %s, local cache %s",
                                path, fileID, cachedFileID != null ? cachedFileID : "null"));*/

            File file = getFileFromLink(dlink, path, localPath, fileID, monitor);
            if (file != null) {
                return new Pair<String, File>(fileID, file);
            } else {
                throw SeafException.unknownException;
            }
        }
    }

    // set password for an encrypted repo
    public void setPassword(String repoID, String passwd) throws SeafException {
        try {
            HttpRequest req = prepareApiPostRequest("api2/repos/" + repoID + "/", true, null);

            req.form("password", passwd);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);
        } catch (SeafException e) {
            Log.d(DEBUG_TAG, "Set Password err: " + e.getCode());
            throw e;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, "Exception in setPassword ");
            e.printStackTrace();
            return;
        }
    }

    private String getUploadLink(String repoID, boolean update) throws SeafException {
        return getUploadLink(repoID, update, false);
    }

    private String getUploadLink(String repoID, boolean update, boolean byblock) throws SeafException {
        try {
            String apiPath;
            if (update) {
                if (byblock) {
                    apiPath = "api2/repos/" + repoID + "/upload-";
                } else {
                    apiPath = "api2/repos/" + repoID + "/update-";
                }
            } else {
                apiPath = "api2/repos/" + repoID + "/upload-";
            }

            if (byblock) {
                apiPath = apiPath + "blks-link/";
            } else {
                apiPath = apiPath + "link/";
            }

            HttpRequest req = prepareApiGetRequest(apiPath);
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String result = new String(req.bytes(), "UTF-8");
            // should return "\"http://gonggeng.org:8082/...\"" or "\"https://gonggeng.org:8082/...\"
            if (result.startsWith("\"http")) {
                // remove the starting and trailing quote
                return result.substring(1, result.length()-1);
            } else
                throw SeafException.unknownException;
        } catch (SeafException e) {
            Log.d(DEBUG_TAG, e.getCode() + e.getMessage());
            throw e;
        } catch (Exception e) {
            String msg = e.getMessage();
            if (msg != null)
                Log.d(DEBUG_TAG, msg);
            else
                Log.d(DEBUG_TAG, "get upload link error", e);
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
    public String uploadFile(String repoID, String dir, String filePath, ProgressMonitor monitor)
                            throws SeafException {
        try {
            String url = getUploadLink(repoID, false);
            return uploadFileCommon(url, repoID, dir, filePath, monitor, false);
        } catch (SeafException e) {
            // do again
            String url = getUploadLink(repoID, false);
            return uploadFileCommon(url, repoID, dir, filePath, monitor, false);
        }
    }

    private static final String CRLF = "\r\n";
    private static final String TWO_HYPENS = "--";
    private static final String BOUNDARY = "----SeafileAndroidBound$_$";

    /**
     * Upload a file to seafile httpserver
     */
    private String uploadFileCommon(String link, String repoID, String dir,
                                     String filePath, ProgressMonitor monitor, boolean update)
                                        throws SeafException {

        try {
            File file = new File(filePath);
            if (!file.exists()) {
                throw new SeafException(SeafException.OTHER_EXCEPTION, "File not exists");
            }


            HttpRequest req = HttpRequest.post(link).followRedirects(true).connectTimeout(CONNECTION_TIMEOUT);

            prepareHttpsCheck(req);

            /**
             * We have to set the content-length header, otherwise the whole
             * request would be buffered by android. So we have to format the
             * multipart form-data request ourselves in order to calculate the
             * content length.
             */
            int totalLen = 0;
            byte[] dirParam = {};
            byte[] targetFileParam = {};
            StringBuilder builder;

            if (update) {
                // the "target_file" param is for update file api
                builder = new StringBuilder();
                // line 1, ------SeafileAndroidBound$_$
                builder.append(TWO_HYPENS + BOUNDARY + CRLF);
                // line 2
                builder.append("Content-Disposition: form-data; name=\"target_file\"" + CRLF);
                // line 3, an empty line
                builder.append(CRLF);
                String targetFilePath = Utils.pathJoin(dir, file.getName());
                // line 4
                builder.append(targetFilePath + CRLF);
                targetFileParam = builder.toString().getBytes("UTF-8");
                totalLen += targetFileParam.length;
            } else {
                // the "parent_dir" param is for upload file api
                builder = new StringBuilder();
                // line 1, ------SeafileAndroidBound$_$
                builder.append(TWO_HYPENS + BOUNDARY + CRLF);
                // line 2
                builder.append("Content-Disposition: form-data; name=\"parent_dir\"" + CRLF);
                // line 3, an empty line
                builder.append(CRLF);
                // line 4
                builder.append(dir + CRLF);
                dirParam = builder.toString().getBytes("UTF-8");
                totalLen += dirParam.length;
            }

            // line 1
            String l1 = TWO_HYPENS + BOUNDARY + CRLF;
            // line 2,
            String contentDisposition = "Content-Disposition: form-data; name=\"file\";filename=\"" + file.getName() + "\"" + CRLF;
            byte[] l2 = contentDisposition.getBytes("UTF-8");
            // line 3
            String l3 = "Content-Type: text/plain" + CRLF;
            // line 4
            String l4 = CRLF;
            totalLen += l1.length() + l2.length + l3.length() + l4.length() + file.length() + 2;

            String end = TWO_HYPENS + BOUNDARY + TWO_HYPENS + CRLF;
            totalLen += end.length();

            req.contentLength(totalLen);
            req.header("Connection", "close");
            req.header("Cache-Control", "no-cache");
            req.header("Content-Type", "multipart/form-data;boundary=" + BOUNDARY);

            if (update) {
                req.send(targetFileParam);
            } else {
                req.send(dirParam);
            }
            req.send(l1);
            req.send(l2);
            req.send(l3);
            req.send(l4);

            if (monitor != null) {
                req.bufferSize(MonitoredFileInputStream.BUFFER_SIZE);
                req.send(new MonitoredFileInputStream(file, monitor));
            } else {
                req.send(new FileInputStream(file));
            }

            req.send(CRLF);
            req.send(end);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            return new String(req.bytes(), "UTF-8");
        } catch (IOException e) {
            throw SeafException.networkException;
        } catch (HttpRequestException e) {
            if (e.getCause() instanceof MonitorCancelledException) {
                Log.d(DEBUG_TAG, "upload is cancelled");
                throw SeafException.userCancelledException;
            } else {
                throw getSeafExceptionFromHttpRequestException(e);
            }
        }
    }

    /**
     * Upload file blocks to server
     */
    private String uploadBlocksCommon(String link, String repoID, String dir,
                                      String filePath, List<Block> blocks,
                                      ProgressMonitor monitor, boolean update)
                                        throws SeafException {

        try {
            File file = new File(filePath);
            if (!file.exists()) {
                throw new SeafException(SeafException.OTHER_EXCEPTION, "File not exists");
            }

            HttpRequest req = HttpRequest.post(link).followRedirects(true).connectTimeout(CONNECTION_TIMEOUT);

            prepareHttpsCheck(req);

            /**
             * We have to set the content-length header, otherwise the whole
             * request would be buffered by android. So we have to format the
             * multipart form-data request ourselves in order to calculate the
             * content length.
             */
            int totalLen = 0;
            byte[] dirParam = {};
            StringBuilder updateBuilder = new StringBuilder();
            if (update) {
                // line 1, ------SeafileAndroidBound$_$
                updateBuilder.append(TWO_HYPENS + BOUNDARY + CRLF);
                // line 2
                updateBuilder.append("Content-Disposition: form-data; name=\"replace\"" + CRLF);
                // line 3
                updateBuilder.append(CRLF);
                // line 4
                updateBuilder.append("1" + CRLF);
                dirParam = updateBuilder.toString().getBytes("UTF-8");
                totalLen += dirParam.length;
            }

            StringBuilder parentDirBuilder = new StringBuilder();
            // line 1, ------SeafileAndroidBound$_$
            parentDirBuilder.append(TWO_HYPENS + BOUNDARY + CRLF);
            // line 2
            parentDirBuilder.append("Content-Disposition: form-data; name=\"parent_dir\"" + CRLF);
            // line 3
            parentDirBuilder.append(CRLF);
            // line 4
            parentDirBuilder.append(dir + CRLF);
            totalLen += parentDirBuilder.toString().getBytes("UTF-8").length;

            StringBuilder fileNameBuilder = new StringBuilder();
            // line 1, ------SeafileAndroidBound$_$
            fileNameBuilder.append(TWO_HYPENS + BOUNDARY + CRLF);
            // line 2
            fileNameBuilder.append("Content-Disposition: form-data; name=\"file_name\"" + CRLF);
            // line 3
            fileNameBuilder.append(CRLF);
            // line 4
            fileNameBuilder.append(file.getName() + CRLF);
            totalLen += fileNameBuilder.toString().getBytes("UTF-8").length;

            StringBuilder fileSizeBuilder = new StringBuilder();
            // line 1, ------SeafileAndroidBound$_$
            fileSizeBuilder.append(TWO_HYPENS + BOUNDARY + CRLF);
            // line 2
            fileSizeBuilder.append("Content-Disposition: form-data; name=\"file_size\"" + CRLF);
            // line 3
            fileSizeBuilder.append(CRLF);
            // line 4
            fileSizeBuilder.append(file.length() + CRLF);
            totalLen += fileSizeBuilder.toString().getBytes("UTF-8").length;

            for (Block block : blocks) {
                // line 1
                String l1 = TWO_HYPENS + BOUNDARY + CRLF;

                File blk = new File(block.path);

                // line 2
                String contentDisposition = "Content-Disposition: form-data; name=\"file\";filename=\"" + blk.getName() + "\"" + CRLF;
                byte[] l2 = contentDisposition.getBytes("UTF-8");

                // line 3
                String l3 = "Content-Type: text/plain" + CRLF;

                // line 4
                String l4 = CRLF;
                totalLen += l1.length() + l2.length + l3.length() + l4.length() + blk.length() + 2;
            }

            String end = TWO_HYPENS + BOUNDARY + TWO_HYPENS + CRLF;
            totalLen += end.getBytes().length;

            req.contentLength(totalLen);
            req.header("Connection", "Keep-Alive");
            req.header("Cache-Control", "no-cache");
            req.header("Content-Type", "multipart/form-data;boundary=" + BOUNDARY);

            if (update) {
                req.send(updateBuilder);
                Log.d(DEBUG_TAG, updateBuilder.toString());
            }

            req.send(parentDirBuilder);
            req.send(fileNameBuilder);
            req.send(fileSizeBuilder);

            for (Block block : blocks) {
                // line 1
                String l1 = TWO_HYPENS + BOUNDARY + CRLF;

                File blk = new File(block.path);

                // line 2
                String contentDisposition = "Content-Disposition: form-data; name=\"file\";filename=\"" + blk.getName() + "\"" + CRLF;
                byte[] l2 = contentDisposition.getBytes("UTF-8");

                // line 3
                String l3 = "Content-Type: text/plain" + CRLF;

                // line 4
                String l4 = CRLF;
                totalLen += l1.length() + l2.length + l3.length() + l4.length() + blk.length() + 2;

                StringBuilder chunkReq = new StringBuilder();
                chunkReq.append(l1).append(contentDisposition).append(l3).append(l4);
                req.send(chunkReq);

                if (monitor != null) {
                    req.bufferSize(MonitoredFileInputStream.BUFFER_SIZE);
                    req.send(new MonitoredFileInputStream(blk, monitor));
                } else {
                    req.send(new FileInputStream(blk));
                }
                req.send(CRLF);
            }

            req.send(end);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            //result file_id "3f0da9a0709c5fb9f23957608dabef01becc3a8c"
            return new String(req.bytes(), "UTF-8").replaceAll("\"","");
        } catch (IOException e) {
            throw SeafException.networkException;
        } catch (HttpRequestException e) {
            if (e.getCause() instanceof MonitorCancelledException) {
                Log.d(DEBUG_TAG, "upload is cancelled");
                throw SeafException.userCancelledException;
            } else {
                throw getSeafExceptionFromHttpRequestException(e);
            }
        }
    }

    public Pair<String, String> createNewDir(String repoID,
                                                 String parentDir,
                                                 String dirName) throws SeafException {

        HttpRequest req = null;
        try {
            String fullPath = Utils.pathJoin(parentDir, dirName);
            final String encodeUriComponent = encodeUriComponent(fullPath).replaceAll("\\+", "%20");
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent);
            params.put("reloaddir", "true");

            req = prepareApiPostRequest("api2/repos/" + repoID + "/dir/", true, params, false);

            req.form("operation", "mkdir");

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String newDirID = req.header("oid");
            if (newDirID == null) {
                return null;
            }

            String content = new String(req.bytes(), "UTF-8");
            if (content.length() == 0) {
                return null;
            }

            return new Pair<String, String>(newDirID, content);

        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    public Pair<String, String> createNewFile(String repoID,
                                                  String parentDir,
                                                  String fileName) throws SeafException {

        try {
            String fullPath = Utils.pathJoin(parentDir, fileName);
            final String encodeUriComponent = encodeUriComponent(fullPath).replaceAll("\\+", "%20");
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent);
            params.put("reloaddir", "true");

            HttpRequest req = prepareApiPostRequest("api2/repos/" + repoID + "/file/", true, params, false);

            req.form("operation", "create");

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String newDirID = req.header("oid");
            if (newDirID == null) {
                return null;
            }

            String content = new String(req.bytes(), "UTF-8");
            if (content.length() == 0) {
                return null;
            }

            return new Pair<String, String>(newDirID, content);
        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }


    /**
     * Wrap a FileInputStream in a upload task. We publish the progress of the upload during the process, and if we detect the task has been cancelled by the user, we throw a {@link MonitorCancelledException} to indicate such a situation.
     */
    private class MonitoredFileInputStream extends InputStream {
        public static final int BUFFER_SIZE = 1024;


        private static final long PROGRESS_UPDATE_INTERVAL = 1000;
        private ProgressMonitor monitor;
        private InputStream src;
        private long bytesRead = 0;
        private long nextUpdate = System.currentTimeMillis() + PROGRESS_UPDATE_INTERVAL;

        public MonitoredFileInputStream(File file, ProgressMonitor monitor) throws IOException {
            this.src = new FileInputStream(file);
            this.monitor = monitor;
        }

        @Override
        public int read(byte[] buffer) throws IOException {
            int read = src.read(buffer);
            if (read != -1) {
                bytesRead += read;
            }

            checkMonitor();

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

        @Override
        public void close() throws IOException {
            src.close();
        }

        private void checkMonitor() throws MonitorCancelledException {
            if (monitor.isCancelled() ||
                Thread.currentThread().isInterrupted()) {
                throw new MonitorCancelledException();
            }

            if (System.currentTimeMillis() > nextUpdate) {
                monitor.onProgressNotify(bytesRead, false, false);
                nextUpdate = System.currentTimeMillis() + PROGRESS_UPDATE_INTERVAL;
            }
        }
    }

    /**
     * Wrap a FileOutputStream in a download task. We publish the upload progress during the process, and if we detect the task has been cancelled by the user, we throw a {@link MonitorCancelledException} to indicate such a situation.
     */
    private class MonitoredFileOutputStream extends OutputStream {
        public static final int BUFFER_SIZE = 4096;

        private static final long PROGRESS_UPDATE_INTERVAL = 500;
        private ProgressMonitor monitor;
        private OutputStream dst;
        private long bytesWritten = 0;
        private long nextUpdate = System.currentTimeMillis() + PROGRESS_UPDATE_INTERVAL;

        private FileBlocks fileBlocks;
        private String blockId;

        public MonitoredFileOutputStream(File file, ProgressMonitor monitor) throws IOException {
            this.dst = new FileOutputStream(file);
            this.monitor = monitor;
        }

        public MonitoredFileOutputStream(FileBlocks fileBlocks, String blockId, File file, ProgressMonitor monitor) throws IOException {
            this.dst = new FileOutputStream(file);
            this.monitor = monitor;
            if (fileBlocks != null) {
                this.fileBlocks = fileBlocks;
                this.blockId = blockId;
            }
        }

        @Override
        public void write(byte[] buffer, int off, int len) throws IOException {
            dst.write(buffer, off, len);
            bytesWritten += len;
            checkMonitor();
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

        @Override
        public void close() throws IOException {
            dst.close();
        }

        private void checkMonitor() throws MonitorCancelledException {
            if (monitor.isCancelled() ||
                Thread.currentThread().isInterrupted()) {
                throw new MonitorCancelledException();
            }

            if (System.currentTimeMillis() > nextUpdate) {
                if (fileBlocks != null) {
                    fileBlocks.getBlock(blockId).finished = bytesWritten;
                    monitor.onProgressNotify(fileBlocks.getFinished(), false, fileBlocks.remaining());
                } else {
                    monitor.onProgressNotify(bytesWritten, false, false);
                }
                nextUpdate = System.currentTimeMillis() + PROGRESS_UPDATE_INTERVAL;
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

    public String getShareLink(String repoID, String path, boolean isdir) throws SeafException {
        try {
            String apiPath = String.format("api2/repos/%s/file/shared-link/", repoID);
            HttpRequest req = prepareApiPutRequest(apiPath, null);
            req.form("p", path);
            req.form("type", isdir ? "d": "f");
            checkRequestResponseStatus(req, HttpURLConnection.HTTP_CREATED);

            String result = req.header("Location");
            if (result == null) {
                throw SeafException.illFormatException;
            }
            return result;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (IOException e) {
            throw SeafException.networkException;
        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    public void star(String repoID, String path) throws SeafException {
        try {
            HttpRequest req = prepareApiPostRequest("api2/starredfiles/", true, null);

            req.form("repo_id", repoID);
            req.form("p", path);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_CREATED);

        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    public void unstar(String repoID, String path) throws SeafException {
        try {
            Map<String, Object> params = Maps.newHashMap();
            params.put("repo_id", repoID);
            params.put("p", path);
            HttpRequest req = prepareApiDeleteRequest("api2/starredfiles/", params);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    public Pair<String, String> rename(String repoID, String path,
                                       String newName, boolean isdir) throws SeafException {
        try {
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", path);
            params.put("reloaddir", "true");
            String suffix = isdir ? "/dir/" : "/file/";
            HttpRequest req = prepareApiPostRequest("api2/repos/" + repoID + suffix, true, params);

            req.form("operation", "rename");
            req.form("newname", newName);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String newDirID = req.header("oid");
            if (newDirID == null) {
                return null;
            }

            String content = new String(req.bytes(), "UTF-8");
            if (content.length() == 0) {
                return null;
            }

            return new Pair<String, String>(newDirID, content);
        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    public Pair<String, String> delete(String repoID, String path,
               boolean isdir) throws SeafException {
        try {
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent(path).replaceAll("\\+", "%20"));
            params.put("reloaddir", "true");
            String suffix = isdir ? "/dir/" : "/file/";
            HttpRequest req = prepareApiDeleteRequest("api2/repos/" + repoID + suffix, params);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String newDirID = req.header("oid");
            if (newDirID == null) {
                return null;
            }

            String content = new String(req.bytes(), "UTF-8");
            if (content.length() == 0) {
                return null;
            }

                return new Pair<String, String>(newDirID, content);
            } catch (SeafException e) {
                throw e;
            } catch (UnsupportedEncodingException e) {
                throw SeafException.encodingException;
            } catch (HttpRequestException e) {
                throw getSeafExceptionFromHttpRequestException(e);
            }
    }

    /**
     * Copy a file or multiple files, multiple file/folder names should be seperated by a ":".
     *
     * @param srcRepoId the source repo id
     * @param srcDir the source folder in src_repo
     * @param srcFn list of file/folder names to copy. Multiple file/folder names can be seperated by ":"
     * @param dstRepoId the destination repo id
     * @param dstDir the destination folder in dst_repo
     * @throws SeafException
     */
    public void copy(String srcRepoId, String srcDir, String srcFn,
                     String dstRepoId, String dstDir) throws SeafException {
        try {
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent(srcDir).replaceAll("\\+", "%20"));

            HttpRequest req = prepareApiPostRequest("api2/repos/" + srcRepoId + "/fileops/copy/", true, params);

            req.form("dst_repo", dstRepoId);
            req.form("dst_dir", dstDir);
            req.form("file_names", srcFn);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        }
    }

    /**
     * Move multiple files
     *
     * @param srcRepoId the source repo id
     * @param srcDir the source folder in src_repo
     * @param srcFn list of file/folder names to move. Multiple file/folder names can be seperated by ":"
     * @param dstRepoId the destination repo id
     * @param dstDir the destination folder in dst_repo
     * @throws SeafException
     */
    public void move(String srcRepoId, String srcDir, String srcFn,
                     String dstRepoId, String dstDir) throws SeafException {
        try {
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", srcDir);

            HttpRequest req = prepareApiPostRequest("api2/repos/" + srcRepoId + "/fileops/move/", true, params);

            req.form("dst_repo", dstRepoId);
            req.form("dst_dir", dstDir);
            req.form("file_names", srcFn);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

        } catch (SeafException e) {
            throw e;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    /**
     * Move a single file
     *
     * @param srcRepoId the source repo id
     * @param srcPath the source file path
     * @param dstRepoId the destination repo id
     * @param dstDir the destination folder in dst_repo
     * @return
     * @throws SeafException
     */
    public Pair<String, String> move(String srcRepoId, String srcPath, String dstRepoId, String dstDir) throws SeafException {
        try {
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent(srcPath).replaceAll("\\+", "%20"));
            params.put("reloaddir", "true");
            String suffix = "/file/";
            HttpRequest req = prepareApiPostRequest("api2/repos/" + srcRepoId + suffix, true, params);

            req.form("operation", "move");
            req.form("dst_repo", dstRepoId);
            req.form("dst_dir", dstDir);

            checkRequestResponseStatus(req, HttpURLConnection.HTTP_OK);

            String newDirID = req.header("oid");
            if (newDirID == null) {
                return null;
            }

            String content = new String(req.bytes(), "UTF-8");
            if (content.length() == 0) {
                return null;
            }

            return new Pair<String, String>(newDirID, content);
        } catch (SeafException e) {
            throw e;
        } catch (UnsupportedEncodingException e) {
            throw SeafException.encodingException;
        } catch (HttpRequestException e) {
            throw getSeafExceptionFromHttpRequestException(e);
        }
    }

    private void checkRequestResponseStatus(HttpRequest req, int expectedStatusCode) throws SeafException {
        if (req.code() != expectedStatusCode) {
            Log.d(DEBUG_TAG, "HTTP request failed : " + req.url() + ", " + req.code() + ", " + req.message());

            if (req.message() == null) {
                throw SeafException.networkException;
            } else {
                throw new SeafException(req.code(), req.message());
            }
        }
        else {
            // Log.v(DEBUG_TAG, "HTTP request ok : " + req.url());
        }
    }

    private SeafException getSeafExceptionFromHttpRequestException(HttpRequestException e) {
        if (e.getCause() instanceof SSLHandshakeException) {
            return SeafException.sslException;
        } else {
            return SeafException.networkException;
        }
    }
}
