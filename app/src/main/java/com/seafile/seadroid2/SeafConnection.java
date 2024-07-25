package com.seafile.seadroid2;

import android.content.Context;
import android.content.pm.PackageInfo;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Build;
import android.text.TextUtils;
import android.util.Log;
import android.util.Pair;

import com.github.kevinsawicki.http.HttpRequest;
import com.github.kevinsawicki.http.HttpRequest.HttpRequestException;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.ApiUrls;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.data.FileBlocks;
import com.seafile.seadroid2.framework.data.ProgressMonitor;
import com.seafile.seadroid2.ssl.SSLTrustManager;
import com.seafile.seadroid2.framework.util.DeviceIdManager;
import com.seafile.seadroid2.framework.util.Utils;

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
import java.util.Map;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLHandshakeException;

import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

/**
 * SeafConnection encapsulates Seafile Web API
 *
 * @author plt
 */
public class SeafConnection {
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
            HttpsURLConnection sconn = (HttpsURLConnection) conn;
            sconn.setSSLSocketFactory(SSLTrustManager.instance().getSSLSocketFactory(account));
        }

        return req;
    }

    private static String encodeUriComponent(String src) throws UnsupportedEncodingException {
        return URLEncoder.encode(src, "UTF-8");
    }

    public Pair<String, String> getDownloadLink(String repoID, String path, boolean isReUsed) throws SeafException {
        try {
            String apiPath = String.format("api2/repos/%s/file/", repoID);
            Map<String, Object> params = Maps.newHashMap();
            params.put("p", encodeUriComponent(path));
            params.put("op", "download");
            if (isReUsed) {
                params.put("reuse", 1);
            }
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


    public String getReUsedFileLink(String repoID, String path) throws SeafException {
        //Setting up links can be reused
        Pair<String, String> ret = getDownloadLink(repoID, path, true);
        return ret.first;
    }

    private void checkRequestResponseStatus(HttpRequest req, int expectedStatusCode) throws SeafException {
        if (req.code() != expectedStatusCode) {
            Log.d(DEBUG_TAG, "HTTP request failed : " + req.url() + ", " + req.code() + ", " + req.message());

            if (req.message() == null) {
                throw SeafException.networkException;
            } else if (req.code() == HttpURLConnection.HTTP_UNAUTHORIZED) {
                String wiped = req.header("X-Seafile-Wiped");
                if (wiped != null) {
                    throw SeafException.remoteWipedException;
                } else {
                    throw new SeafException(req.code(), req.message());
                }
            } else {
                try {
                    String result = new String(req.bytes(), "UTF-8");
                    if (result != null && Utils.parseJsonObject(result) != null) {
                        JSONObject json = Utils.parseJsonObject(result);
                        if (json.has("detail")) {
                            throw new SeafException(req.code(), json.optString("detail"));
                        }
                        throw new SeafException(req.code(), json.optString("error_msg"));
                    } else {
                        throw new SeafException(req.code(), req.message());
                    }
                } catch (UnsupportedEncodingException e) {
                    e.printStackTrace();
                    throw new SeafException(req.code(), req.message());
                }

            }
        } else {
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
