package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;

import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;

import org.json.JSONObject;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.security.cert.CertificateException;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;
import javax.net.ssl.SSLPeerUnverifiedException;

import okhttp3.ResponseBody;
import retrofit2.HttpException;
import retrofit2.Response;

public class ExceptionUtils {
    public static SeafException getExceptionByThrowable(Throwable throwable) {
        if (throwable == null) {
            return SeafException.UNKNOWN_EXCEPTION;
        }

        if (throwable instanceof SeafException) {
            return (SeafException) throwable;
        }

        if (throwable instanceof HttpException httpException) {

            Response<?> resp = httpException.response();

            if (resp != null) {
                String wiped = resp.headers().get("X-Seafile-Wiped");
                if (!TextUtils.isEmpty(wiped)) {
                    return SeafException.REMOTE_WIPED_EXCEPTION;
                }
            }

            //401
            if (HttpURLConnection.HTTP_UNAUTHORIZED == httpException.code()) {
                return SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION;
            }

            //504
            if (HttpURLConnection.HTTP_GATEWAY_TIMEOUT == httpException.code()) {
                if (NetworkUtils.isConnected()) {
                    ToastUtils.showLong(R.string.transfer_list_network_error);
                } else {
                    ToastUtils.showLong(R.string.network_unavailable);
                }

                return SeafException.NETWORK_EXCEPTION;
            }

            //403, need to re-logg-in
            if (HttpURLConnection.HTTP_FORBIDDEN == httpException.code()) {
                return SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION;
            }

            //404
            if (HttpURLConnection.HTTP_NOT_FOUND == httpException.code()) {
                return SeafException.NOT_FOUND_EXCEPTION;
            }

            //HTTP_STATUS_REPO_PASSWORD_REQUIRED
            if (440 == httpException.code()) {
                return SeafException.INVALID_PASSWORD;
            }

            //500: HTTP_INTERNAL_ERROR
            if (HttpURLConnection.HTTP_INTERNAL_ERROR == httpException.code()) {
                return SeafException.SERVER_INTERNAL_ERROR;
            }

            if (resp != null) {
                try {
                    ResponseBody body = resp.errorBody();
                    return parseErrorJson(httpException.code(), body == null ? null : body.string());
                } catch (IOException e) {
                    SLogs.e(e);
                    return parseErrorJson(httpException.code(), null);
                }

            }
        }

        if (throwable instanceof SSLHandshakeException sslHandshakeException) {
            SLogs.e(sslHandshakeException.getMessage());
            return SeafException.SSL_EXCEPTION;
        }

        if (throwable instanceof SocketTimeoutException socketTimeoutException) {
            SLogs.e(socketTimeoutException.getMessage());
            return SeafException.NETWORK_EXCEPTION;
        }

        if (throwable instanceof SSLPeerUnverifiedException) {
            return SeafException.SSL_EXCEPTION;
        }

        if (throwable instanceof SSLException) {
            return SeafException.SSL_EXCEPTION;
        }

        if (throwable instanceof SecurityException){
            return SeafException.IO_EXCEPTION;
        }

        return new SeafException(SeafException.CODE_ERROR, throwable.getLocalizedMessage());
    }

    public static SeafException parseErrorJson(int errorCode, String bodyString) {
        if (TextUtils.isEmpty(bodyString)) {
            return returnBadRequest(errorCode);
        }

        String lowerBody = bodyString.toLowerCase();
        //[text={"error": "Out of quota.\n"}]
        if (lowerBody.contains("out of quota")) {
            return SeafException.OUT_OF_QUOTA;
        }

        if (lowerBody.contains("wrong password")) {
            return SeafException.INVALID_PASSWORD;
        }

        //"Repo is encrypted. Please provide password to view it."
        if (lowerBody.contains("please provide password to view it")) {
            return SeafException.INVALID_PASSWORD;
        }

        //{"error": "Parent dir doesn't exist."}
        if (lowerBody.contains("parent dir doesn't exist")) {
            return SeafException.NOT_FOUND_DIR_EXCEPTION;
        }

        JSONObject json = Utils.parseJsonObject(bodyString);
        if (json == null) {
            return returnBadRequest(errorCode);
        }

        if (json.has("error_msg")) {
            return new SeafException(errorCode, json.optString("error_msg"));
        }

        if (json.has("error")) {
            return new SeafException(errorCode, json.optString("error"));
        }

        if (json.has("detail")) {
            return new SeafException(errorCode, json.optString("detail"));
        }

        //not parsed
        return new SeafException(errorCode, bodyString);
    }

    private static SeafException returnBadRequest(int code) {
        if (HttpURLConnection.HTTP_BAD_REQUEST == code) {
            return SeafException.REQUEST_EXCEPTION;
        }

        return new SeafException(code, "unknown error");
    }


}
