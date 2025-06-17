package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;

import org.json.JSONObject;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;
import javax.net.ssl.SSLPeerUnverifiedException;

import okhttp3.ResponseBody;
import retrofit2.HttpException;
import retrofit2.Response;

public class ExceptionUtils {
    public static SeafException parseByThrowable(Throwable throwable) {
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

            if (resp != null) {
                try {
                    try (ResponseBody body = resp.errorBody()) {
                        return parse(httpException.code(), body == null ? null : body.string());
                    }
                } catch (IOException e) {
                    SLogs.e(e);
                    return parse(httpException.code(), null);
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

        if (throwable instanceof SecurityException) {
            return SeafException.IO_EXCEPTION;
        }

        return new SeafException(SeafException.CODE_ERROR, throwable.getLocalizedMessage());
    }

    public static SeafException parse(int errorCode, String bodyString) {

        //400
        if (HttpURLConnection.HTTP_BAD_REQUEST == errorCode) {
            //"Repo is encrypted. Please provide password to view it."
            if (bodyString.toLowerCase().contains("please provide password to view it")) {
                return SeafException.INVALID_PASSWORD;
            }

            if (bodyString.toLowerCase().contains("wrong password")) {
                return SeafException.INVALID_PASSWORD;
            }

            if (bodyString.toLowerCase().contains("operation not supported")) {
                return SeafException.OPERATION_NOT_SUPPORTED_EXCEPTION;
            }

            //{
            //  "non_field_errors" : [ "Not allowed to connect to android client." ]
            //}
            return SeafException.REQUEST_EXCEPTION;
        }


        //401
        if (HttpURLConnection.HTTP_UNAUTHORIZED == errorCode) {
            return SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION;
        }

        //403, need to re-logg-in
        if (HttpURLConnection.HTTP_FORBIDDEN == errorCode) {
            return SeafException.NOT_FOUND_LOGGED_USER_EXCEPTION;
        }

        //404
        if (HttpURLConnection.HTTP_NOT_FOUND == errorCode) {
            return SeafException.NOT_FOUND_EXCEPTION;
        }

        //441
        if (441 == errorCode) {
            return SeafException.NOT_FOUND_FILE_EXCEPTION;
        }

        //HTTP_STATUS_REPO_PASSWORD_REQUIRED
        if (440 == errorCode) {
            return SeafException.INVALID_PASSWORD;
        }

        //500: HTTP_INTERNAL_ERROR
        if (errorCode >= HttpURLConnection.HTTP_INTERNAL_ERROR) {
            return SeafException.SERVER_INTERNAL_ERROR;
        }

        return parseBody(errorCode, bodyString);
    }

    public static SeafException parseBody(int code, String bodyString) {
        if (TextUtils.isEmpty(bodyString)) {
            return SeafException.UNKNOWN_EXCEPTION;
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
            return new SeafException(code, bodyString);
        }

        if (json.has("error_msg")) {
            return new SeafException(code, json.optString("error_msg"));
        }

        if (json.has("error")) {
            return new SeafException(code, json.optString("error"));
        }

        if (json.has("detail")) {
            return new SeafException(code, json.optString("detail"));
        }

        //not parsed
        return new SeafException(code, bodyString);
    }
}
