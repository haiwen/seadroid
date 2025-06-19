package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;

import com.google.gson.Gson;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.model.ErrorModel;

import org.json.JSONObject;

import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.util.Arrays;
import java.util.List;

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
                try (ResponseBody body = resp.errorBody()) {
                    if (body == null) {
                        return parse(httpException.code(), null);
                    }
                    return parse(httpException.code(), body.string());
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

        SLogs.d("parse errorCode: " + errorCode + ", bodyString: " + bodyString);

        String errorContent = null;
        if (!TextUtils.isEmpty(bodyString)) {
            Gson gson = new Gson();
            ErrorModel errorModel = gson.fromJson(bodyString, ErrorModel.class);
            if (errorModel != null) {
                errorContent = errorModel.getError();
            }
        }

        //400
        if (HttpURLConnection.HTTP_BAD_REQUEST == errorCode) {
            //"Repo is encrypted. Please provide password to view it."
            if (!TextUtils.isEmpty(errorContent) && errorContent.toLowerCase().contains("please provide password to view it")) {
                return SeafException.INVALID_PASSWORD;
            }

            if (!TextUtils.isEmpty(errorContent) && errorContent.toLowerCase().contains("wrong password")) {
                return SeafException.INVALID_PASSWORD;
            }

            if (!TextUtils.isEmpty(errorContent) && errorContent.toLowerCase().contains("operation not supported")) {
                return SeafException.REQUEST_EXCEPTION;
            }

            //{
            //  "non_field_errors" : [ "Not allowed to connect to android client." ]
            //}
            return new SeafException(400, errorContent);
        }

        //401
        if (HttpURLConnection.HTTP_UNAUTHORIZED == errorCode) {
            //Authentication credentials were not provided
            //Incorrect authentication credentials.
            //Invalid token
            //User inactive or deleted
            //Invalid token header. No credentials provided.
            //Invalid token header. Token string should not contain spaces
            //Token inactive or deleted
            //Invalid username/password
            return SeafException.UNAUTHORIZED_EXCEPTION;
        }

        //403 forbidden
        if (HttpURLConnection.HTTP_FORBIDDEN == errorCode) {
            if (!TextUtils.isEmpty(errorContent) && (errorContent.toLowerCase().contains("password is required") || errorContent.toLowerCase().contains("invalid password"))) {
                return SeafException.UNAUTHORIZED_EXCEPTION;
            }

//            for (String detail : HTTP_403_FORBIDDEN_DETAIL_LIST) {
//                if (!TextUtils.isEmpty(lowerBody) && lowerBody.contains(detail)) {
//                    return SeafException.PERMISSION_EXCEPTION;
//                }
//            }

            return SeafException.PERMISSION_EXCEPTION;
        }

        //404
        if (HttpURLConnection.HTTP_NOT_FOUND == errorCode) {
            return SeafException.NOT_FOUND_EXCEPTION;
        }

        //HTTP_423_LOCKED: File is locked
        if (423 == errorCode) {
            return SeafException.REQUEST_EXCEPTION;
        }
        //HTTP_441_REPO_PASSWD_MAGIC_REQUIRED = 441
//        if (441 == errorCode) {
//        }

        //HTTP_409_CONFLICT
//        if (409 == errorCode) {
//        }

        //HTTP_440_REPO_PASSWD_REQUIRED = 440
        if (440 == errorCode) {
            return SeafException.INVALID_PASSWORD;
        }

        //HTTP_443_ABOVE_QUOTA = 443
        if (443 == errorCode) {
            return SeafException.OUT_OF_QUOTA;
        }

        //HTTP_447_TOO_MANY_FILES_IN_LIBRARY = 447
        if (447 == errorCode) {
            return SeafException.REQUEST_EXCEPTION;
        }

        // 500: HTTP_INTERNAL_ERROR
        // HTTP_520_OPERATION_FAILED = 520
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
        //{"error": "Out of quota.\n"}
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

        // {detail: "You do not have permission to perform this action."}
        if (lowerBody.contains("you do not have permission to perform this action")) {
            return SeafException.PERMISSION_EXCEPTION;
        }

        // you do not have permission
        if (lowerBody.contains("you do not have permission")) {
            return SeafException.PERMISSION_EXCEPTION;
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

    //no org、group、email、wiki
    private static final List<String> HTTP_403_FORBIDDEN_DETAIL_LIST = Arrays.asList(
            "permission denied",
            "password is required",
            "invalid password",
            "you do not have permission to perform this action",
            "you do not have permission to create library",
            "you do not have permission to view public libraries",
            "you do not have permission to access this library",
            "you do not have permission to rename this library",
            "you do not have permission to access this folder",
            "you do not have permission to unshare library",
            "you do not have permission to delete this file",
            "you do not have permission to move file in this folder",
            "you do not have permission to move file to destination folder",
            "you do not have permission to copy file of this folder",
            "you do not have permission to copy file to destination folder",
            "you do not have permission to rename file",
            "you do not have permission to move file",
            "you do not have permission to copy file",
            "you do not have permission to create file",
            "you do not have permission to view this file",
            "this library has not been decrypted",
            "library is encrypted, but password is not set in server",
            "forbidden",
            "password is not correct",
            "can not generate share link",
            "can not generate upload link",
            "share link is encrypted",
            "share link has no can_upload permission",
            "share link is expired",
            "upload link is expired",
            "folder permission denied",
            "file is already locked",
            "file is not locked",
            "file is locked",
            "you can not unlock this file",
            "you can not refresh this file's lock",
            "file lock feature only supported in professional edition",
            "office Web App feature only supported in professional edition",
            "office Web App feature not enabled",
            "library encrypted",
            "not allow to create encrypted library",
            "unsyncable share permission",
            "wrong old password",
            "incorrect password",
            "lib_need_decrypt",
            "please check its permission",//"Can't delete folder %s, please check its permission",
            "has no edit permission",//"Share link %s has no edit permission",
            "you can not refresh this file's lock",
            "feature not enabled",
            "feature is not enabled."
    );
}
