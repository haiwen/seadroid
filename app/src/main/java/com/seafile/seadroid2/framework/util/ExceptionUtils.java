package com.seafile.seadroid2.framework.util;

import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.model.ErrorModel;

import java.io.EOFException;
import java.io.IOException;
import java.net.ConnectException;
import java.net.HttpURLConnection;
import java.net.SocketTimeoutException;
import java.net.UnknownHostException;
import java.util.Arrays;
import java.util.List;

import javax.net.ssl.SSLException;
import javax.net.ssl.SSLHandshakeException;
import javax.net.ssl.SSLPeerUnverifiedException;

import okhttp3.Headers;
import okhttp3.ResponseBody;
import okhttp3.internal.http2.ConnectionShutdownException;
import okhttp3.internal.http2.StreamResetException;
import retrofit2.HttpException;
import retrofit2.Response;

public class ExceptionUtils {
    public static SeafException parseByThrowable(Throwable throwable) {
        return parseByThrowable(throwable, false);
    }

    public static SeafException parseByThrowable(Throwable throwable, boolean withAuthToken) {
        if (throwable == null) {
            return new SeafException(SeafException.CODE_FAILED, "Exception is null");
        }

        //seaf
        if (throwable instanceof SeafException) {
            return (SeafException) throwable;
        }

        //http
        if (throwable instanceof HttpException httpException) {

            Response<?> resp = httpException.response();
            if (resp == null) {
                return new SeafException(SeafException.CODE_FAILED, "response is null");
            }

            Headers headers = resp.headers();
            String wiped = headers.get("X-Seafile-Wiped");
            if (!TextUtils.isEmpty(wiped)) {
                return SeafException.REMOTE_WIPED_EXCEPTION;
            }

            String otp = headers.get("X-Seafile-OTP");
            if ("required".equals(otp)) {
                if (withAuthToken) {
                    return SeafException.TWO_FACTOR_AUTH_TOKEN_INVALID_EXCEPTION;
                } else {
                    return SeafException.TWO_FACTOR_AUTH_TOKEN_MISSING_EXCEPTION;
                }
            }

            try (ResponseBody body = resp.errorBody()) {
                if (body == null) {
                    return parseHttpException(httpException.code(), null);
                }
                return parseHttpException(httpException.code(), body.string());
            } catch (IOException e) {
                SLogs.e(e);
                return parseHttpException(httpException.code(), null);
            }
        }

        //ssl
        if (throwable instanceof SSLHandshakeException sslHandshakeException) {
            SLogs.e(sslHandshakeException.getMessage());
            return SeafException.NETWORK_SSL_EXCEPTION;
        }

        if (throwable instanceof SSLPeerUnverifiedException) {
            return SeafException.NETWORK_SSL_EXCEPTION;
        }

        if (throwable instanceof SSLException) {
            return SeafException.NETWORK_SSL_EXCEPTION;
        }

        if (throwable instanceof SecurityException) {
            return new SeafException(SeafException.PERMISSION_EXCEPTION.getCode(), "May be missing permission");
        }

        // This issue is common if the server is built on Cloudflare/Nginx/gRPC
        if (throwable instanceof StreamResetException streamResetException) {
            SLogs.e(streamResetException);
            return new SeafException(SeafException.NETWORK_RESET_EXCEPTION.getCode(), streamResetException.getMessage());
        }

        // When the connection has been closed, but you still try to manipulate it, such as reusing an HTTP/2 stream.
        if (throwable instanceof ConnectionShutdownException shutdownException) {
            SLogs.e(shutdownException);
            return SeafException.NETWORK_SHUTDOWN_EXCEPTION;
        }

        //connect
        if (throwable instanceof SocketTimeoutException socketTimeoutException) {
            SLogs.e(socketTimeoutException.getMessage());
            return SeafException.NETWORK_TIMEOUT_EXCEPTION;
        }

        //interrupted(socket timeout parent)
        if (throwable instanceof InterruptedException) {
            return SeafException.NETWORK_INTERRUPTED_EXCEPTION;
        }

        if (throwable instanceof ConnectException) {
            return SeafException.NETWORK_CONNECT_REFUSE_EXCEPTION;
        }

        if (throwable instanceof UnknownHostException) {
            return SeafException.NETWORK_UNKNOWN_HOST_EXCEPTION;
        }

        if (throwable instanceof EOFException) {
            return SeafException.NETWORK_EOF_EXCEPTION;
        }

        if (throwable instanceof IOException ioException) {
            SLogs.e(ioException);
            return new SeafException(SeafException.NETWORK_IO_EXCEPTION.getCode(), ioException.getMessage());
        }

        return new SeafException(SeafException.CODE_FAILED, throwable.getLocalizedMessage());
    }

    @NonNull
    public static SeafException parseHttpException(int errorCode, String bodyString) {

        SLogs.d("parse errorCode: " + errorCode + ", bodyString: " + bodyString);

        String errorContent = null;
        if (!TextUtils.isEmpty(bodyString)) {
            try {
                Gson gson = new Gson();
                ErrorModel errorModel = gson.fromJson(bodyString, ErrorModel.class);
                if (errorModel != null && !TextUtils.isEmpty(errorModel.getError())) {
                    errorContent = errorModel.getError();
                } else if (errorModel != null) {
                    errorContent = bodyString;
                    SLogs.w("ExceptionUtils", "ErrorModel parsed but error field is empty. Body: " + bodyString);
                }
            } catch (JsonSyntaxException | IllegalStateException e) {
                SLogs.e("ExceptionUtils", "Failed to parse error body as ErrorModel JSON object.");
                SLogs.e("ExceptionUtils", "Body: " + bodyString);
                SLogs.e(e);
            }

        }

        //400
        if (HttpURLConnection.HTTP_BAD_REQUEST == errorCode) {
            return parse400Body(errorContent, bodyString);
        }

        //401
        if (SeafException.HTTP_401_UNAUTHORIZED == errorCode) {
            return SeafException.UNAUTHORIZED_EXCEPTION;
        }

        //403
        if (SeafException.HTTP_403_FORBIDDEN == errorCode) {
            if (!TextUtils.isEmpty(errorContent) && (errorContent.toLowerCase().contains("password is required") || errorContent.toLowerCase().contains("invalid password"))) {
                return SeafException.UNAUTHORIZED_EXCEPTION;
            }

            return SeafException.PERMISSION_EXCEPTION;
        }

        //404
        if (HttpURLConnection.HTTP_NOT_FOUND == errorCode) {
            return SeafException.NOT_FOUND_EXCEPTION;
        }

        //423
        if (SeafException.HTTP_423_LOCKED == errorCode) {
            return new SeafException(SeafException.HTTP_423_LOCKED, "File is locked");
        }

        //409
        if (SeafException.HTTP_409_CONFLICT == errorCode) {
            return new SeafException(SeafException.HTTP_409_CONFLICT, "Repo is not encrypted");
        }

        //440
        if (SeafException.HTTP_440_REPO_PASSWD_REQUIRED == errorCode) {
            return new SeafException(SeafException.HTTP_440_REPO_PASSWD_REQUIRED, "Library password is needed");
        }

        //441
        if (SeafException.HTTP_441_REPO_PASSWD_MAGIC_REQUIRED == errorCode) {
            return new SeafException(SeafException.HTTP_441_REPO_PASSWD_MAGIC_REQUIRED, "Library password magic is needed.");
        }

        //443
        if (SeafException.HTTP_443_OUT_OF_QUOTA == errorCode) {
            return SeafException.OUT_OF_QUOTA;
        }

        //447
        if (SeafException.HTTP_447_TOO_MANY_FILES_IN_LIBRARY == errorCode) {
            return new SeafException(SeafException.HTTP_447_TOO_MANY_FILES_IN_LIBRARY, "Too many files in library");
        }

        // >= 500: HTTP_INTERNAL_ERROR
        if (errorCode >= HttpURLConnection.HTTP_INTERNAL_ERROR) {
            if (!TextUtils.isEmpty(errorContent)) {
                return new SeafException(errorCode, errorContent);
            }
            return SeafException.SERVER_INTERNAL_ERROR;
        }

        if (!TextUtils.isEmpty(errorContent)) {
            return new SeafException(errorCode, errorContent);
        }

        return new SeafException(errorCode, bodyString);
    }

    @NonNull
    private static SeafException parse400Body(String errorContent, String originalBodyString) {

        if (!TextUtils.isEmpty(errorContent)) {
//            for (String s : HTTP_401_UNAUTHORIZED_DETAIL_LIST) {
//                if (errorContent.toLowerCase().contains(s)) {
//                    return SeafException.UNAUTHORIZED_EXCEPTION;
//                }
//            }


            //custom error

            //"Repo is encrypted. Please provide password to view it."
            if (!TextUtils.isEmpty(errorContent) && errorContent.toLowerCase().contains("please provide password to view it")) {
                return SeafException.INVALID_PASSWORD;
            }


            if (!TextUtils.isEmpty(errorContent) && errorContent.toLowerCase().contains("wrong password")) {
                return SeafException.INVALID_PASSWORD;
            }

            if (!TextUtils.isEmpty(errorContent) && errorContent.toLowerCase().contains("operation not supported")) {
                return SeafException.BAD_REQUEST_EXCEPTION;
            }


            return new SeafException(SeafException.HTTP_400_BAD_REQUEST, errorContent);
        }

        if (TextUtils.isEmpty(originalBodyString)) {
            return SeafException.BAD_REQUEST_EXCEPTION;
        }

        return new SeafException(400, originalBodyString);
    }

    private static final List<String> HTTP_401_UNAUTHORIZED_DETAIL_LIST = Arrays.asList(
            "authentication credentials were not provided",
            "incorrect authentication credentials",
            "unable to login with provided credentials",
            "invalid token",
            "user inactive or deleted",
            "invalid token header. no credentials provided.",
            "invalid token header. token string should not contain spaces",
            "Invalid token header. token string should not contain invalid characters",
            "token inactive or deleted",
            "invalid username/password",
            "Invalid basic header. credentials not correctly base64 encoded",
            "Invalid basic header. credentials string should not contain spaces",
            "Invalid basic header. no credentials provided"
    );

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
