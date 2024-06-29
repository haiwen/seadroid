package com.seafile.seadroid2.framework.util;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.data.model.enums.TransferResult;

import org.json.JSONException;

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.SocketTimeoutException;

import javax.net.ssl.SSLHandshakeException;

public class TransferUtils {

    public static TransferResult convertException2TransferResult(Exception e) {
        if (e instanceof JSONException) {
            return TransferResult.ENCODING_EXCEPTION;
        } else if (e instanceof SeafException) {
            if (e == SeafException.notFoundException) {
                return TransferResult.FILE_NOT_FOUND;
            } else if (e == SeafException.OUT_OF_QUOTA) {
                return TransferResult.OUT_OF_QUOTA;
            } else if (e == SeafException.networkException) {
                return TransferResult.NETWORK_CONNECTION;
            } else if (e == SeafException.sslException) {
                return TransferResult.SSL_EXCEPTION;
            } else if (e == SeafException.illFormatException) {
                return TransferResult.ENCODING_EXCEPTION;
            } else if (e == SeafException.notLoggedInException) {
                return TransferResult.ACCOUNT_NOT_LOGGED_IN;
            } else if (e == SeafException.notFoundUserException) {
                return TransferResult.ACCOUNT_NOT_FOUND;
            }
        } else if (e instanceof UnsupportedEncodingException) {
            return TransferResult.ENCODING_EXCEPTION;
        } else if (e instanceof SSLHandshakeException) {
            return TransferResult.SSL_EXCEPTION;
        } else if (e instanceof SocketTimeoutException) {
            return TransferResult.NETWORK_CONNECTION;
        } else if (e instanceof IOException) {
            return TransferResult.NETWORK_CONNECTION;
        }

        return TransferResult.UNKNOWN;
    }
}
