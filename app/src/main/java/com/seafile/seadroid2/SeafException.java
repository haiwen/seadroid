package com.seafile.seadroid2;

import com.google.common.base.MoreObjects;

public class SeafException extends Exception {
    private static final long serialVersionUID = 1L;

    public static final int OTHER_EXCEPTION = 599;
    public static final int HTTP_ABOVE_QUOTA = 443;

    private int code;
    public static final int CODE_SUCCESS = -1;
    public static final int CODE_ERROR = 0;

    public static final SeafException SUCCESS = new SeafException(CODE_SUCCESS, "success");

    public static final SeafException OUT_OF_QUOTA = new SeafException(HTTP_ABOVE_QUOTA, SeadroidApplication.getAppContext().getString(R.string.above_quota));
    public static final SeafException REQUEST_EXCEPTION = new SeafException(400, "request failed");
    public static final SeafException SERVER_INTERNAL_ERROR = new SeafException(500, "server internal error");

    public static final SeafException UNKNOWN_EXCEPTION = new SeafException(1, "unknown error");
    public static final SeafException NETWORK_EXCEPTION = new SeafException(2, SeadroidApplication.getAppContext().getString(R.string.network_error));
    public static final SeafException ENCODING_EXCEPTION = new SeafException(3, "encoding error");
    public static final SeafException ILL_FORMAT_EXCEPTION = new SeafException(4, "Ill-formatted Response");
    public static final SeafException SSL_EXCEPTION = new SeafException(5, "not trusted SSL server");
    public static final SeafException USER_CANCELLED_EXCEPTION = new SeafException(6, "operation cancelled by user");


    public static final SeafException INVALID_PASSWORD = new SeafException(7, SeadroidApplication.getAppContext().getString(R.string.wrong_password));
    public static final SeafException UNSUPPORTED_ENC_VERSION = new SeafException(8, "unsupported encryption version");
    public static final SeafException ENCRYPT_EXCEPTION = new SeafException(10, "encryption key or iv is null");
    public static final SeafException DECRYPT_EXCEPTION = new SeafException(11, "decryption key or iv is null");
    public static final SeafException REMOTE_WIPED_EXCEPTION = new SeafException(12, "remote Wiped Error");
    public static final SeafException TWO_FACTOR_AUTH_TOKEN_MISSING_EXCEPTION = new SeafException(13, SeadroidApplication.getAppContext().getString(R.string.two_factor_auth_token_empty));
    public static final SeafException TWO_FACTOR_AUTH_TOKEN_INVALID_EXCEPTION = new SeafException(14, SeadroidApplication.getAppContext().getString(R.string.two_factor_auth_invalid));

    public static final SeafException NOT_FOUND_EXCEPTION = new SeafException(404, "not found");
    public static final SeafException NOT_FOUND_FILE_EXCEPTION = new SeafException(441, "File does not exist");
    public static final SeafException NOT_FOUND_USER_EXCEPTION = new SeafException(20, "not found user");
    public static final SeafException NOT_FOUND_LOGGED_USER_EXCEPTION = new SeafException(21, "the signed-in user was not found");
    public static final SeafException NOT_FOUND_DIR_EXCEPTION = new SeafException(23, "parent dir doesn't exist");
    public static final SeafException TRANSFER_FILE_EXCEPTION = new SeafException(22, "the file transfer is abnormal");
    public static final SeafException REQUEST_TRANSFER_URL_EXCEPTION = new SeafException(24, "request transfer url failed");
    public static final SeafException IO_EXCEPTION = new SeafException(25, "io exception");
    public static final SeafException PERMISSION_EXCEPTION = new SeafException(26, "miss permission");

    public SeafException(int code, String msg) {
        super(msg);
        this.code = code;
    }

    public int getCode() {
        return code;
    }

    public String toString() {
        return MoreObjects.toStringHelper(this)
                .add("code", code)
                .add("msg", getMessage())
                .toString();
    }
}
