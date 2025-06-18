package com.seafile.seadroid2;

import com.google.common.base.MoreObjects;

public class SeafException extends Exception {
    private static final long serialVersionUID = 1L;

    private int code;
    public static final int CODE_SUCCESS = -1;
    public static final int CODE_ERROR = 0;

    public static final SeafException SUCCESS = new SeafException(CODE_SUCCESS, "success");

    //http error code
    public static final SeafException REQUEST_EXCEPTION = new SeafException(400, "request failed");
    public static final SeafException UNAUTHORIZED_EXCEPTION = new SeafException(401, "Not logged in");
    public static final SeafException PERMISSION_EXCEPTION = new SeafException(403, SeadroidApplication.getAppString(R.string.share_link_no_permission));
    public static final SeafException NOT_FOUND_EXCEPTION = new SeafException(404, "Not found");
    public static final SeafException OUT_OF_QUOTA = new SeafException(443, SeadroidApplication.getAppString(R.string.above_quota));
    public static final SeafException SERVER_INTERNAL_ERROR = new SeafException(500, SeadroidApplication.getAppString(R.string.internal_server_error));


    public static final SeafException UNKNOWN_EXCEPTION = new SeafException(1000, "unknown error");
    public static final SeafException NETWORK_EXCEPTION = new SeafException(2000, SeadroidApplication.getAppString(R.string.network_error));
    public static final SeafException NETWORK_UNAVAILABLE = new SeafException(2001, SeadroidApplication.getAppString(R.string.network_unavailable));

    public static final SeafException ENCODING_EXCEPTION = new SeafException(2002, "encoding error");
    public static final SeafException ILL_FORMAT_EXCEPTION = new SeafException(2003, "Ill-formatted Response");
    public static final SeafException SSL_EXCEPTION = new SeafException(2004, "not trusted SSL server");

    public static final SeafException UNSUPPORTED_ENC_VERSION = new SeafException(2005, "unsupported encryption version");
    public static final SeafException ENCRYPT_EXCEPTION = new SeafException(2006, "encryption key or iv is null");
    public static final SeafException DECRYPT_EXCEPTION = new SeafException(2007, "decryption key or iv is null");
    public static final SeafException REMOTE_WIPED_EXCEPTION = new SeafException(2008, "remote Wiped Error");
    public static final SeafException TWO_FACTOR_AUTH_TOKEN_MISSING_EXCEPTION = new SeafException(2009, SeadroidApplication.getAppString(R.string.two_factor_auth_token_empty));
    public static final SeafException TWO_FACTOR_AUTH_TOKEN_INVALID_EXCEPTION = new SeafException(2010, SeadroidApplication.getAppString(R.string.two_factor_auth_invalid));

    public static final SeafException INVALID_PASSWORD = new SeafException(2011, SeadroidApplication.getAppString(R.string.wrong_password));
    public static final SeafException NOT_FOUND_USER_EXCEPTION = new SeafException(2012, SeadroidApplication.getAppString(R.string.saf_account_not_found_exception));
    public static final SeafException NOT_FOUND_DIR_EXCEPTION = new SeafException(2013, "parent dir doesn't exist");
    public static final SeafException TRANSFER_FILE_EXCEPTION = new SeafException(2014, "the file transfer is abnormal");
    public static final SeafException REQUEST_TRANSFER_URL_EXCEPTION = new SeafException(2015, "request transfer url failed");
    public static final SeafException IO_EXCEPTION = new SeafException(2016, "io exception");
    public static final SeafException USER_CANCELLED_EXCEPTION = new SeafException(2017, "operation cancelled by user");

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
