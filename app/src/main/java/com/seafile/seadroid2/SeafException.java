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

    public static final SeafException unknownException = new SeafException(1, "Unknown Error");
    public static final SeafException networkException = new SeafException(2, "Network Error");
    public static final SeafException encodingException = new SeafException(3, "Encoding Error");
    public static final SeafException illFormatException = new SeafException(4, "Ill-formatted Response");
    public static final SeafException sslException = new SeafException(5, "not trusted SSL server");
    public static final SeafException userCancelledException = new SeafException(6, "operation cancelled by user");


    public static final SeafException invalidPassword = new SeafException(7, SeadroidApplication.getAppContext().getString(R.string.wrong_password));
    public static final SeafException unsupportedEncVersion = new SeafException(8, "unsupported encryption version");
    public static final SeafException blockListNullPointerException = new SeafException(9, "block list is null");
    public static final SeafException encryptException = new SeafException(10, "encryption key or iv is null");
    public static final SeafException decryptException = new SeafException(11, "decryption key or iv is null");
    public static final SeafException remoteWipedException = new SeafException(12, "Remote Wiped Error");
    public static final SeafException twoFactorAuthTokenMissing = new SeafException(13, SeadroidApplication.getAppContext().getString(R.string.two_factor_auth_token_empty));
    public static final SeafException twoFactorAuthTokenInvalid = new SeafException(14, SeadroidApplication.getAppContext().getString(R.string.two_factor_auth_invalid));

    public static final SeafException notFoundException = new SeafException(404, "Not found");
    public static final SeafException notFoundUserException = new SeafException(20, "Not logged in");
    public static final SeafException notLoggedInException = new SeafException(21, "Not logged in");

    public static final SeafException OUT_OF_QUOTA = new SeafException(HTTP_ABOVE_QUOTA, SeadroidApplication.getAppContext().getString(R.string.above_quota));
    public static final SeafException REQUEST_EXCEPTION = new SeafException(400, "Request Failed");

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
