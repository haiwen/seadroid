package com.seafile.seadroid2;

import com.google.common.base.Objects;

public class SeafException extends Exception {
    private static final long serialVersionUID = 1L;

    public static final int OTHER_EXCEPTION = 599;

    private int code;

    public static SeafException unknownException = new SeafException(1, "Unknown Error");
    public static SeafException networkException = new SeafException(2, "Network Error");
    public static SeafException encodingException = new SeafException(3, "Encoding Error");
    public static SeafException illFormatException = new SeafException(4, "Ill-formatted Response");
    public static SeafException sslException = new SeafException(5, "not trusted SSL server");
    public static SeafException userCancelledException = new SeafException(6, "operation canclled by user");

    public SeafException(int code, String msg) {
        super(msg);
        this.code = code;
    }

    public int getCode() {
        return code;
    }

    public String toString() {
        return Objects.toStringHelper(this)
            .add("code", code)
            .add("msg", getMessage())
            .toString();
    }
}
