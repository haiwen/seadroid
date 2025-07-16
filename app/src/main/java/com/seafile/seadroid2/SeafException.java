package com.seafile.seadroid2;

import androidx.annotation.NonNull;

import com.google.common.base.MoreObjects;

import java.io.Serial;

public class SeafException extends Exception {
    @Serial
    private static final long serialVersionUID = 1L;

    private final int code;

    public SeafException(int code, String msg) {
        super(msg);
        this.code = code;
    }

    public int getCode() {
        return code;
    }

    @NonNull
    public String toString() {
        return MoreObjects.toStringHelper(this)
                .add("code", code)
                .add("msg", getMessage())
                .toString();
    }

    public static final int CODE_SUCCESS = -1;
    public static final int CODE_FAILED = 0;

    //support parsed http status code
    public static final int HTTP_400_BAD_REQUEST = 400;
    public static final int HTTP_401_UNAUTHORIZED = 401;
    public static final int HTTP_403_FORBIDDEN = 403;
    public static final int HTTP_404_NOT_FOUND = 404;
    public static final int HTTP_409_CONFLICT = 409;
    public static final int HTTP_423_LOCKED = 423;
    public static final int HTTP_440_REPO_PASSWD_REQUIRED = 440;
    public static final int HTTP_441_REPO_PASSWD_MAGIC_REQUIRED = 441;
    public static final int HTTP_443_OUT_OF_QUOTA = 443;
    public static final int HTTP_447_TOO_MANY_FILES_IN_LIBRARY = 447;
    public static final int HTTP_500_INTERNAL_SERVER_ERROR = 500;
    public static final int HTTP_520_OPERATION_FAILED = 520;


    //nonsupport parsed http status code
    public static final int HTTP_405_METHOD_NOT_ALLOWED = 405;
    public static final int HTTP_406_NOT_ACCEPTABLE = 406;
    public static final int HTTP_407_PROXY_AUTHENTICATION_REQUIRED = 407;
    public static final int HTTP_408_REQUEST_TIMEOUT = 408;
    public static final int HTTP_410_GONE = 410;
    public static final int HTTP_411_LENGTH_REQUIRED = 411;
    public static final int HTTP_412_PRECONDITION_FAILED = 412;
    public static final int HTTP_413_REQUEST_ENTITY_TOO_LARGE = 413;
    public static final int HTTP_414_REQUEST_URI_TOO_LONG = 414;
    public static final int HTTP_415_UNSUPPORTED_MEDIA_TYPE = 415;
    public static final int HTTP_416_REQUESTED_RANGE_NOT_SATISFIABLE = 416;
    public static final int HTTP_417_EXPECTATION_FAILED = 417;
    public static final int HTTP_418_IM_A_TEAPOT = 418;
    public static final int HTTP_421_MISDIRECTED_REQUEST = 421;
    public static final int HTTP_422_UNPROCESSABLE_ENTITY = 422;
    public static final int HTTP_424_FAILED_DEPENDENCY = 424;
    public static final int HTTP_425_TOO_EARLY = 425;
    public static final int HTTP_426_UPGRADE_REQUIRED = 426;
    public static final int HTTP_428_PRECONDITION_REQUIRED = 428;
    public static final int HTTP_429_TOO_MANY_REQUESTS = 429;
    public static final int HTTP_431_REQUEST_HEADER_FIELDS_TOO_LARGE = 431;
    public static final int HTTP_451_UNAVAILABLE_FOR_LEGAL_REASONS = 451;
    public static final int HTTP_501_NOT_IMPLEMENTED = 501;
    public static final int HTTP_502_BAD_GATEWAY = 502;
    public static final int HTTP_503_SERVICE_UNAVAILABLE = 503;
    public static final int HTTP_504_GATEWAY_TIMEOUT = 504;
    public static final int HTTP_505_HTTP_VERSION_NOT_SUPPORTED = 505;
    public static final int HTTP_506_VARIANT_ALSO_NEGOTIATES = 506;
    public static final int HTTP_507_INSUFFICIENT_STORAGE = 507;
    public static final int HTTP_508_LOOP_DETECTED = 508;
    public static final int HTTP_509_BANDWIDTH_LIMIT_EXCEEDED = 509;
    public static final int HTTP_510_NOT_EXTENDED = 510;
    public static final int HTTP_511_NETWORK_AUTHENTICATION_REQUIRED = 511;

    public static final SeafException SUCCESS = new SeafException(CODE_SUCCESS, "success");

    //http error code
    public static final SeafException BAD_REQUEST_EXCEPTION = new SeafException(HTTP_400_BAD_REQUEST, "request failed");
    public static final SeafException UNAUTHORIZED_EXCEPTION = new SeafException(HTTP_401_UNAUTHORIZED, "Not logged in");
    public static final SeafException PERMISSION_EXCEPTION = new SeafException(HTTP_403_FORBIDDEN, SeadroidApplication.getAppString(R.string.share_link_no_permission));
    public static final SeafException NOT_FOUND_EXCEPTION = new SeafException(HTTP_404_NOT_FOUND, "Not found");
    public static final SeafException OUT_OF_QUOTA = new SeafException(HTTP_443_OUT_OF_QUOTA, SeadroidApplication.getAppString(R.string.above_quota));
    public static final SeafException SERVER_INTERNAL_ERROR = new SeafException(HTTP_500_INTERNAL_SERVER_ERROR, SeadroidApplication.getAppString(R.string.internal_server_error));

    //Feature error
    public static final SeafException FILE_IO_EXCEPTION = new SeafException(2016, "File io exception");

    public static final SeafException ENCODING_EXCEPTION = new SeafException(2002, "Encoding error");
    public static final SeafException ILL_FORMAT_EXCEPTION = new SeafException(2003, "Ill-formatted Response");

    public static final SeafException UNSUPPORTED_ENC_VERSION = new SeafException(2005, "Unsupported encryption version");
    public static final SeafException ENCRYPT_EXCEPTION = new SeafException(2006, "Encryption key or iv is null");
    public static final SeafException DECRYPT_EXCEPTION = new SeafException(2007, "Decryption key or iv is null");
    public static final SeafException REMOTE_WIPED_EXCEPTION = new SeafException(2008, "Remote Wiped Error");
    public static final SeafException TWO_FACTOR_AUTH_TOKEN_MISSING_EXCEPTION = new SeafException(2009, SeadroidApplication.getAppString(R.string.two_factor_auth_token_empty));
    public static final SeafException TWO_FACTOR_AUTH_TOKEN_INVALID_EXCEPTION = new SeafException(2010, SeadroidApplication.getAppString(R.string.two_factor_auth_invalid));

    public static final SeafException INVALID_PASSWORD = new SeafException(2011, SeadroidApplication.getAppString(R.string.wrong_password));
    public static final SeafException NOT_FOUND_USER_EXCEPTION = new SeafException(2012, SeadroidApplication.getAppString(R.string.saf_account_not_found_exception));
    public static final SeafException NOT_FOUND_DIR_EXCEPTION = new SeafException(2013, "Parent dir doesn't exist");
    public static final SeafException TRANSFER_FILE_EXCEPTION = new SeafException(2014, "The file transfer is abnormal");
    public static final SeafException REQUEST_URL_EXCEPTION = new SeafException(2015, "Request url failed");
    public static final SeafException USER_CANCELLED_EXCEPTION = new SeafException(2017, "The operation was canceled by the user");
    public static final SeafException READ_FILE_EXCEPTION = new SeafException(2018, "Read file failed");


    //network
    public static final SeafException NETWORK_RESET_EXCEPTION = new SeafException(2101, "The connection was reset");
    public static final SeafException NETWORK_SSL_EXCEPTION = new SeafException(2102, "Not trusted SSL server");

    public static final SeafException NETWORK_EXCEPTION = new SeafException(2103, SeadroidApplication.getAppString(R.string.network_error));
    public static final SeafException NETWORK_UNAVAILABLE = new SeafException(2104, SeadroidApplication.getAppString(R.string.network_unavailable));
    public static final SeafException NETWORK_TIMEOUT_EXCEPTION = new SeafException(2105, "Network timeout");
    public static final SeafException NETWORK_INTERRUPTED_EXCEPTION = new SeafException(2106, "Network is interrupted");
    public static final SeafException NETWORK_CONNECT_REFUSE_EXCEPTION = new SeafException(2107, "Connection is refused");
    public static final SeafException NETWORK_EOF_EXCEPTION = new SeafException(2108, "Connection EOF");
    public static final SeafException NETWORK_SHUTDOWN_EXCEPTION = new SeafException(2109, "The connection is shut down");
    public static final SeafException NETWORK_UNKNOWN_HOST_EXCEPTION = new SeafException(2110, "Unknown host");
    public static final SeafException NETWORK_IO_EXCEPTION = new SeafException(2111, "Network io exception");

}
