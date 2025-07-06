package com.seafile.seadroid2.framework.model;

import android.text.TextUtils;

import java.util.List;

public class ErrorModel {
    public String error_msg;
    public String msg;
    public String error;
    public String errorMsg;
    public String errMessage;
    public String err_message;
    public String detail;
    public List<String> non_field_errors;

    public String getError() {
        if (!TextUtils.isEmpty(error_msg)) return error_msg;
        if (!TextUtils.isEmpty(error)) return error;
        if (!TextUtils.isEmpty(errorMsg)) return errorMsg;
        if (!TextUtils.isEmpty(detail)) return detail;
        if (!TextUtils.isEmpty(errMessage)) return errMessage;
        if (!TextUtils.isEmpty(err_message)) return err_message;
        if (non_field_errors != null && !non_field_errors.isEmpty()) {
            return non_field_errors.get(0);
        }
        if (!TextUtils.isEmpty(msg)) return msg;
        return null;
    }
}
