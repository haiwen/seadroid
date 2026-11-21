package com.seafile.seadroid2.filter;

import android.text.InputFilter;
import android.text.Spanned;
import android.text.TextUtils;

public class CommonInputFilter implements InputFilter {

    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
        //source : old input
        //dest : new input

//        //Only a decimal point
//        if (dest.toString().equals(".")) {
//            return "0.";
//        }

        //Do not allow multiple decimal points
        if (source.toString().contains(".") && dest.toString().contains(".")) {
            return "";
        }

//        //The integer part must not exceed 10 digits
//        if (!source.toString().equals(".") && !dest.toString().contains(".")) {
//            if (dest.toString().length() > 9) {
//                return "";
//            }
//        }

        //Do not start with a decimal point
        if (source.toString().contains(".")) {
            if (TextUtils.isEmpty(dest.toString()) && source.toString().equals(".")) {
                return "0.";
            }
        }

//        //Keep two digits after the decimal point
//        if (start < end && dest.toString().contains(".") && (dest.length() - dest.toString().indexOf(".")) > 3) {
//            LogUtils.e("Only two digits are allowed after the decimal point");
//            return "";
//        }

//        //Prevent a leading zero in a non-decimal number
//        if (start < end && dest.toString().indexOf(".") > 1 && dest.toString().equals("0.") && source.equals("0")) {
//            LogUtils.e("Cannot start with 0");
//            return "";
//        } else if (start < end && !dest.toString().contains(".") && dest.toString().equals("0") && !source.equals(".")) {
//            LogUtils.e("Please enter a correctly formatted amount");
//            return "";
//        }
        return null;
    }
}
