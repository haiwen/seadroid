package com.seafile.seadroid2.filter;

import android.text.InputFilter;
import android.text.Spanned;
import android.text.TextUtils;

public class CommonInputFilter implements InputFilter {

    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
        //source : old input
        //dest : new input

//        //only one decimal point
//        if (dest.toString().equals(".")) {
//            return "0.";
//        }

        //cannot enter the decimal point repeatedly
        if (source.toString().contains(".") && dest.toString().contains(".")) {
            return "";
        }

//        //the integer part cannot exceed 10 digits
//        if (!source.toString().equals(".") && !dest.toString().contains(".")) {
//            if (dest.toString().length() > 9) {
//                return "";
//            }
//        }

        //cannot start with a decimal point
        if (source.toString().contains(".")) {
            if (TextUtils.isEmpty(dest.toString()) && source.toString().equals(".")) {
                return "0.";
            }
        }

//        //keep two decimal places after the decimal point
//        if (start < end && dest.toString().contains(".") && (dest.length() - dest.toString().indexOf(".")) > 3) {
//            LogUtils.e("only two digits are allowed after the decimal point");
//            return "";
//        }

//        //ensure the first digit is not 0 when it is not a decimal
//        if (start < end && dest.toString().indexOf(".") > 1 && dest.toString().equals("0.") && source.equals("0")) {
//            LogUtils.e("cannot start with 0");
//            return "";
//        } else if (start < end && !dest.toString().contains(".") && dest.toString().equals("0") && !source.equals(".")) {
//            LogUtils.e("please enter an amount in the correct format");
//            return "";
//        }
        return null;
    }
}
