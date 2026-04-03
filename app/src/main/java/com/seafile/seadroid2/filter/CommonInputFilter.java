package com.seafile.seadroid2.filter;

import android.text.InputFilter;
import android.text.Spanned;
import android.text.TextUtils;

public class CommonInputFilter implements InputFilter {

    @Override
    public CharSequence filter(CharSequence source, int start, int end, Spanned dest, int dstart, int dend) {
        //source : old input
        //dest : new input

//        //只有一个小数点
//        if (dest.toString().equals(".")) {
//            return "0.";
//        }

        //不能重复输入小数点
        if (source.toString().contains(".") && dest.toString().contains(".")) {
            return "";
        }

//        //整数不能超过10位
//        if (!source.toString().equals(".") && !dest.toString().contains(".")) {
//            if (dest.toString().length() > 9) {
//                return "";
//            }
//        }

        //不能以小数点开始
        if (source.toString().contains(".")) {
            if (TextUtils.isEmpty(dest.toString()) && source.toString().equals(".")) {
                return "0.";
            }
        }

//        //小数点后要保留两位小数
//        if (start < end && dest.toString().contains(".") && (dest.length() - dest.toString().indexOf(".")) > 3) {
//            LogUtils.e("小数点后只能输两位");
//            return "";
//        }

//        //确保不会出现不是小数的第一位为0
//        if (start < end && dest.toString().indexOf(".") > 1 && dest.toString().equals("0.") && source.equals("0")) {
//            LogUtils.e("不能从0开始");
//            return "";
//        } else if (start < end && !dest.toString().contains(".") && dest.toString().equals("0") && !source.equals(".")) {
//            LogUtils.e("请输入正确格式的金额");
//            return "";
//        }
        return null;
    }
}
