package com.seafile.seadroid2.view;

import android.content.Context;
import android.text.TextUtils;
import android.util.Pair;
import android.view.Gravity;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.blankj.utilcode.util.NumberUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.google.android.flexbox.FlexWrap;
import com.google.android.flexbox.FlexboxLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.data.model.sdoc.MetadataConfigDataModel;

import java.math.BigDecimal;
import java.util.Locale;

public class KeyViews {
    public int DP_2 = Constants.DP.DP_2;
    public int DP_4 = Constants.DP.DP_4;
    public int DP_8 = Constants.DP.DP_8;
    public int DP_16 = Constants.DP.DP_16;
    public int DP_32 = Constants.DP.DP_32;

    public static int DEFAULT_VIEW_WIDTH = SizeUtils.dp2px(150);
    public static int DEFAULT_VIEW_HEIGHT = SizeUtils.dp2px(30);
    public static int DEFAULT_IMAGE_VIEW_WH = SizeUtils.dp2px(96);

    //
    private static volatile KeyViews mSingleton = null;

    public static KeyViews getInstance() {
        if (mSingleton == null) {
            synchronized (KeyViews.class) {
                if (mSingleton == null) {
                    mSingleton = new KeyViews();
                }
            }
        }
        return mSingleton;
    }

    public void buildEditableText(Context context, boolean editable, LinearLayout parentContainer, String defaultText) {

    }

    private Pair<Boolean, FlexboxLayout> getFlexContainer(Context context, LinearLayout parentContainer, String key) {
        FlexboxLayout flexboxContainer = parentContainer.findViewWithTag(key);
        boolean isAdded = flexboxContainer != null;
        if (flexboxContainer != null) {
            flexboxContainer.removeAllViews();
        } else {
            flexboxContainer = new FlexboxLayout(context);
            LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(-1, -2);
            llp.topMargin = DP_8;
            llp.bottomMargin = DP_16;
            flexboxContainer.setLayoutParams(llp);
            flexboxContainer.setTag(key);
            flexboxContainer.setFlexWrap(FlexWrap.WRAP);
        }
        return new Pair<>(isAdded, flexboxContainer);
    }


    public TextView getSingleLineTextView(Context context, String string) {
        TextView textView = new TextView(context);
        textView.setTextSize(14);
        textView.setTextColor(context.getColor(R.color.light_grey));
        textView.setMaxLines(1);
        textView.setEllipsize(TextUtils.TruncateAt.END);
        textView.setText(string);
        textView.setGravity(Gravity.CENTER_VERTICAL);
        LinearLayout.LayoutParams llp = new LinearLayout.LayoutParams(DEFAULT_VIEW_WIDTH, DEFAULT_VIEW_HEIGHT);
        textView.setLayoutParams(llp);
        textView.setPadding(0, DP_2, DP_2, DP_2);
        return textView;
    }


    public String getNormalFormatNumber(String t, MetadataConfigDataModel columnDataModel) {
        return getFormatNumber(t, columnDataModel, false);
    }

    public String getNoDotCharNumber(String t, String format) {
        if (TextUtils.isEmpty(format)) {
            return t;
        }

        //set default value
        if (!"number".equals(format)) {
            return t;
        }
        //1.234567890111E12

        if (TextUtils.isEmpty(t)) {
            return t;
        }

        //Scientific notation
        if (t.contains(".") && t.toLowerCase(Locale.getDefault()).contains("e")) {
            BigDecimal bd = new BigDecimal(t);
            return bd.toPlainString();
        } else if (t.contains(".")) {
            int decimalIndex = t.indexOf(".");
            if (decimalIndex >= 0) {
                String[] ts = t.split("\\.");
                String decimalPart = ts[1];
                double decimalValue = Double.parseDouble("0." + decimalPart);
                if (decimalValue == 0) {
                    return ts[0];
                } else {
                    return t;
                }
            }
        }


        return t;
    }

    /**
     * @param isPercentTypeMultiplyBy100 true: if type is percent, multiply by 100
     */
    public String getFormatNumber(String t, MetadataConfigDataModel columnDataModel, boolean isPercentTypeMultiplyBy100) {
        if (TextUtils.isEmpty(t)) {
            return t;
        }

        if (columnDataModel == null) {
            return t;
        }

        //set default value
        if ("number".equals(columnDataModel.format)) {
            return getNoDotCharNumber(t, columnDataModel.format);
        }

        if ("percent".equals(columnDataModel.format)) {
            if (t.contains("%")) {
                t = t.replace("%", "");
            }
            double d = Double.parseDouble(t);
            if (isPercentTypeMultiplyBy100) {
                d = d * 100;
            }

            String r;
            if (columnDataModel.enable_precision) {
                r = NumberUtils.format(d, columnDataModel.precision);
            } else {
                r = String.valueOf(d);
            }

            return r + "%";
        } else if ("yuan".equals(columnDataModel.format)) {
            if (t.contains("￥")) {
                t = t.replace("￥", "");
            }
            double d = Double.parseDouble(t);

            String r;
            if (columnDataModel.enable_precision) {
                r = NumberUtils.format(d, columnDataModel.precision);
            } else {
                r = String.valueOf(d);
            }
            return "￥" + r;
        } else if ("dollar".equals(columnDataModel.format)) {
            if (t.contains("$")) {
                t = t.replace("$", "");
            }

            double d = Double.parseDouble(t);

            String r;
            if (columnDataModel.enable_precision) {
                r = NumberUtils.format(d, columnDataModel.precision);
            } else {
                r = String.valueOf(d);
            }

            return "$" + r;
        } else if ("euro".equals(columnDataModel.format)) {
            if (t.contains("€")) {
                t = t.replace("€", "");
            }
            double d = Double.parseDouble(t);

            String r;
            if (columnDataModel.enable_precision) {
                r = NumberUtils.format(d, columnDataModel.precision);
            } else {
                r = String.valueOf(d);
            }
            return "€" + r;
        } else if ("custom_currency".equals(columnDataModel.format)) {
            if (t.contains(columnDataModel.currency_symbol)) {
                t = t.replace(columnDataModel.currency_symbol, "");
            }

            double d = Double.parseDouble(t);
            String r;
            if (columnDataModel.enable_precision) {
                r = NumberUtils.format(d, columnDataModel.precision);
            } else {
                r = String.valueOf(d);
            }

            if ("before".equals(columnDataModel.currency_symbol_position)) {
                return columnDataModel.currency_symbol + r;
            } else {
                return r + columnDataModel.currency_symbol;
            }
        }
        return t;
    }
}
