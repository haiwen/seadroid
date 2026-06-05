package com.seafile.seadroid2.framework.model.activities;

import android.content.Context;
import android.text.SpannableStringBuilder;
import android.text.Spanned;
import android.text.TextUtils;
import android.text.style.ForegroundColorSpan;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.util.SystemSwitchUtils;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class ActivityModel extends BaseModel {

    public String author_contact_email;
    public String author_email;
    public String author_name;
    public String avatar_url;
    public String commit_id;
    public String login_id;
    public String name;
    public String op_type;
    public String obj_type;
    public String repo_id;
    public String path;
    public String repo_name;
    public String time;
    public String old_path;
    public String old_name;


    // custom
    public String related_account;

    // server version >= 14.0
    public int count = 0;
    public List<ActivityDetailModel> details;

    private transient String formatedTime;
    private transient String formattedDesc;
    private transient CharSequence formattedDetail;
    private transient int formattedDetailDefaultColor;
    private transient int formattedDetailFancyColor;

    public String getTime() {
        if (StringUtils.isNoneEmpty(formatedTime)) {
            return formatedTime;
        }

        if (StringUtils.isEmpty(time)) {
            return "";
        }

        long mTempTimeLong = Times.convertMtime2Long(time);
        formatedTime = Utils.translateCommitTime(mTempTimeLong);
        return formatedTime;
    }

    public boolean isDir() {
        return TextUtils.equals(obj_type, "dir");
    }

    public String getDesc(Context context) {
        if (!TextUtils.isEmpty(formattedDesc)) {
            return formattedDesc;
        }

        formattedDesc = SystemSwitchUtils.obj_type(context, obj_type, op_type, count);
        return formattedDesc;
    }

    public String getRepoNameText() {
        if ("repo".equals(obj_type)) {
            return "";
        }

        return safeString(repo_name);
    }

    public CharSequence getDetailText(Context context, int defaultColor, int fancyColor) {
        if (formattedDetail != null
                && formattedDetailDefaultColor == defaultColor
                && formattedDetailFancyColor == fancyColor) {
            return formattedDetail;
        }

        SpannableStringBuilder builder = new SpannableStringBuilder();

        if ("repo".equals(obj_type)) {
            appendColored(builder, repo_name, fancyColor);
        } else if ("rename".equals(op_type)) {
            int start = builder.length();
            builder.append(safeString(old_name));
            builder.append(" => ");
            builder.append(safeString(name));
            builder.setSpan(new ForegroundColorSpan(fancyColor), start, builder.length(), Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        } else if (CollectionUtils.isEmpty(details)) {
            appendColored(builder, name, "delete".equals(op_type) ? defaultColor : fancyColor);
        } else if (details.size() == 1) {
            appendColored(builder, name, "delete".equals(op_type) ? defaultColor : fancyColor);
        } else {
            int c = count - 1;
            if (c < 1) {
                c = 1;
            }

            String otherStr;
            if ("dir".equals(obj_type)) {
                otherStr = context.getString(R.string.and_other_folders, c);
            } else {
                otherStr = context.getString(R.string.and_other_files, c);
            }

            int detailColor;
            if ("delete".equals(op_type) || "batch_delete".equals(op_type)) {
                detailColor = defaultColor;
            } else {
                detailColor = fancyColor;
            }

            appendColored(builder, name, detailColor);
            builder.append(" ");
            appendColored(builder, otherStr, defaultColor);
        }

        formattedDetail = builder;
        formattedDetailDefaultColor = defaultColor;
        formattedDetailFancyColor = fancyColor;
        return formattedDetail;
    }

    private void appendColored(SpannableStringBuilder builder, String text, int color) {
        int start = builder.length();
        String safeText = safeString(text);
        builder.append(safeText);
        if (!safeText.isEmpty()) {
            builder.setSpan(new ForegroundColorSpan(color), start, builder.length(), Spanned.SPAN_EXCLUSIVE_EXCLUSIVE);
        }
    }

    private String safeString(String text) {
        return text == null ? "" : text;
    }
}
