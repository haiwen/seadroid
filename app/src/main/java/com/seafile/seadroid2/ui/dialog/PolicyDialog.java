package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import android.text.SpannableStringBuilder;
import android.text.TextPaint;
import android.text.method.LinkMovementMethod;
import android.text.style.ClickableSpan;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.settings.PrivacyPolicyActivity;
import com.seafile.seadroid2.util.Utils;

public class PolicyDialog extends Dialog implements View.OnClickListener {
    private TextView tv_statement, tv_details;
    private TextView mConfirm, mCancel;

    private Context mContext;
    private String content;
    private OnCloseListener listener;
    private String positiveName;
    private String negativeName;
    private String title;
    private String policy_statement = "请你务必审慎阅读、充分理解\"隐私政策\"个条款，包括但不限于:为了更好的向你提供服务，我们需要收集你的设备标识、操作日志等信息用于分析、优化应用性能。";
    private String policy_details = "你可阅读《隐私政策》了解详细信息。如果你同意，请点击下面按钮开始使用本应用。";

    public PolicyDialog(Context context) {
        super(context);
        this.mContext = context;
    }

    public PolicyDialog(@NonNull Context context, int themeResId, String content) {
        super(context, themeResId);
        this.mContext = context;
        this.content = content;
    }

    public PolicyDialog(@NonNull Context context, int themeResId, OnCloseListener listener) {
        super(context, themeResId);
        this.mContext = context;
        this.listener = listener;
    }

    public PolicyDialog(@NonNull Context context, int themeResId, String content, OnCloseListener listener) {
        super(context, themeResId);
        this.mContext = context;
        this.content = content;
        this.listener = listener;
    }

    protected PolicyDialog(@NonNull Context context, boolean cancelable, @Nullable OnCancelListener cancelListener) {
        super(context, cancelable, cancelListener);
        this.mContext = context;
    }

    public PolicyDialog setTitle(String title) {
        this.title = title;
        return this;
    }

    public PolicyDialog setContent(String content) {
        this.content = content;
        return this;
    }

    public PolicyDialog setPositiveButton(String name) {
        this.positiveName = name;
        return this;
    }

    public PolicyDialog setNegativeButton(String name) {
        this.negativeName = name;
        return this;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.policy_dialog);


        Window win = getWindow();
        WindowManager.LayoutParams lp = win.getAttributes();
        lp.height = Utils.dip2px(mContext, 500);
        lp.width = Utils.dip2px(mContext, 300);
        win.setAttributes(lp);
        setCanceledOnTouchOutside(true);
        tv_statement = findViewById(R.id.tv_policy_statement);
        tv_details = findViewById(R.id.tv_policy_details);
        mCancel = findViewById(R.id.cancel);
        mConfirm = findViewById(R.id.confirm);
        tv_statement.setText(policy_statement);
        SpannableStringBuilder ssb = new SpannableStringBuilder();
        ssb.append(policy_details);
        final int start = policy_details.indexOf("《");
        ssb.setSpan(new ClickableSpan() {

            @Override
            public void onClick(View widget) {
                Intent intent = new Intent(mContext, PrivacyPolicyActivity.class);
                mContext.startActivity(intent);
            }

            @Override
            public void updateDrawState(TextPaint ds) {
                super.updateDrawState(ds);
                ds.setColor(mContext.getResources().getColor(R.color.blue_700));
                ds.setUnderlineText(false);
            }

        }, start, start + 6, 0);
        tv_details.setMovementMethod(LinkMovementMethod.getInstance());
        tv_details.setText(ssb, TextView.BufferType.SPANNABLE);


        mConfirm.setOnClickListener(this);
        mCancel.setOnClickListener(this);
    }

    @Override
    public void onClick(View v) {
        switch (v.getId()) {
            case R.id.cancel:
                if (listener != null) {
                    listener.onClick(false);
                }
                this.dismiss();
                break;
            case R.id.confirm:
                if (listener != null) {
                    listener.onClick(true);
                }
                this.dismiss();
                break;
        }
    }

    public interface OnCloseListener {
        void onClick(boolean confirm);
    }
}
