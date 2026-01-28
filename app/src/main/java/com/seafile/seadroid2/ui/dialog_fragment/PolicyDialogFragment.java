package com.seafile.seadroid2.ui.dialog_fragment;

import android.app.Dialog;
import android.os.Bundle;
import android.text.SpannableStringBuilder;
import android.text.TextPaint;
import android.text.method.LinkMovementMethod;
import android.text.style.ClickableSpan;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.core.content.ContextCompat;
import androidx.fragment.app.DialogFragment;

import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.listener.OnCallback;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;


public class PolicyDialogFragment extends DialogFragment {

    private OnCallback onCallback;

    public void setOnCallback(OnCallback onCallback) {
        this.onCallback = onCallback;
    }


    @NonNull
    @Override
    public Dialog onCreateDialog(@Nullable Bundle savedInstanceState) {

        View rootView = getLayoutInflater().inflate(R.layout.layout_dialog_policy, null);
        TextView messageView = rootView.findViewById(R.id.text_view_message);

        TextView positiveView = rootView.findViewById(R.id.text_view_positive);
        TextView negativeView = rootView.findViewById(R.id.text_view_negative);
        positiveView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (onCallback != null) {
                    onCallback.onSuccess();
                }
                dismiss();
            }
        });
        negativeView.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                if (onCallback != null) {
                    onCallback.onFailed();
                }
                dismiss();
            }
        });

        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireActivity());
        builder.setTitle("隐私政策");
        String message = "请你务必审慎阅读、充分理解“隐私政策”个条款，包括但不限于：为了更好的向你提供服务，我们需要收集你的设备标识、操作日志等信息用于分析、优化应用性能。\n\n你可阅读《隐私政策》了解详细信息。如果你同意，请点击下面按钮开始使用本应用。";

        SpannableStringBuilder ssb = new SpannableStringBuilder();
        ssb.append(message);
        final int start = message.indexOf("《");
        ssb.setSpan(new ClickableSpan() {

            @Override
            public void onClick(View widget) {
                SeaWebViewActivity.openUrl(requireContext(), Constants.URL_PRIVACY,false);
            }

            @Override
            public void updateDrawState(TextPaint ds) {
                super.updateDrawState(ds);
                ds.setColor(ContextCompat.getColor(requireContext(), R.color.fancy_orange));
                ds.setUnderlineText(false);
            }

        }, start, start + 6, 0);
        messageView.setMovementMethod(LinkMovementMethod.getInstance());
        messageView.setText(ssb, TextView.BufferType.SPANNABLE);

        builder.setView(rootView);

        final AlertDialog dialog = builder.create();
        dialog.setCanceledOnTouchOutside(false);
        dialog.setCancelable(false);

        return dialog;
    }

}
