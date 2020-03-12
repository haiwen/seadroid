package com.seafile.seadroid2.editor.controller;

import android.content.DialogInterface;
import android.support.v7.app.AlertDialog;
import android.text.TextUtils;
import android.view.ViewGroup;
import android.widget.LinearLayout;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.editor.LinkDialogView;
import com.yydcdut.markdown.MarkdownEditText;


public class LinkController {
    private final LinkDialogView mLinkDialogView;
    private MarkdownEditText mRxMDEditText;

    private AlertDialog mAlertDialog;

    public LinkController(MarkdownEditText rxMDEditText) {
        mRxMDEditText = rxMDEditText;
        mLinkDialogView = new LinkDialogView(rxMDEditText.getContext());
        mLinkDialogView.setLayoutParams(new LinearLayout.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                ViewGroup.LayoutParams.WRAP_CONTENT));
    }


    public void doImage() {
        if (mAlertDialog == null) {
            initDialog();
        }
        mLinkDialogView.clear();
        mAlertDialog.show();
    }

    private void initDialog() {
        mAlertDialog = new AlertDialog.Builder(mRxMDEditText.getContext())
                .setView(mLinkDialogView)
                .setPositiveButton(R.string.confirm, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        dialog.dismiss();
                        String description = mLinkDialogView.getDescription();
                        String link = mLinkDialogView.getLink();
                        doRealLink(description, link);
                    }
                })
                .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialog, int which) {
                        dialog.dismiss();
                    }
                })
                .setTitle(R.string.link)
                .setCancelable(false)
                .create();
    }

    private void doRealLink(String description, String link) {
        int start = mRxMDEditText.getSelectionStart();
        if (TextUtils.isEmpty(description)) {
            mRxMDEditText.getText().insert(start, "[](" + link + ")");
            mRxMDEditText.setSelection(start + 2);
        } else {
            mRxMDEditText.getText().insert(start, "[" + description + "](" + link + ")");
        }
    }
}
