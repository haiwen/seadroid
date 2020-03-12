package com.seafile.seadroid2.editor.controller;

import android.text.Editable;
import android.widget.Toast;

import com.seafile.seadroid2.editor.Utils;
import com.yydcdut.markdown.MarkdownEditText;
import com.yydcdut.markdown.syntax.SyntaxKey;

public class StyleController {

    private MarkdownEditText mRxMDEditText;

    public StyleController(MarkdownEditText rxMDEditText) {
        mRxMDEditText = rxMDEditText;
    }


    public void doBold() {
        int start = mRxMDEditText.getSelectionStart();
        int end = mRxMDEditText.getSelectionEnd();
        if (start == end) {
            mRxMDEditText.getText().insert(start, "****");
            mRxMDEditText.setSelection(start + 2, end + 2);
        } else if (end - start > 4) {//select 4 and more
            int position0 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), start) + 1;
            int position00 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), end) + 1;
            if (position0 != position00) {
                Toast.makeText(mRxMDEditText.getContext(), "Cannot operate multiple lines", Toast.LENGTH_SHORT).show();
                return;
            }
            Editable editable = mRxMDEditText.getText();
            int i = Utils.safePosition(start, editable);
            int j = Utils.safePosition(start + "**".length(), editable);
            String s = editable.subSequence(i, j).toString();

            int k = Utils.safePosition(end - "**".length(), editable);
            int m = Utils.safePosition(end, editable);
            String str = editable.subSequence(k, m).toString();

            if ("**".equals(s) && "**".equals(str)) {
                mRxMDEditText.getText().delete(end - "**".length(), end);
                mRxMDEditText.getText().delete(start, start + "**".length());
                mRxMDEditText.setSelection(start, end - "**".length() * 2);
            } else {
                mRxMDEditText.getText().insert(end, "**");
                mRxMDEditText.getText().insert(start, "**");
                mRxMDEditText.setSelection(start, end + "**".length() * 2);
            }
        } else {
            mRxMDEditText.getText().insert(end, "**");
            mRxMDEditText.getText().insert(start, "**");
            mRxMDEditText.setSelection(start, end + "**".length() * 2);
        }
    }


    public void doItalic() {
        int start = mRxMDEditText.getSelectionStart();
        int end = mRxMDEditText.getSelectionEnd();
        if (start == end) {
            mRxMDEditText.getText().insert(start, "**");
            mRxMDEditText.setSelection(start + 1, end + 1);
        } else if (end - start > 2) {//select 2 and more
            int position0 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), start) + 1;
            int position00 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), end) + 1;
            if (position0 != position00) {
                Toast.makeText(mRxMDEditText.getContext(), "Cannot operate multiple lines", Toast.LENGTH_SHORT).show();
                return;
            }
            Editable editable = mRxMDEditText.getText();
            int i = Utils.safePosition(start, editable);
            int j = Utils.safePosition(start + SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.length(), editable);
            String s = editable.subSequence(i, j).toString();

            int k = Utils.safePosition(end - SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.length(), editable);
            int m = Utils.safePosition(end, editable);
            String str = editable.subSequence(k, m).toString();
            if (SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.equals(s) && SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.equals(str)) {
                mRxMDEditText.getText().delete(end - SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.length(), end);
                mRxMDEditText.getText().delete(start, start + SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.length());
                mRxMDEditText.setSelection(start, end - SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.length() * 2);
            } else {
                mRxMDEditText.getText().insert(end, SyntaxKey.KEY_BOLD_ASTERISK_SINGLE);
                mRxMDEditText.getText().insert(start, SyntaxKey.KEY_BOLD_ASTERISK_SINGLE);
                mRxMDEditText.setSelection(start, end + SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.length() * 2);
            }
        } else {
            mRxMDEditText.getText().insert(end, SyntaxKey.KEY_BOLD_ASTERISK_SINGLE);
            mRxMDEditText.getText().insert(start, SyntaxKey.KEY_BOLD_ASTERISK_SINGLE);
            mRxMDEditText.setSelection(start, end + SyntaxKey.KEY_BOLD_ASTERISK_SINGLE.length() * 2);
        }
    }

}
