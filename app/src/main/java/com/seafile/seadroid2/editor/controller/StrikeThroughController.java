package com.seafile.seadroid2.editor.controller;

import android.text.Editable;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.editor.Utils;
import com.yydcdut.markdown.MarkdownEditText;


public class StrikeThroughController {
    private MarkdownEditText mRxMDEditText;

    public StrikeThroughController(MarkdownEditText rxMDEditText) {
        mRxMDEditText = rxMDEditText;
    }


    public void doStrikeThrough() {
        int start = mRxMDEditText.getSelectionStart();
        int end = mRxMDEditText.getSelectionEnd();
        if (start == end) {
            mRxMDEditText.getText().insert(start, "~~~~");
            mRxMDEditText.setSelection(start + 2, end + 2);
        } else if (end - start > 4) {
            int position0 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), start) + 1;
            int position00 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), end) + 1;
            if (position0 != position00) {
                Toast.makeText(mRxMDEditText.getContext(), R.string.editor_lines_error, Toast.LENGTH_SHORT).show();
                return;
            }
            Editable editable = mRxMDEditText.getText();
            if ("~~".equals(editable.subSequence(Utils.safePosition(start, editable), Utils.safePosition(start + "~~".length(), editable)).toString()) &&
                    "~~".equals(editable.subSequence(Utils.safePosition(end - "~~".length(), editable), Utils.safePosition(end, editable)).toString())) {
                mRxMDEditText.getText().delete(end - "~~".length(), end);
                mRxMDEditText.getText().delete(start, start + "~~".length());
                mRxMDEditText.setSelection(start, end - "~~".length() * 2);
            } else {
                mRxMDEditText.getText().insert(end, "~~");
                mRxMDEditText.getText().insert(start, "~~");
                mRxMDEditText.setSelection(start, end + "~~".length() * 2);
            }
        } else {
            mRxMDEditText.getText().insert(end, "~~");
            mRxMDEditText.getText().insert(start, "~~");
            mRxMDEditText.setSelection(start, end + "~~".length() * 2);
        }
    }
}
