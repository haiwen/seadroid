package com.seafile.seadroid2.editor.controller;

import android.text.Editable;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.editor.Utils;
import com.yydcdut.markdown.MarkdownEditText;

public class CodeController {
    private MarkdownEditText mRxMDEditText;

    public CodeController(MarkdownEditText rxMDEditText) {
        mRxMDEditText = rxMDEditText;
    }


    public void doInlineCode() {
        int start = mRxMDEditText.getSelectionStart();
        int end = mRxMDEditText.getSelectionEnd();
        if (start == end) {
            mRxMDEditText.getText().insert(start, "``");
            mRxMDEditText.setSelection(start + 1, end + 1);
        } else if (end - start > 2) {
            int position0 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), start) + 1;
            int position00 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), end) + 1;
            if (position0 != position00) {
                Toast.makeText(mRxMDEditText.getContext(), R.string.editor_lines_error, Toast.LENGTH_SHORT).show();
                return;
            }
            Editable editable = mRxMDEditText.getText();
            if ("`".equals(editable.subSequence(Utils.safePosition(start, editable), Utils.safePosition(start + "`".length(), editable))
                    .toString()) &&
                    "`".equals(editable.subSequence(Utils.safePosition(end - "`".length(), editable), Utils.safePosition(end, editable))
                            .toString())) {
                mRxMDEditText.getText().delete(end - "`".length(), end);
                mRxMDEditText.getText().delete(start, start + "`".length());
                mRxMDEditText.setSelection(start, end - "`".length() * 2);
            } else {
                mRxMDEditText.getText().insert(end, "`");
                mRxMDEditText.getText().insert(start, "`");
                mRxMDEditText.setSelection(start, end + "`".length() * 2);
            }
        } else {
            mRxMDEditText.getText().insert(end, "`");
            mRxMDEditText.getText().insert(start, "`");
            mRxMDEditText.setSelection(start, end + "`".length() * 2);
        }
    }

    public void doCode() {
        int start = mRxMDEditText.getSelectionStart();
        int end = mRxMDEditText.getSelectionEnd();
        if (start == end) {
            int position0 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), start) + 1;
            int position1 = Utils.findNextNewLineChar(mRxMDEditText.getText(), end);
            if (position1 == -1) {
                position1 = mRxMDEditText.length();
            }
            Editable editable = mRxMDEditText.getText();
            if (position0 >= 4 && position1 < mRxMDEditText.length() - 4) {
                boolean begin = "```".equals(editable.subSequence(Utils.safePosition(position0 - 1 - "```".length(), editable), Utils
                        .safePosition(position0 - 1, editable)).toString());
                if (begin && "```\n".equals(editable.subSequence(Utils.safePosition(position1 + 1, editable), Utils.safePosition
                        (position1 + 1 + "```\n".length(), editable)).toString())) {
                    mRxMDEditText.getText().delete(position1 + 1, position1 + 1 + "```\n".length());
                    mRxMDEditText.getText().delete(position0 - "\n```".length(), position0);
                    return;
                }
            }

            int selectedStart = mRxMDEditText.getSelectionStart();
            char c = mRxMDEditText.getText().charAt(position1 >= mRxMDEditText.length() ? mRxMDEditText.length() - 1 : position1);
            if (c == '\n') {
                mRxMDEditText.getText().insert(position1, "\n```");
            } else {
                mRxMDEditText.getText().insert(position1, "\n```\n");
            }
            mRxMDEditText.getText().insert(position0, "```\n");
            mRxMDEditText.setSelection(selectedStart + "```\n".length(), selectedStart + "```\n".length());
        } else if (end - start > 6) {
            Editable editable = mRxMDEditText.getText();
            if ("```".equals(editable.subSequence(Utils.safePosition(start, editable), Utils.safePosition(start + "```".length(),
                    editable)).toString()) &&
                    "```".equals(editable.subSequence(Utils.safePosition(end - "```".length(), editable), Utils.safePosition(end,
                            editable)).toString())) {
                int selectedStart = mRxMDEditText.getSelectionStart();
                int selectedEnd = mRxMDEditText.getSelectionEnd();
                mRxMDEditText.getText().delete(end - "\n```".length(), end);
                mRxMDEditText.getText().delete(start, start + "```\n".length());
                mRxMDEditText.setSelection(selectedStart, selectedEnd - 8);
                return;
            }

            doInlineCode();
        } else {
            doInlineCode();
        }
    }
}
