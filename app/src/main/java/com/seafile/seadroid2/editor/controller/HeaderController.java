package com.seafile.seadroid2.editor.controller;

import android.text.style.RelativeSizeSpan;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.editor.Utils;
import com.yydcdut.markdown.MarkdownConfiguration;
import com.yydcdut.markdown.MarkdownEditText;

public class HeaderController {

    private MarkdownEditText mRxMDEditText;
    private MarkdownConfiguration mRxMDConfiguration;

    public HeaderController(MarkdownEditText rxMDEditText, MarkdownConfiguration rxMDConfiguration) {
        mRxMDEditText = rxMDEditText;
        mRxMDConfiguration = rxMDConfiguration;
    }


    public void doHeader(int headerNumber) {
        int start = mRxMDEditText.getSelectionStart();
        int end = mRxMDEditText.getSelectionEnd();
        int position0 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), start) + 1;
        int position1 = Utils.findBeforeNewLineChar(mRxMDEditText.getText(), end) + 1;
        if (position0 == position1) {
            boolean hasCenterSpan = Utils.hasCenterSpan(mRxMDEditText, start, end);
            if (hasCenterSpan) {
                position0 = position0 + 1;
            }
            RelativeSizeSpan relativeSizeSpan = Utils.getSpans(mRxMDEditText, start, end, RelativeSizeSpan.class);
            if (relativeSizeSpan == null) {//add "#" header
                addHeaderKey(position0, headerNumber);
                return;
            }
            replace(position0, headerNumber, headerNumber, relativeSizeSpan);
        } else {//Cannot operate multiple lines
            Toast.makeText(mRxMDEditText.getContext(), R.string.editor_lines_error, Toast.LENGTH_SHORT).show();
        }
    }

    private void replace(int startPosition, int deleteHeaderNumber, int addHeaderNumber, RelativeSizeSpan relativeSizeSpan) {
        if (relativeSizeSpan.getSizeChange() == mRxMDConfiguration.getHeader1RelativeSize()) {
            deleteHeaderKey(startPosition, 1);
            mRxMDEditText.getText().removeSpan(relativeSizeSpan);
            if (deleteHeaderNumber != 1) {
                addHeaderKey(startPosition, addHeaderNumber);
            }
        } else if (relativeSizeSpan.getSizeChange() == mRxMDConfiguration.getHeader2RelativeSize()) {
            deleteHeaderKey(startPosition, 2);
            mRxMDEditText.getText().removeSpan(relativeSizeSpan);
            if (deleteHeaderNumber != 2) {
                addHeaderKey(startPosition, addHeaderNumber);
            }
        } else if (relativeSizeSpan.getSizeChange() == mRxMDConfiguration.getHeader3RelativeSize()) {
            deleteHeaderKey(startPosition, 3);
            mRxMDEditText.getText().removeSpan(relativeSizeSpan);
            if (deleteHeaderNumber != 3) {
                addHeaderKey(startPosition, addHeaderNumber);
            }
        } else if (relativeSizeSpan.getSizeChange() == mRxMDConfiguration.getHeader4RelativeSize()) {
            deleteHeaderKey(startPosition, 4);
            mRxMDEditText.getText().removeSpan(relativeSizeSpan);
            if (deleteHeaderNumber != 4) {
                addHeaderKey(startPosition, addHeaderNumber);
            }
        } else if (relativeSizeSpan.getSizeChange() == mRxMDConfiguration.getHeader5RelativeSize()) {
            deleteHeaderKey(startPosition, 5);
            mRxMDEditText.getText().removeSpan(relativeSizeSpan);
            if (deleteHeaderNumber != 5) {
                addHeaderKey(startPosition, addHeaderNumber);
            }
        } else if (relativeSizeSpan.getSizeChange() == mRxMDConfiguration.getHeader6RelativeSize()) {
            deleteHeaderKey(startPosition, 6);
            mRxMDEditText.getText().removeSpan(relativeSizeSpan);
            if (deleteHeaderNumber != 6) {
                addHeaderKey(startPosition, addHeaderNumber);
            }
        }
    }


    private void deleteHeaderKey(int startPosition, int deleteHeader) {
        switch (deleteHeader) {
            case 1:
                mRxMDEditText.getText().delete(startPosition, startPosition + "# ".length());
                break;
            case 2:
                mRxMDEditText.getText().delete(startPosition, startPosition + "## ".length());
                break;
            case 3:
                mRxMDEditText.getText().delete(startPosition, startPosition + "### ".length());
                break;
            case 4:
                mRxMDEditText.getText().delete(startPosition, startPosition + "#### ".length());
                break;
            case 5:
                mRxMDEditText.getText().delete(startPosition, startPosition + "##### ".length());
                break;
            case 6:
                mRxMDEditText.getText().delete(startPosition, startPosition + "###### ".length());
                break;
        }
    }

    private void addHeaderKey(int startPosition, int addHeader) {
        switch (addHeader) {
            case 1:
                mRxMDEditText.getText().insert(startPosition, "# ");
                break;
            case 2:
                mRxMDEditText.getText().insert(startPosition, "## ");
                break;
            case 3:
                mRxMDEditText.getText().insert(startPosition, "### ");
                break;
            case 4:
                mRxMDEditText.getText().insert(startPosition, "#### ");
                break;
            case 5:
                mRxMDEditText.getText().insert(startPosition, "##### ");
                break;
            case 6:
                mRxMDEditText.getText().insert(startPosition, "###### ");
                break;
        }
    }

}
