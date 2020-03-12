package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.util.Utils;
import com.yydcdut.markdown.MarkdownEditText;

import java.io.File;
import java.io.IOException;

class FileSaveTask extends TaskDialog.Task {
    String path;
    MarkdownEditText mMarkdownEditText;

    FileSaveTask(String path, MarkdownEditText mMarkdownEditText) {
        this.path = path;
        this.mMarkdownEditText = mMarkdownEditText;
    }

    @Override
    protected void runTask() {
        try {
            Utils.writeFile(new File(path), mMarkdownEditText.getText().toString());
        } catch (IOException e) {
            setTaskException(new SeafException(SeafException.OTHER_EXCEPTION, "File save failed"));
        }
    }
}

public class FileSaveTaskDialog extends TaskDialog {
    String path;
    MarkdownEditText mMarkdownEditText;

    public void init(String path, MarkdownEditText mMarkdownEditText) {
        this.path = path;
        this.mMarkdownEditText = mMarkdownEditText;
    }


    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_file_save, null);
        return view;
    }


    @Override
    protected void onDialogCreated(Dialog dialog) {
        super.onDialogCreated(dialog);
        dialog.setTitle(getString(R.string.editor_file_save_title));
    }


    @Override
    protected FileSaveTask prepareTask() {
        FileSaveTask task = new FileSaveTask(path, mMarkdownEditText);
        return task;
    }
}

