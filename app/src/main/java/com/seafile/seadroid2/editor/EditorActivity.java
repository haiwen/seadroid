package com.seafile.seadroid2.editor;

import android.content.Intent;
import android.os.Bundle;
import android.support.v7.widget.Toolbar;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.editor.widget.HorizontalEditScrollView;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.ui.dialog.FileSaveTaskDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.yydcdut.markdown.MarkdownConfiguration;
import com.yydcdut.markdown.MarkdownEditText;
import com.yydcdut.markdown.MarkdownProcessor;
import com.yydcdut.markdown.syntax.edit.EditFactory;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import ren.qinc.edit.PerformEdit;

public class EditorActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {

    private MarkdownEditText mMarkdownEditText;
    private HorizontalEditScrollView mHorizontalEditScrollView;
    private MarkdownProcessor mMarkdownProcessor;
    private String path;
    private PerformEdit mPerformEdit;
    private boolean isSave = true;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_editor);
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);


        mMarkdownEditText = findViewById(R.id.edit_md);
        mHorizontalEditScrollView = findViewById(R.id.scroll_edit);
        findViewById(R.id.view_toolbar_bottom_line).setVisibility(View.GONE);

        Intent intent = getIntent();
        path = intent.getStringExtra("path");
        markdown();
        getSupportActionBar().setTitle(new File(path).getName());
        mMarkdownEditText.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {

            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {

            }

            @Override
            public void afterTextChanged(Editable s) {
                isSave = false;
            }
        });
    }


    private void markdown() {
        MarkdownConfiguration markdownConfiguration = new MarkdownConfiguration.Builder(this)
                .setDefaultImageSize(50, 50)
                .setBlockQuotesLineColor(0xffdddddd)
                .setHeader1RelativeSize(1.6f)
                .setHeader2RelativeSize(1.5f)
                .setHeader3RelativeSize(1.4f)
                .setHeader4RelativeSize(1.3f)
                .setHeader5RelativeSize(1.2f)
                .setHeader6RelativeSize(1.1f)
                .setHorizontalRulesColor(0xffdce1e7)
                .setCodeBgColor(0xff222B38)
                .setTodoColor(0xFF2196F3)
                .setTodoDoneColor(0xffb8b8b8)
                .setUnOrderListColor(0xFF2196F3)
                .build();

        mPerformEdit = new PerformEdit(mMarkdownEditText);
        mPerformEdit.setDefaultText(readToString(new File(path)));
        mHorizontalEditScrollView.setEditTextAndConfig(mMarkdownEditText, markdownConfiguration);
        mMarkdownProcessor = new MarkdownProcessor(this);
        mMarkdownProcessor.config(markdownConfiguration);
        mMarkdownProcessor.factory(EditFactory.create());
        mMarkdownProcessor.live(mMarkdownEditText);
    }


    public String readToString(File file) {
        try {
            if (file == null || !file.exists()) {
                return null;
            }
            FileReader reader = new FileReader(file);
            StringBuilder out = new StringBuilder();
            char[] buffer = new char[1024 * 4];
            int numRead = 0;
            while ((numRead = reader.read(buffer)) > -1) {
                out.append(String.valueOf(buffer, 0, numRead));
            }
            reader.close();
            return out.toString();
        } catch (IOException e) {
            e.printStackTrace();
        }
        return "";
    }

    @Override
    public void onDetachedFromWindow() {
        mPerformEdit.clearHistory();
        super.onDetachedFromWindow();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getActionBarToolbar().inflateMenu(R.menu.editor_view_menu);
        return true;
    }


    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                onBackPressed();
                break;
            case R.id.edit_undo:
                mPerformEdit.undo();
                break;
            case R.id.edit_redo:
                mPerformEdit.redo();
                break;
            case R.id.edit_save:
                showSaveDialog();
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

    @Override
    public void onBackPressed() {
        if (isSave) {
            super.onBackPressed();
        } else {
            showSaveDialog();
        }
    }


    private void showSaveDialog() {
        FileSaveTaskDialog dialog = new FileSaveTaskDialog();
        dialog.init(path, mMarkdownEditText);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                // save file success
                isSave = true;
                Toast.makeText(EditorActivity.this, getString(R.string.editor_file_save_success), Toast.LENGTH_SHORT).show();
                finish();
            }

            @Override
            public void onTaskFailed(SeafException e) {
                Toast.makeText(EditorActivity.this, getString(R.string.editor_file_save_failed), Toast.LENGTH_SHORT).show();
            }
        });
        dialog.show(getSupportFragmentManager(), "FileSaveTaskDialog");
    }
}
