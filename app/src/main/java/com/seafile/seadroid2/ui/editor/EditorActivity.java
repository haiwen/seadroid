package com.seafile.seadroid2.ui.editor;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.widget.Toast;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.Nullable;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.editor.widget.HorizontalEditScrollView;
import com.seafile.seadroid2.view.PerformEdit;
import com.yydcdut.markdown.MarkdownConfiguration;
import com.yydcdut.markdown.MarkdownEditText;
import com.yydcdut.markdown.MarkdownProcessor;
import com.yydcdut.markdown.syntax.edit.EditFactory;

import java.io.File;

import io.reactivex.functions.Consumer;

public class EditorActivity extends BaseActivityWithVM<EditorViewModel> implements Toolbar.OnMenuItemClickListener {

    private MarkdownEditText mMarkdownEditText;
    private HorizontalEditScrollView mHorizontalEditScrollView;
    private MarkdownProcessor mMarkdownProcessor;
    private String localPath, repoId, filePathInRepo;
    private PerformEdit mPerformEdit;
    private boolean isSave = true;
    private String lastContentMD5 = null;

    public static void start(Context context, String localPath, String repoId, String filePathInRepo) {
        Intent starter = new Intent(context, EditorActivity.class);
        starter.putExtra("path", localPath);
        starter.putExtra("full_path", filePathInRepo);
        starter.putExtra("repo_id", repoId);
        context.startActivity(starter);
    }

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

        Intent intent = getIntent();
        localPath = intent.getStringExtra("path");
        repoId = intent.getStringExtra("repo_id");
        filePathInRepo = intent.getStringExtra("full_path");

        getSupportActionBar().setTitle(new File(localPath).getName());

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (isSave) {
                    finish();
                } else {
                    saveFile();
                }
            }
        });
    }

    @Override
    protected void onPostCreate(@Nullable Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);
        initViewModel();
        markdown();
        loadData();
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    showProgressDialog();
                } else {
                    dismissProgressDialog();
                }
            }
        });

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException seafException) {
                SLogs.e(seafException);
                ToastUtils.showLong(R.string.editor_file_save_failed);
            }
        });

        getViewModel().getFileIdLiveData().observe(this, new Observer<String>() {
            @Override
            public void onChanged(String s) {
                isSave = true;
//                Toast.makeText(EditorActivity.this, getString(R.string.editor_file_save_success), Toast.LENGTH_SHORT).show();
                finish();
            }
        });
    }

    private void loadData() {
        getViewModel().read(localPath, new Consumer<String>() {
            @Override
            public void accept(String s) throws Exception {
                if (!TextUtils.isEmpty(s)) {
                    lastContentMD5 = EncryptUtils.encryptMD5ToString(s);

                    mPerformEdit.setDefaultText(s);
                }
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


        mHorizontalEditScrollView.setEditTextAndConfig(mMarkdownEditText, markdownConfiguration);
        mMarkdownProcessor = new MarkdownProcessor(this);
        mMarkdownProcessor.config(markdownConfiguration);
        mMarkdownProcessor.factory(EditFactory.create());
        mMarkdownProcessor.live(mMarkdownEditText);


        mPerformEdit = new PerformEdit(mMarkdownEditText);


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

    private Dialog dialog;

    private void showProgressDialog() {

        if (dialog == null) {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
            builder.setView(R.layout.layout_dialog_progress_bar);
            dialog = builder.create();
        }

        dialog.show();
    }

    private void dismissProgressDialog() {
        if (dialog != null) {
            dialog.dismiss();
        }
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
                getOnBackPressedDispatcher().onBackPressed();
                break;
            case R.id.edit_undo:
                mPerformEdit.undo();
                break;
            case R.id.edit_redo:
                mPerformEdit.redo();
                break;
            case R.id.edit_save:
                saveFile();
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }


    private void saveFile() {
        String content = mMarkdownEditText.getText().toString();

        String md5 = EncryptUtils.encryptMD5ToString(content);
        if (TextUtils.equals(lastContentMD5, md5)) {
            finish();
            return;
        }

        getViewModel().save(repoId, localPath, filePathInRepo, content, filePathInRepo);
    }
}
