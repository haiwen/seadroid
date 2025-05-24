package com.seafile.seadroid2.ui.editor;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.blankj.utilcode.util.ToastUtils;
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

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("local_path", localPath);
        outState.putString("repo_id", repoId);
        outState.putString("file_path_in_repo", filePathInRepo);

        if (mMarkdownEditText != null) {
            outState.putString("edit_content", mMarkdownEditText.getText().toString());
        }
    }

    public static void start(Context context, String localPath, String repoId, String filePathInRepo) {
        Intent starter = new Intent(context, EditorActivity.class);
        starter.putExtra("local_path", localPath);
        starter.putExtra("remote_full_path", filePathInRepo);
        starter.putExtra("repo_id", repoId);
        context.startActivity(starter);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_editor);

        initView();
        initViewModel();
        initMarkdown();

        if (savedInstanceState != null) {
            localPath = savedInstanceState.getString("local_path");
            repoId = savedInstanceState.getString("repo_id");
            filePathInRepo = savedInstanceState.getString("file_path_in_repo");
            String editContent = savedInstanceState.getString("edit_content");
            if (!TextUtils.isEmpty(editContent)) {
                mMarkdownEditText.setText(editContent);
            }


        } else {
            Intent intent = getIntent();
            localPath = intent.getStringExtra("local_path");
            repoId = intent.getStringExtra("repo_id");
            filePathInRepo = intent.getStringExtra("remote_full_path");

            if (!NetworkUtils.isConnected()) {
                ToastUtils.showLong(R.string.network_unavailable);
            }

            loadData();
        }

        getSupportActionBar().setTitle(new File(localPath).getName());

    }

    private void initView(){
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
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

        mMarkdownEditText = findViewById(R.id.edit_md);
        mHorizontalEditScrollView = findViewById(R.id.scroll_edit);

    }
    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    showLoadingDialog();
                } else {
                    dismissLoadingDialog();
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

    private void initMarkdown() {
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
        if (item.getItemId() == R.id.edit_undo) {
            getOnBackPressedDispatcher().onBackPressed();
        } else if (item.getItemId() == R.id.edit_undo) {
            mPerformEdit.undo();
        } else if (item.getItemId() == R.id.edit_redo) {
            mPerformEdit.redo();
        } else if (item.getItemId() == R.id.edit_save) {
            saveFile();
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
