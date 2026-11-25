package com.seafile.seadroid2.ui.editor;

import android.content.Context;
import android.content.Intent;
import android.graphics.Color;
import android.os.Build;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.ViewTreeObserver;

import androidx.activity.EdgeToEdge;
import androidx.activity.OnBackPressedCallback;
import androidx.activity.SystemBarStyle;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsAnimationCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.FileIOUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.databinding.ActivityEditorBinding;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.editor.widget.HorizontalEditScrollView;
import com.seafile.seadroid2.view.PerformEdit;
import com.yydcdut.markdown.MarkdownConfiguration;
import com.yydcdut.markdown.MarkdownEditText;
import com.yydcdut.markdown.MarkdownProcessor;
import com.yydcdut.markdown.syntax.edit.EditFactory;

import org.apache.commons.io.FilenameUtils;

import java.io.File;
import java.io.IOException;
import java.util.List;

import io.reactivex.functions.Consumer;

/**
 * Known issue: This page does not preview data properly if the text data is very large.
 *
 */
public class EditorActivity extends BaseActivityWithVM<EditorViewModel> {

    private static final String TAG = "EditorActivity";

    private ActivityEditorBinding binding;
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

        // The blue Toolbar background extends into the status bar / navigation bar areas,
        // so the system bar icons must stay light.
        EdgeToEdge.enable(this,
                SystemBarStyle.dark(Color.TRANSPARENT),
                SystemBarStyle.dark(Color.TRANSPARENT));

        binding = ActivityEditorBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        initToolbar();

        applyEdgeToEdge(binding.getRoot());

        initView();

        adaptInputMethod();

        initViewModel();

        if (savedInstanceState != null) {
            localPath = savedInstanceState.getString("local_path");
            repoId = savedInstanceState.getString("repo_id");
            filePathInRepo = savedInstanceState.getString("file_path_in_repo");
        } else {
            Intent intent = getIntent();
            localPath = intent.getStringExtra("local_path");
            repoId = intent.getStringExtra("repo_id");
            filePathInRepo = intent.getStringExtra("remote_full_path");

            if (!NetworkUtils.isConnected()) {
                Toasts.show(R.string.network_unavailable);
            }
        }

        initMarkdown();

        loadData();

        if (!TextUtils.isEmpty(localPath) && getSupportActionBar() != null) {
            getSupportActionBar().setTitle(new File(localPath).getName());
        }
    }

    private void initView() {
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (isSave) {
                    finish();
                } else {
                    saveFile();
                }
            }
        });


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
                Toasts.show(R.string.editor_file_save_failed);
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
                if (TextUtils.isEmpty(s)) {
                    return;
                }

                lastContentMD5 = EncryptUtils.encryptMD5ToString(s);

                // Disable the undo listener on the initial text fill so that filling a large text
                // is not treated as user editing (which would push/reorder history entries);
                // this also avoids triggering live Markdown parsing.
                mPerformEdit.setEnabled(false);

                isSave = true;
                mPerformEdit.setDefaultText(s);
                isSave = false;

                mPerformEdit.setEnabled(true);
            }
        });
    }

    private void adaptInputMethod() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
            ViewCompat.setWindowInsetsAnimationCallback(binding.editScroll, new WindowInsetsAnimationCompat.Callback(WindowInsetsAnimationCompat.Callback.DISPATCH_MODE_STOP) {

                        //                        private boolean lastImeVisible = false;
                        private int startHeight = 0;
                        private int lastDiffH = 0;

                        @Override
                        public void onPrepare(@NonNull WindowInsetsAnimationCompat animation) {
                            if (startHeight == 0) {
                                startHeight = binding.editScroll.getHeight();
                            }
                        }

                        @NonNull
                        @Override
                        public WindowInsetsCompat onProgress(@NonNull WindowInsetsCompat insets,
                                                             @NonNull List<WindowInsetsAnimationCompat> runningAnimations) {
                            Insets imeInsets = insets.getInsets(WindowInsetsCompat.Type.ime());
                            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());

                            Insets diff = Insets.subtract(imeInsets, systemBars);
                            Insets maxDiff = Insets.max(diff, Insets.NONE);

                            int diffH = Math.abs(maxDiff.top - maxDiff.bottom);

                            ViewGroup.MarginLayoutParams layoutParams = (ViewGroup.MarginLayoutParams) binding.editScroll.getLayoutParams();
                            layoutParams.bottomMargin = diffH;
                            binding.editScroll.setLayoutParams(layoutParams);

                            lastDiffH = diffH;
                            return insets;
                        }
                    }
            );
        } else {
            // <= Android R
            binding.editScroll.getViewTreeObserver().addOnGlobalLayoutListener(new ViewTreeObserver.OnGlobalLayoutListener() {
                int lastBottom = 0;

                @Override
                public void onGlobalLayout() {
                    WindowInsetsCompat insets = ViewCompat.getRootWindowInsets(binding.editScroll);
                    if (insets != null) {
                        int bottom = insets.getInsets(WindowInsetsCompat.Type.ime()).bottom;
                        if (lastBottom != 0 && bottom == 0) {
                            binding.editScroll.getViewTreeObserver().removeOnGlobalLayoutListener(this);
                        }
                        lastBottom = bottom;
                    }
                }
            });
        }
    }

    /**
     * Markdown files larger than this size skip live syntax highlighting to avoid an ANR
     * caused by a full-text regex scan on the main thread.
     */
    private static final long MARKDOWN_LIVE_RENDER_MAX_BYTES = 50 * 1024;

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

        // Enable live syntax highlighting only for small Markdown files; otherwise every
        // keystroke re-runs a full-text regex parse on the main thread, which will definitely
        // ANR on large files.
        if (shouldEnableLiveMarkdown()) {
            mMarkdownProcessor.live(mMarkdownEditText);
        } else {
            SLogs.d(TAG, "skip live markdown rendering for large or non-markdown file");
        }

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

    private boolean shouldEnableLiveMarkdown() {
        if (TextUtils.isEmpty(localPath)) {
            return false;
        }
        String lower = localPath.toLowerCase();
        if (!lower.endsWith(".md") && !lower.endsWith(".markdown")) {
            return false;
        }
        File file = new File(localPath);
        return file.exists() && file.length() <= MARKDOWN_LIVE_RENDER_MAX_BYTES;
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
            mPerformEdit.undo();
        } else if (item.getItemId() == R.id.edit_redo) {
            mPerformEdit.redo();
        } else if (item.getItemId() == R.id.edit_save) {
            saveFile();
        }
        return super.onOptionsItemSelected(item);
    }

    private void saveFile() {
        String content = mMarkdownEditText.getText().toString();

        String md5 = EncryptUtils.encryptMD5ToString(content);
        if (TextUtils.equals(lastContentMD5, md5)) {
            //
            finish();
            return;
        }

        getViewModel().save(repoId, localPath, filePathInRepo, content, filePathInRepo);
    }


    private File lockFile = null;

    private File getLockFile() {
        if (lockFile != null) {
            return lockFile;
        }

        String path = getLockFilePath();
        lockFile = new File(path);
        return lockFile;
    }

    private String getLockFilePath() {
        File tempDir = StorageManager.getInstance().getTempDir();
        String filePath = EncryptUtils.encryptMD5ToString(localPath);
        String fileName = FilenameUtils.getName(localPath);
        String p = Utils.pathJoin(tempDir.getAbsolutePath(), filePath);
        return p + "-" + fileName + ".lock";
    }


    @Todo
    private void acquireEditLock() {
        if (TextUtils.isEmpty(localPath)) return;

        new Thread(() -> {
            try {
                File lock = getLockFile();
                if (!lock.exists()) {
                    boolean created = lock.createNewFile();
                    if (created) {
                        FileIOUtils.writeFileFromString(lock, String.valueOf(System.currentTimeMillis()));
                        SLogs.d("Editor", "Lock acquired: " + lock.getName());
                    }
                }
            } catch (IOException e) {
                SLogs.e("Editor", "Could not create lock file", e);
            }
        }).start();
    }

    @Todo
    private void releaseEditLock() {
        File lock = getLockFile();
        if (lock != null && lock.exists()) {
            boolean deleted = lock.delete();
            if (deleted) {
                SLogs.d("Editor", "Lock released: " + lock.getName());
            }
        }
    }

//    @Override
//    public void onDestroy() {
//
//        releaseEditLock();
//
//        super.onDestroy();
//    }
}
