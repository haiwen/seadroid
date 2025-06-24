package com.seafile.seadroid2.ui.markdown;

import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.view.Menu;
import android.view.MenuItem;

import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.core.content.FileProvider;

import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.FileMimeUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.editor.EditorActivity;
import com.seafile.seadroid2.ui.editor.EditorViewModel;

import java.io.File;

import br.tiagohm.markdownview.MarkdownView;
import br.tiagohm.markdownview.css.InternalStyleSheet;
import br.tiagohm.markdownview.css.styles.Github;
import io.reactivex.functions.Consumer;

/**
 * For showing markdown files
 */
public class MarkdownActivity extends BaseActivityWithVM<EditorViewModel> implements Toolbar.OnMenuItemClickListener {

    private MarkdownView markdownView;

    private String path, repoId, fullPathInRemote;

    public static void start(Context context, String localPath, String repoId, String target_file) {
        Intent starter = new Intent(context, MarkdownActivity.class);
        starter.putExtra("local_path", localPath);
        starter.putExtra("full_path", target_file);
        starter.putExtra("repo_id", repoId);
        context.startActivity(starter);
    }

    @Override
    protected void onSaveInstanceState(@NonNull Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putString("local_path", path);
        outState.putString("repo_id", repoId);
        outState.putString("full_path", fullPathInRemote);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_markdown);

        if (savedInstanceState != null) {
            path = savedInstanceState.getString("local_path");
            repoId = savedInstanceState.getString("repo_id");
            fullPathInRemote = savedInstanceState.getString("full_path");
        } else {
            Intent intent = getIntent();
            path = intent.getStringExtra("local_path");
            repoId = intent.getStringExtra("repo_id");
            fullPathInRemote = intent.getStringExtra("full_path");
        }

        if (path == null) return;

        markdownView = findViewById(R.id.markdownView);
//        markdownView.setWebViewClient(new ImageLoadWebViewClient());
        initMarkdown();

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(FileUtils.getFileName(path));

        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_unavailable);
        }
    }

    private void initMarkdown() {

        InternalStyleSheet css = new Github();

        int mode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        css.addRule("a", "color: orange");
        if (mode == Configuration.UI_MODE_NIGHT_YES) {
            css.addRule("body", new String[]{"line-height: 1.6", "padding: 0px", "background-color: #222;"});
            css.addRule("body", "color: white");
        } else {
            css.addRule("body", new String[]{"line-height: 1.6", "padding: 0px"});
        }

        markdownView.addStyleSheet(css);
    }

    @Override
    public void onResume() {
        super.onResume();

        loadMarkContent();
    }

    private void loadMarkContent() {

        getViewModel().read(path, new Consumer<String>() {
            @Override
            public void accept(String s) {
                markdownView.loadMarkdown(s);
            }
        });
    }


    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        getActionBarToolbar().inflateMenu(R.menu.markdown_view_menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                finish();
                break;
            case R.id.edit_markdown:
                edit();
                break;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

    private void edit() {
        PackageManager pm = getPackageManager();

        // First try to find an activity who can handle markdown edit
        Intent editAsMarkDown = new Intent(Intent.ACTION_EDIT);
        Uri uri = FileProvider.getUriForFile(this, BuildConfig.FILE_PROVIDER_AUTHORITIES, new File(path));
        editAsMarkDown.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);

        String mime = FileMimeUtils.getMimeType(new File(path));
        editAsMarkDown.setDataAndType(uri, mime);

        if ("text/plain".equals(mime)) {
            EditorActivity.start(this, path, repoId, fullPathInRemote);
        } else if (pm.queryIntentActivities(editAsMarkDown, 0).size() > 0) {
            // Some activity can edit markdown
            startActivity(editAsMarkDown);
        } else {
            // No activity to handle markdown, take it as text
            Intent editAsText = new Intent(Intent.ACTION_EDIT);
            mime = "text/plain";
            editAsText.setDataAndType(uri, mime);

            try {
                startActivity(editAsText);
            } catch (ActivityNotFoundException e) {
                Toasts.show(R.string.activity_not_found);
            }
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        if (markdownView != null) {
            markdownView.loadUrl("about:blank");
            markdownView.stopLoading();
            markdownView.destroy();
        }
    }
}
