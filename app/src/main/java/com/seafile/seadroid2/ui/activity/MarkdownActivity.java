package com.seafile.seadroid2.ui.activity;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.support.v4.content.FileProvider;
import android.support.v7.widget.Toolbar;
import android.view.Menu;
import android.view.MenuItem;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.editor.EditorActivity;
import com.seafile.seadroid2.util.FileMimeUtils;

import java.io.File;

import br.tiagohm.markdownview.MarkdownView;
import br.tiagohm.markdownview.Utils;
import br.tiagohm.markdownview.css.InternalStyleSheet;
import br.tiagohm.markdownview.css.styles.Github;

/**
 * For showing markdown files
 */
public class MarkdownActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {

    @SuppressWarnings("unused")
    private static final String DEBUG_TAG = "MarkdownActivity";

    private MarkdownView markdownView;

    String path;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_markdown);

        Intent intent = getIntent();
        path = intent.getStringExtra("path");

        if (path == null) return;

        markdownView = findViewById(R.id.markdownView);
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    }

    @Override
    public void onResume() {
        super.onResume();
        File file = new File(path);
        if (!file.exists())
            return;

        InternalStyleSheet css = new Github();
        css.addRule("body", new String[]{"line-height: 1.6", "padding: 0px"});
        css.addRule("a", "color: orange");
        markdownView.addStyleSheet(css);
        try {
            markdownView.loadMarkdownFromFile(file);
        } catch (Exception e) {
            markdownView.loadData(Utils.getStringFromFile(file), "text/plain", "UTF-8");
            e.printStackTrace();
        }

        getSupportActionBar().setTitle(file.getName());
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
        Uri uri;
        if (android.os.Build.VERSION.SDK_INT > 23) {
            uri = FileProvider.getUriForFile(this, getPackageName() + ".provider", new File(path));
            editAsMarkDown.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION);
        } else {
            uri = Uri.parse(path);
        }

        String mime = FileMimeUtils.getMimeType(new File(path));
        editAsMarkDown.setDataAndType(uri, mime);

        if ("text/plain".equals(mime)) {
            Intent intent = new Intent(this, EditorActivity.class);
            intent.putExtra("path", path);
            startActivity(intent);
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
                showShortToast(this, getString(R.string.activity_not_found));
            }
        }
    }

}
