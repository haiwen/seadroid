package com.seafile.seadroid2.ui.activity;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.util.Utils;
import us.feras.mdv.MarkdownView;

import java.io.File;

/*
 * For showing markdown files
 */
public class MarkdownActivity extends SherlockActivity {

    @SuppressWarnings("unused")
    private static final String DEBUG_TAG = "MarkdownActivity";

    private MarkdownView markdownView;

    String path;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.start);
        markdownView = new MarkdownView(this);
        setContentView(markdownView);

        Intent intent = getIntent();
        path = intent.getStringExtra("path");

        if (path == null)
            return;
        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayHomeAsUpEnabled(true);
        
    }
    
    @Override
    public void onResume() {
        super.onResume();
        File file = new File(path);
        if (!file.exists())
            return;

        String content = Utils.readFile(file);
        markdownView.loadMarkdown(content);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getSupportMenuInflater();
        inflater.inflate(R.menu.markdown_view_menu, menu);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
        case R.id.edit_markdown:
            edit();
            return true;
        case android.R.id.home:
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void edit() {
        PackageManager pm = getPackageManager();

        // First try to find an activity who can handle markdown edit
        Intent editAsMarkDown = new Intent(Intent.ACTION_EDIT);

        Uri uri = Uri.fromFile(new File(path));
        String mime = "text/markdown";
        editAsMarkDown.setDataAndType(uri, mime);

        if (pm.queryIntentActivities(editAsMarkDown, 0).size() > 0) {
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
                ToastUtils.show(this, getString(R.string.activity_not_found));
            }
        }
    }

}
