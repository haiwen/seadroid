package com.seafile.seadroid2;

import java.io.File;

import us.feras.mdv.MarkdownView;

import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.os.Bundle;
import android.util.Log;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;


/*
 * For showing markdown files
 */
public class MarkdownActivity extends SherlockActivity {

    private static final String DEBUG_TAG = "MarkdownActivity";

    String path;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.start);
        MarkdownView markdownView = new MarkdownView(this);
        setContentView(markdownView);

        Intent intent = getIntent();
        path = intent.getStringExtra("path");

        if (path == null)
            return;

        File file = new File(path);
        if (!file.exists())
            return;

        Log.d(DEBUG_TAG, "" + file.getAbsolutePath());
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
        }
        return super.onOptionsItemSelected(item);
    }

    private void edit() {
        PackageManager pm = getPackageManager();

        // First try to find an activity who can handle markdown edit
        Intent editAsMarkDown = new Intent(Intent.ACTION_VIEW, Uri.parse(path));
        editAsMarkDown.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        editAsMarkDown.setAction(Intent.ACTION_EDIT);

        Uri uri = Uri.fromFile(new File(path));
        String mime = "text/markdown";
        editAsMarkDown.setDataAndType(uri, mime);

        if (pm.queryIntentActivities(editAsMarkDown, 0).size() > 0) {
            // Some activity can edit markdown
            startActivity(editAsMarkDown);
        } else {
            // No activity to handle markdown, take it as text
            Intent editAsText = new Intent(Intent.ACTION_EDIT);
            editAsText.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            editAsText.setAction(Intent.ACTION_EDIT);
            mime = "text/plain";
            editAsMarkDown.setDataAndType(uri, mime);

            try {
                startActivity(editAsText);
            } catch (ActivityNotFoundException e) {
                showToast(getString(R.string.activity_not_found));
            }
        }
    }

    private void showToast(CharSequence msg) {
        Context context = getApplicationContext();
        Toast toast = Toast.makeText(context, msg, Toast.LENGTH_SHORT);
        toast.show();
    }
}
