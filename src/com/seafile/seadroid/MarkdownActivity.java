package com.seafile.seadroid;

import java.io.File;
import java.util.List;

import com.seafile.seadroid.data.DataManager;

import us.feras.mdv.MarkdownView;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;


/*
 * For showing markdown files
 */
public class MarkdownActivity extends Activity {

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
    

}
