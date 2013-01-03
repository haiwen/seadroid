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

    String repoID;
    String path;
    String fileID;
    
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.start);
        MarkdownView markdownView = new MarkdownView(this);
        setContentView(markdownView);
        
        Intent intent = getIntent();
        repoID = intent.getStringExtra("repoID");
        path = intent.getStringExtra("path");
        fileID = intent.getStringExtra("fileID");
        
        if (path == null || fileID == null)
            return;
        
        File file = DataManager.getFileForFileCache(path, fileID);
        if (!file.exists())
            return;
        
        Log.d(DEBUG_TAG, "" + file.getAbsolutePath());
        String content = Utils.readFile(file);
        markdownView.loadMarkdown(content);
    }
    

}
