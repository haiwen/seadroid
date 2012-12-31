package com.seafile.seadroid;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.support.v4.app.FragmentTransaction;
import android.util.Log;
import android.view.View;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.app.ActionBar.Tab;
import com.actionbarsherlock.view.Window;
import com.seafile.seadroid.BrowserActivity.TabListener;
import com.seafile.seadroid.account.Account;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.ui.FileFragment;
import com.seafile.seadroid.ui.ReposFragment;

public class FileActivity extends SherlockFragmentActivity {
    
    private static final String DEBUG_TAG = "FileActivity";
    
    private DataManager dataManager = null;
    FileFragment fileFragment = null;
    private Account account;
    
    String repoID;
    String path;
    String fileID;
    long size;
    
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        super.onCreate(savedInstanceState);
        
        // Get the message from the intent
        Intent intent = getIntent();
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        String token = intent.getStringExtra("token");
        account = new Account(server, email, null, token);
        Log.d(DEBUG_TAG, "file activity onCreate " + server + " " + email);
        
        repoID = intent.getStringExtra("repoID");
        path = intent.getStringExtra("path");
        fileID = intent.getStringExtra("fileID");
        size = intent.getLongExtra("size", 0);
        
        dataManager = new DataManager(this, account);
        
        //setContentView(R.layout.seadroid_main);
        //setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        
        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayShowTitleEnabled(true);
        setSupportProgressBarIndeterminateVisibility(Boolean.FALSE);
        
        if (savedInstanceState != null) {
            // fragment are saved during screen rotation, so do not need to create a new one
            fileFragment = (FileFragment)
                    getSupportFragmentManager().findFragmentByTag("file_fragment");
        } else
            fileFragment = new FileFragment();
        showFileFragment();
    }
    
    @Override
    protected void onNewIntent(Intent intent) {
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        String token = intent.getStringExtra("token");
        account = new Account(server, email, null, token);

        String repoID = intent.getStringExtra("repoID");
        String path = intent.getStringExtra("path");
        String objectID = intent.getStringExtra("fileID");
        long size = intent.getLongExtra("size", 0);
        Log.d(DEBUG_TAG, "browser activity onNewIntent " + server + " " + email);
        Log.d(DEBUG_TAG, "repoID " + repoID + ":" + path + ":" + objectID);
        
        
        if (getSupportActionBar().getSelectedNavigationIndex() != 0) {
            //
        } else {
            //
        }
    }
    
    private void showFileFragment() {
        FragmentTransaction ft = getSupportFragmentManager().beginTransaction();
        fileFragment.setDataManager(dataManager);
        fileFragment.setFile(repoID, path, fileID, size);
        ft.add(android.R.id.content, fileFragment, "file_fragment");
        ft.commit();
    }
    
    // Open file button click in file fragment
    public void onOpenFileClick(View target) {       
        fileFragment.openFile();
    }
    
    public void onCancelDownloadClick(View target) {
        fileFragment.cancelDownload();
    }
    
}
