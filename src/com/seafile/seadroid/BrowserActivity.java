package com.seafile.seadroid;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.app.FragmentTransaction;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;


public class BrowserActivity extends SherlockFragmentActivity 
        implements ReposFragment.OnFileSelectedListener, OnBackStackChangedListener {
    
    private static final String DEBUG_TAG = "BrowserActivity";
    
    private Account account;
    NavContext navContext = null;
    DataManager dataManager = null;
    
    private boolean twoPaneMode = false;
    ReposFragment reposFragment = null;
    FileFragment fileFragment = null;
    
    public DataManager getDataManager() {
        return dataManager;
    }

    public Account getAccount() {
        return account;
    }
    
    public NavContext getNavContext() {
        return navContext;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        // Get the message from the intent
        Intent intent = getIntent();
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        String token = intent.getStringExtra("token");
        account = new Account(server, email, null, token);

        String repoID = intent.getStringExtra("repoID");
        String path = intent.getStringExtra("path");
        String objectID = intent.getStringExtra("objectID");
        long size = intent.getLongExtra("size", 0);
        Log.d(DEBUG_TAG, "browser activity onCreate " + server + " " + email);
        Log.d(DEBUG_TAG, "repoID " + repoID + ":" + path + ":" + objectID);
        
        super.onCreate(savedInstanceState);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        setContentView(R.layout.seadroid_main);
        getSupportFragmentManager().addOnBackStackChangedListener(this);
        
        navContext = new NavContext();
        dataManager = new DataManager(this, account);
        
        if (findViewById(R.id.fragment_container) != null) {
            twoPaneMode = false;
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
            // in one-pane layout, we have dynamic create fragments
            
            // if we're being restored from a previous state,
            // then we don't need to do anything and should return or else
            // we could end up with overlapping fragments.
            if (savedInstanceState != null) {
                return;
            }

            if (repoID != null && path != null && objectID != null) {
                // call from notification
                showFileFragment(repoID, path, objectID, size);
            } else
                showReposFragment(null, null);
        } else {
            twoPaneMode = true;
            // in two pane mode, the fragments will be loaded from xml file.
        }
    }
    
    @Override
    protected void onNewIntent(Intent intent) {
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        String token = intent.getStringExtra("token");
        account = new Account(server, email, null, token);

        String repoID = intent.getStringExtra("repoID");
        String path = intent.getStringExtra("path");
        String objectID = intent.getStringExtra("objectID");
        long size = intent.getLongExtra("size", 0);
        Log.d(DEBUG_TAG, "browser activity onNewIntent " + server + " " + email);
        Log.d(DEBUG_TAG, "repoID " + repoID + ":" + path + ":" + objectID);
        
        showFileFragment(repoID, path, objectID, size);
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
    
    private boolean isShowingReposFramgnet() {
        return (reposFragment != null && reposFragment.isVisible());
    }
    
    private void showReposFragment(String repoID, String path) {
        if (reposFragment == null)
            reposFragment = new ReposFragment();
        
        Log.d(DEBUG_TAG, "showReposFragment");
        navContext.setCurrentDirRepo(repoID);
        navContext.setCurrentDir(path);
        
        FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
        transaction.replace(R.id.fragment_container, reposFragment, "repos_fragment");
        transaction.commit();
        
        // Add the fragment to the 'fragment_container' FrameLayout
        //getSupportFragmentManager().beginTransaction()
        //        .add(R.id.fragment_container, reposFragmgent, "repos_fragment").commit();
    }
    
    private void showFileFragment(String repoID, String path,
            String objectID, long size) {
        if (fileFragment == null)
            fileFragment = new FileFragment();
        
        navContext.setCurrentFileRepo(repoID);
        navContext.setCurrentFilePath(path);
        navContext.setCurrentFileID(objectID);
        navContext.setCurrentFileSize(size);

        FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();

        // Replace whatever is in the fragment_container view with this fragment,
        // and add the transaction to the back stack so the user can navigate back
        transaction.replace(R.id.fragment_container, fileFragment, "file_fragment");
        transaction.commit();
    }
    

    public void setRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.TRUE);
    }
    
    public void unsetRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.FALSE);
    }
    
    public void showToast(CharSequence msg) {
        Context context = getApplicationContext();
        Toast toast = Toast.makeText(context, msg, Toast.LENGTH_SHORT);
        toast.show();
    }
    
    public void enableUpButton() {
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    }
    
    public void disableUpButton() {
        getSupportActionBar().setDisplayHomeAsUpEnabled(false);
    }
    
    
    /***************  Navigation *************/

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                if (twoPaneMode) {
                    ReposFragment reposFragment = (ReposFragment)
                            getSupportFragmentManager().findFragmentById(R.id.repos_fragment);
                    reposFragment.navUp();
                    return true;
                }
                
                if (isShowingReposFramgnet()) {
                    reposFragment.navUp();
                    return true;
                } else {
                    String repoID = navContext.getCurrentFileRepo();
                    String parentPath = navContext.getFileParentPath();
                    showReposFragment(repoID, parentPath);
                }
               
                return true;
        }
        return super.onOptionsItemSelected(item);
    }
    
    // File selected in repos fragment
    public void onFileSelected(String repoID, String path, SeafDirent dirent) {
        if (twoPaneMode)
            return;
        
        showFileFragment(repoID, path, dirent.id, dirent.size);
    }
    
    @Override
    public void onBackPressed() {
        if (twoPaneMode) {
            super.onBackPressed();
            return;
        }
        
        if (getSupportFragmentManager().getBackStackEntryCount() != 0)
            getSupportFragmentManager().popBackStack();

        if (isShowingReposFramgnet()) {
            if (navContext.inRepo())
                reposFragment.navUp();
            else
                // back to StartActivity
                super.onBackPressed();
        } else {
            // in showing FileFragment
            String repoID = navContext.getCurrentFileRepo();
            String filePath = navContext.getFileParentPath();
            showReposFragment(repoID, filePath);
        }
    }

    @Override
    public void onBackStackChanged() {    
    }

    
    /************** Button clicks **************/
    
    // Open file button click in file fragment
    public void onOpenFileClick(View target) {       
        if (twoPaneMode) {
            return;
        }
        
        FileFragment fileFragment = (FileFragment)
                getSupportFragmentManager().findFragmentByTag("file_fragment");
        if (fileFragment != null && fileFragment.isVisible()) {
            fileFragment.openFile();
        }
    }
    
    public void onCancelDownloadClick(View target) {
        if (twoPaneMode) {
            return;
        }
        
        FileFragment fileFragment = (FileFragment)
                getSupportFragmentManager().findFragmentByTag("file_fragment");
        if (fileFragment != null && fileFragment.isVisible()) {
            fileFragment.cancelDownload();
        }
    }
    
    public void onRefreshClick(View target) {
        ReposFragment reposFragment = (ReposFragment)
                getSupportFragmentManager().findFragmentByTag("repos_fragment");
        if (reposFragment != null && reposFragment.isVisible()) {
            if (navContext.inRepo()) {
                reposFragment.navToDirectory(navContext.getCurrentDirRepo(), 
                        navContext.getCurrentDirPath(), null);
            } else {
                reposFragment.navToReposView();
            }
        }
    }
    
}
