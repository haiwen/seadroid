package com.seafile.seadroid;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.app.FragmentTransaction;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import com.actionbarsherlock.app.ActionBar.Tab;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;
import com.actionbarsherlock.app.ActionBar;
import com.seafile.seadroid.account.Account;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.data.SeafDirent;
import com.seafile.seadroid.ui.FileFragment;
import com.seafile.seadroid.ui.ReposFragment;


public class BrowserActivity extends SherlockFragmentActivity 
        implements ReposFragment.OnFileSelectedListener, OnBackStackChangedListener {
    
    private static final String DEBUG_TAG = "BrowserActivity";
    
    private Account account;
    NavContext navContext = null;
    DataManager dataManager = null;
    
    // private boolean twoPaneMode = false;
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
    
    public class TabListener implements ActionBar.TabListener {

        private final String mTag;

        /** Constructor used each time a new tab is created.
          * @param activity  The host Activity, used to instantiate the fragment
          * @param tag  The identifier tag for the fragment
          * @param clz  The fragment's Class, used to instantiate the fragment
          */
        public TabListener(String tag) {
            mTag = tag;
        }

        @Override
        public void onTabSelected(Tab tab, FragmentTransaction ft) {
            if (mTag.equals("libraries")) {
                showLibrariesTab();
            }
           
            
        }

        @Override
        public void onTabUnselected(Tab tab, FragmentTransaction ft) {
            if (mTag.equals("libraries")) {
                hideLibrariesTab();
            }
        }

        @Override
        public void onTabReselected(Tab tab, FragmentTransaction ft) {
        }
    }

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
        Log.d(DEBUG_TAG, "browser activity onCreate " + server + " " + email);
        
        dataManager = new DataManager(this, account);
        navContext = new NavContext();
        navContext.inFileView = false;
        
        //setContentView(R.layout.seadroid_main);
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        getSupportFragmentManager().addOnBackStackChangedListener(this);
        
        ActionBar actionBar = getSupportActionBar();
        actionBar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);
        actionBar.setDisplayShowTitleEnabled(false);

        Tab tab = actionBar.newTab()
                .setText(R.string.libraries)
                .setTabListener(new TabListener("libraries"));
        actionBar.addTab(tab);

        tab = actionBar.newTab()
            .setText(R.string.cached)
            .setTabListener(new TabListener("cache"));
        actionBar.addTab(tab);

        /*
         * if (repoID != null && path != null && objectID != null) { // call
         * from notification showFileFragment(repoID, path, objectID, size); }
         * else showReposFragment(null, null);
         */
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
        
        
        if (getSupportActionBar().getSelectedNavigationIndex() != 0) {
            navContext.inFileView = true;
            navContext.setRepo(repoID);
            navContext.setFile(path, objectID, size);
            getSupportActionBar().setSelectedNavigationItem(0);   
        } else {
            if (navContext.inFileView) { 
                hideFileFragment();
                showFileFragment();
            } else {
                hideReposFragment();
                showFileFragment();
            }
        }
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
    
    private void showReposFragment() {
        navContext.inFileView = false;
        Log.d(DEBUG_TAG, "showReposFragment");
        
        if (reposFragment == null) {
            reposFragment = new ReposFragment();
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            transaction.add(android.R.id.content, reposFragment, "repos_fragment");
            transaction.commit();
        } else {
            Log.d(DEBUG_TAG, "Attach reposFragment");
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            transaction.attach(reposFragment);
            transaction.commit();
        }
    }
    
    private void hideReposFragment() {
        if (reposFragment.isDetached())
            return;
        FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
        transaction.detach(reposFragment);
        transaction.commit();
    }
    
    private void showFileFragment() {
        navContext.inFileView = true;
        
        if (fileFragment == null) {
            fileFragment = new FileFragment();
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            transaction.add(android.R.id.content, fileFragment, "file_fragment");
            transaction.commit();
        } else {
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
            transaction.attach(fileFragment);
            transaction.commit();
        }
//        
    }
    
    private void hideFileFragment() {
        FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();
        transaction.detach(fileFragment);
        transaction.commit();
    }
    
    private void showLibrariesTab() {
        if (navContext.inFileView) {
            showFileFragment();
        } else {
            showReposFragment();
        }
    }
    
    private void hideLibrariesTab() {
        if (navContext.inFileView) {
            hideFileFragment();
        } else {
            hideReposFragment();
        }
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
                
                if (!navContext.inFileView) {
                    if (navContext.isRepoRoot()) {
                        navContext.setRepo(null);
                    } else {
                        String parentPath = Utils.getParentPath(navContext.getDirPath());
                        navContext.setDir(parentPath, null);
                    }
                    reposFragment.refreshView();
                    return true;
                } else {
                    String parentPath = Utils.getParentPath(navContext.getFilePath());
                    navContext.setDir(parentPath, null);
                    hideFileFragment();
                    showReposFragment();
                }
               
                return true;
        }
        return super.onOptionsItemSelected(item);
    }
    
    // File selected in repos fragment
    public void onFileSelected(String repoID, String path, SeafDirent dirent) {
        navContext.setFile(path, dirent.id, dirent.size);
        hideReposFragment();
        showFileFragment();
    }
    
    @Override
    public void onBackPressed() {
        if (getSupportFragmentManager().getBackStackEntryCount() != 0)
            getSupportFragmentManager().popBackStack();

        if (!navContext.inFileView) {
            if (navContext.inRepo()) {
                if (navContext.isRepoRoot()) {
                    navContext.setRepo(null);
                } else {
                    String parentPath = Utils.getParentPath(navContext.getDirPath());
                    navContext.setDir(parentPath, null);
                }
                reposFragment.refreshView();
            } else
                // back to StartActivity
                super.onBackPressed();
        } else {
            // in showing FileFragment
            String parentPath = Utils.getParentPath(navContext.getFilePath());
            navContext.setDir(parentPath, null);
            hideFileFragment();
            showReposFragment();
        }
    }

    @Override
    public void onBackStackChanged() {    
    }

    
    /************** Button clicks **************/
    
    // Open file button click in file fragment
    public void onOpenFileClick(View target) {       

        FileFragment fileFragment = (FileFragment)
                getSupportFragmentManager().findFragmentByTag("file_fragment");
        if (fileFragment != null && fileFragment.isVisible()) {
            fileFragment.openFile();
        }
    }
    
    public void onCancelDownloadClick(View target) {
        FileFragment fileFragment = (FileFragment)
                getSupportFragmentManager().findFragmentByTag("file_fragment");
        if (fileFragment != null && fileFragment.isVisible()) {
            fileFragment.cancelDownload();
        }
    }
    
    public void onRefreshClick(View target) {
        reposFragment.refreshView();
    }
    
}
