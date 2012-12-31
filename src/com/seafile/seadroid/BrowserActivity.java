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
import com.seafile.seadroid.data.SeafCachedFile;
import com.seafile.seadroid.data.SeafDirent;
import com.seafile.seadroid.ui.CacheFragment;
import com.seafile.seadroid.ui.CacheFragment.OnCachedFileSelectedListener;
import com.seafile.seadroid.ui.FileFragment;
import com.seafile.seadroid.ui.ReposFragment;


public class BrowserActivity extends SherlockFragmentActivity 
        implements ReposFragment.OnFileSelectedListener, OnBackStackChangedListener, 
            OnCachedFileSelectedListener {
    
    private static final String DEBUG_TAG = "BrowserActivity";
    
    private Account account;
    NavContext navContext = null;
    DataManager dataManager = null;
    
    // private boolean twoPaneMode = false;
    ReposFragment reposFragment = null;
    CacheFragment cacheFragment = null;
    
    private String currentTab;
    
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
            currentTab = mTag;
            if (mTag.equals("libraries")) {
                showReposFragment(ft);
            } else if (mTag.equals("cache")) {
                showCacheFragment(ft);
            }
           
        }

        @Override
        public void onTabUnselected(Tab tab, FragmentTransaction ft) {
            if (mTag.equals("libraries")) {
                hideReposFragment(ft);
            } else if (mTag.equals("cache")) {
                hideCacheFragment(ft);
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
            //
        } else {
            //
        }
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
    
    private void showReposFragment(FragmentTransaction ft) {
        Log.d(DEBUG_TAG, "showReposFragment");
        
        if (reposFragment == null) {
            reposFragment = new ReposFragment();
            ft.add(android.R.id.content, reposFragment, "repos_fragment");
        } else {
            Log.d(DEBUG_TAG, "Attach reposFragment");
            ft.attach(reposFragment);
        }
    }
    
    private void hideReposFragment(FragmentTransaction ft) {
        if (reposFragment.isDetached())
            return;
        ft.detach(reposFragment);
    }
    
    
    private void showCacheFragment(FragmentTransaction ft) {
        if (cacheFragment == null) {
            cacheFragment = new CacheFragment();
            ft.add(android.R.id.content, cacheFragment, "cache_fragment");
        } else {
            ft.attach(cacheFragment);
        }
    }
    
    private void hideCacheFragment(FragmentTransaction ft) {
        if (cacheFragment.isDetached())
            return;
        ft.detach(cacheFragment);
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
            if (navContext.isRepoRoot()) {
                navContext.setRepo(null);
            } else {
                String parentPath = Utils
                        .getParentPath(navContext.getDirPath());
                navContext.setDir(parentPath, null);
            }
            reposFragment.refreshView();

            return true;
        }
        return super.onOptionsItemSelected(item);
    }
    
    void startFileActivity(String repoID, String path, String fileID, long size) {
        Intent intent = new Intent(this, FileActivity.class);
        intent.putExtra("server", account.server);
        intent.putExtra("email", account.email);
        intent.putExtra("token", account.token);
        intent.putExtra("repoID", repoID);
        intent.putExtra("path", path);
        intent.putExtra("fileID", fileID);
        intent.putExtra("size", size);
        startActivity(intent);
    }
    
    // File selected in repos fragment
    public void onFileSelected(String repoID, String path, SeafDirent dirent) {
        startFileActivity(repoID, path, dirent.id, dirent.size);
    }
    
    @Override
    public void onCachedFileSelected(SeafCachedFile item) {
        startFileActivity(item.repo, item.path, item.fileID, item.getSize());
    }
    
    @Override
    public void onBackPressed() {
        if (getSupportFragmentManager().getBackStackEntryCount() != 0) {
            getSupportFragmentManager().popBackStack();
            return;
        }
        

        if (currentTab.equals("libraries")) {
            if (navContext.inRepo()) {
                if (navContext.isRepoRoot()) {
                    navContext.setRepo(null);
                } else {
                    String parentPath = Utils.getParentPath(navContext
                            .getDirPath());
                    navContext.setDir(parentPath, null);
                }
                reposFragment.refreshView();
            } else
                // back to StartActivity
                super.onBackPressed();
        } else if (currentTab.equals("cache")) {
            super.onBackPressed();
        }
        super.onBackPressed();
        return;
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
