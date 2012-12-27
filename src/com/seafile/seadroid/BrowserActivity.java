package com.seafile.seadroid;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.app.FragmentTransaction;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;


public class BrowserActivity extends SherlockFragmentActivity 
        implements ReposFragment.OnFileSelectedListener, OnBackStackChangedListener {
    
    private Account account;
    NavContext navContext = null;
    DataManager dataManager = null;
    
    private boolean twoPaneMode = false;
    
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
        
        super.onCreate(savedInstanceState);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        setContentView(R.layout.seadroid_main);
        getSupportFragmentManager().addOnBackStackChangedListener(this);
        
        navContext = new NavContext();
        dataManager = new DataManager(this, account);
        
        if (findViewById(R.id.fragment_container) != null) {
            twoPaneMode = false;
            // in one-pane layout, we have dynamic create fragments
            
            // if we're being restored from a previous state,
            // then we don't need to do anything and should return or else
            // we could end up with overlapping fragments.
            if (savedInstanceState != null) {
                return;
            }

            ReposFragment reposFragmgent = new ReposFragment();
            // In case this activity was started with special instructions from an Intent,
            // pass the Intent's extras to the fragment as arguments
            reposFragmgent.setArguments(getIntent().getExtras());
            
            // Add the fragment to the 'fragment_container' FrameLayout
            getSupportFragmentManager().beginTransaction()
                    .add(R.id.fragment_container, reposFragmgent, "repos_fragment").commit();
        } else {
            twoPaneMode = true;
            // in two pane mode, the fragments will be loaded from xml file.
        }
    }
    
    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
    
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
                
                ReposFragment reposFragment = (ReposFragment)
                    getSupportFragmentManager().findFragmentByTag("repos_fragment");
                if (reposFragment != null && reposFragment.isVisible()) {
                    reposFragment.navUp();
                    return true;
                } else {
                    getSupportFragmentManager().popBackStack();
                }
               
                return true;
        }
        return super.onOptionsItemSelected(item);
    }
    
    public void onFileSelected(String repoID, String path, SeafDirent dirent) {
        FileFragment fileFrag = (FileFragment)
                getSupportFragmentManager().findFragmentById(R.id.file_fragment);

        if (fileFrag != null) {
            // we're in two-pane layout
            fileFrag.updateFileView(repoID, path, dirent);
        } else {
            FileFragment newFragment = new FileFragment();
            Bundle args = new Bundle();
            args.putString("repoID", repoID);
            args.putString("path", path);
            args.putString("objectID", dirent.id);
            args.putLong("size", dirent.size);
            
            newFragment.setArguments(args);
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();

            // Replace whatever is in the fragment_container view with this fragment,
            // and add the transaction to the back stack so the user can navigate back
            transaction.replace(R.id.fragment_container, newFragment, "file_fragment");
            transaction.addToBackStack(null);
            transaction.commit();
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


    @Override
    public void onBackStackChanged() {    
    }
    
    @Override
    public void onBackPressed() {
        if (twoPaneMode) {
            super.onBackPressed();
            return;
        }
        
        ReposFragment reposFragment = (ReposFragment)
            getSupportFragmentManager().findFragmentByTag("repos_fragment");
        if (reposFragment != null && reposFragment.isVisible()) {
            if (navContext.currentRepo == null)
                super.onBackPressed();
            else
                reposFragment.navUp();
        } else {
            getSupportFragmentManager().popBackStack();
        }
    }
    
    
    
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
    
    public void onRefreshClick(View target) {
        ReposFragment reposFragment = (ReposFragment)
                getSupportFragmentManager().findFragmentByTag("repos_fragment");
        if (reposFragment != null && reposFragment.isVisible()) {
            if (navContext.inRepo()) {
                reposFragment.navToDirectory(navContext.getCurrentRepoID(), 
                        navContext.getCurrentPath(), null);
            } else {
                reposFragment.navToReposView();
            }
        } 
    }
    
}
