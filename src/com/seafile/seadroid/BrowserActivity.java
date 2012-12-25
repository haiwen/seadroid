package com.seafile.seadroid;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.app.FragmentTransaction;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;


public class BrowserActivity extends SherlockFragmentActivity 
        implements ReposFragment.OnFileSelectedListener, OnBackStackChangedListener {
    
    private Account account;
    NavContext navContext = null;
    
    private boolean twoPaneMode = false;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        // Get the message from the intent
        Intent intent = getIntent();
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        account = new Account(server, email);
        
        super.onCreate(savedInstanceState);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        setContentView(R.layout.seadroid_main);
        getSupportFragmentManager().addOnBackStackChangedListener(this);
        navContext = new NavContext();
        
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
    
    public void onFileSelected(String repoID, String path, String objectID) {
        FileFragment fileFrag = (FileFragment)
                getSupportFragmentManager().findFragmentById(R.id.file_fragment);

        if (fileFrag != null) {
            // we're in two-pane layout
            fileFrag.updateFileView(repoID, path, objectID);
        } else {
            FileFragment newFragment = new FileFragment();
            Bundle args = new Bundle();
            args.putString("repoID", repoID);
            args.putString("path", path);
            args.putString("objectID", objectID);
            
            newFragment.setArguments(args);
            FragmentTransaction transaction = getSupportFragmentManager().beginTransaction();

            // Replace whatever is in the fragment_container view with this fragment,
            // and add the transaction to the back stack so the user can navigate back
            transaction.replace(R.id.fragment_container, newFragment);
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

    public Account getAccount() {
        return account;
    }

    @Override
    public void onBackStackChanged() {    
    }
    
    public NavContext getNavContext() {
        return navContext;
    }
    
}
