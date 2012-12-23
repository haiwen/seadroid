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
    
    private String server;
    
    ReposFragment reposFragmgent = null;
    NavContext navContext = null;
    
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        // Get the message from the intent
        Intent intent = getIntent();
        server = intent.getStringExtra("server");
        
        super.onCreate(savedInstanceState);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        setContentView(R.layout.seadroid_main);
        getSupportFragmentManager().addOnBackStackChangedListener(this);
        navContext = new NavContext();
        
        if (findViewById(R.id.fragment_container) != null) {
            // we are in one-pane layout
            
            // if we're being restored from a previous state,
            // then we don't need to do anything and should return or else
            // we could end up with overlapping fragments.
            if (savedInstanceState != null) {
                return;
            }

            reposFragmgent = new ReposFragment();
            // In case this activity was started with special instructions from an Intent,
            // pass the Intent's extras to the fragment as arguments
            reposFragmgent.setArguments(getIntent().getExtras());
            
            // Add the fragment to the 'fragment_container' FrameLayout
            getSupportFragmentManager().beginTransaction()
                    .add(R.id.fragment_container, reposFragmgent).commit();
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
                reposFragmgent.navUp();
                return true;
        }
        return super.onOptionsItemSelected(item);
    }
    
    public void onFileSelected(String repoID, String path, String objectID) {
        // The user selected the headline of an article from the HeadlinesFragment

        // Capture the article fragment from the activity layout
        FileFragment fileFrag = (FileFragment)
                getSupportFragmentManager().findFragmentById(R.id.file_fragment);

        if (fileFrag != null) {
            // If file frag is available, we're in two-pane layout...

            // Call a method in the ArticleFragment to update its content
            fileFrag.updateFileView(repoID, path, objectID);

        } else {
            // If the frag is not available, we're in the one-pane layout and must swap frags...

            // Create fragment and give it an argument for the selected article
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

    public String getServer() {
        return server;
    }

    @Override
    public void onBackStackChanged() {    
    }
    
    public NavContext getNavContext() {
        return navContext;
    }
    
}
