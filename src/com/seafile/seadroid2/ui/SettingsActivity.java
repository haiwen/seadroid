package com.seafile.seadroid2.ui;

import android.app.FragmentManager;
import android.app.FragmentTransaction;
import android.os.Bundle;
import android.util.Log;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.R;

public class SettingsActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "SettingsActivity";

    public void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "SettingsActivity.onCreate is called");
        super.onCreate(savedInstanceState);
        
        setContentView(R.layout.settings_activity_layout);
        FragmentManager fragmentManager = getFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        fragmentTransaction.add(R.id.settings_fragment_container, new SettingsPreferenceFragment());
        fragmentTransaction.commit();
        
        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayHomeAsUpEnabled(true);
        
    }
    
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
         switch (item.getItemId()) {
            case android.R.id.home:
                this.finish();
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    
}
