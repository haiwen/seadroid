package com.seafile.seadroid2.ui;

import android.os.Bundle;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
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
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        fragmentTransaction.add(R.id.settings_fragment_container, new SettingsPreferenceFragment());
        fragmentTransaction.commit();
        
        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayHomeAsUpEnabled(true);
        
    }
    /*
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.HONEYCOMB) {
            // Load the legacy preferences headers
            addPreferencesFromResource(R.xml.preference_headers_legacy);
        }
    }

    @TargetApi(Build.VERSION_CODES.HONEYCOMB)
    @Override
    public void onBuildHeaders(List<Header> target) {
        loadHeadersFromResource(R.xml.preference_headers, target);
    }
    
    @SuppressLint("Override")
    protected boolean isValidFragment(String fragmentName) {
        return SettingsPreferenceFragment.class.getName().equals(fragmentName);

    }*/
    
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
