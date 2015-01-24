package com.seafile.seadroid2.ui.activity;

import android.os.Bundle;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.util.Log;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.fragment.SettingsPreferenceFragment;

public class SettingsActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "SettingsActivity";

    public void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "SettingsActivity.onCreate is called");
        super.onCreate(savedInstanceState);
        // This has to be called before setContentView and you must use the
        // class in android.support.v4.view and NOT android.view
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);

        setContentView(R.layout.settings_activity_layout);
        FragmentManager fragmentManager = getSupportFragmentManager();
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
