package com.seafile.seadroid2.cache;

import android.content.Intent;
import android.os.Bundle;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.fragment.SettingsPreferenceFragment;

/**
 * Custom cache directory
 */
public class CacheDirectoryActivity extends SherlockFragmentActivity {
    public static final String DEBUG_TAG = "CacheDirectoryActivity";

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.cache_activity_layout);

        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        fragmentTransaction.add(R.id.cache_fragment_container, new CacheDirectoryFragment());
        fragmentTransaction.commit();

        if (getActionBar() != null)
            getActionBar().hide();

    }

    public void saveCacheDirectory(String selectedDir) {
        if (selectedDir == null) {
            return;
        }

        Intent intent = new Intent();
        intent.putExtra(SettingsPreferenceFragment.CACHE_DOWNLOAD_LOCAL_DIRECTORIE, selectedDir);

        setResult(RESULT_OK, intent);
    }
}
