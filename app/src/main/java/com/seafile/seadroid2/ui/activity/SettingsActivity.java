package com.seafile.seadroid2.ui.activity;

import android.os.Bundle;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v7.widget.Toolbar;

import android.view.MenuItem;
import android.view.Window;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.fragment.SettingsFragment;
import com.seafile.seadroid2.ui.widget.swipeback.SwipeBackActivity;

public class SettingsActivity extends SwipeBackActivity implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "SettingsActivity";

    public void onCreate(Bundle savedInstanceState) {
        supportRequestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        super.onCreate(savedInstanceState);

        setContentView(R.layout.settings_activity_layout);
        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        fragmentTransaction.add(R.id.settings_fragment_container, new SettingsFragment());
        fragmentTransaction.commit();

        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        toolbar.setOnMenuItemClickListener(this);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.settings);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
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
