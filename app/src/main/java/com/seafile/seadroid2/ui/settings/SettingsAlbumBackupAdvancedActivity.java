package com.seafile.seadroid2.ui.settings;

import android.os.Bundle;
import android.view.MenuItem;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.appcompat.widget.Toolbar;
import androidx.core.os.BuildCompat;
import androidx.fragment.app.Fragment;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;

import com.blankj.utilcode.util.FragmentUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.SettingsAlbumBackupActivityLayoutBinding;
import com.seafile.seadroid2.ui.base.BaseActivity;


public class SettingsAlbumBackupAdvancedActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener, PreferenceFragmentCompat.OnPreferenceStartFragmentCallback {

    private SettingsAlbumBackupActivityLayoutBinding binding;

    @OptIn(markerClass = BuildCompat.PrereleaseSdkCheck.class)
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = SettingsAlbumBackupActivityLayoutBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        toolbar.setOnMenuItemClickListener(this);

        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(R.string.settings_camera_upload_advanced_feature_title);
        }

        FragmentUtils.add(getSupportFragmentManager(), SettingsAlbumBackupAdvancedFragment.newInstance(), R.id.settings_fragment_container);

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                setResult(RESULT_OK);
                finish();
            }
        });
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            setResult(RESULT_OK);
            this.finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onPreferenceStartFragment(@NonNull PreferenceFragmentCompat caller, @NonNull Preference pref) {
        final Bundle args = pref.getExtras();
        String f = pref.getFragment();
        final Fragment fragment = getSupportFragmentManager().getFragmentFactory().instantiate(getClassLoader(), f);
        fragment.setArguments(args);
        getSupportFragmentManager().beginTransaction()
                .replace(R.id.settings_fragment_container, fragment)
                .addToBackStack(null)
                .commit();
        return true;
    }
}
