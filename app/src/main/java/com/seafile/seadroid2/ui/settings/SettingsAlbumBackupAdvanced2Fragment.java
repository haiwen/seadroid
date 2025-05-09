package com.seafile.seadroid2.ui.settings;

import static android.app.Activity.RESULT_OK;

import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.text.TextUtils;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.preference.Preference;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.preferences.RenameSharePreferenceFragmentCompat;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.camera_upload.AlbumBackupAdapter;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadConfigActivity;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;
import com.seafile.seadroid2.widget.prefs.TextSwitchPreference;
import com.seafile.seadroid2.widget.prefs.TextTitleSummaryPreference;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

public class SettingsAlbumBackupAdvanced2Fragment extends RenameSharePreferenceFragmentCompat {

    private final Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();

    private final Object[] initSettingsArray = new Object[4];
    private final Object[] compareSettingsArray = new Object[4];

    public static SettingsAlbumBackupAdvanced2Fragment newInstance() {
        return new SettingsAlbumBackupAdvanced2Fragment();
    }

    @Override
    public String getSharePreferenceSuffix() {
        if (currentAccount != null) {
            return currentAccount.getEncryptSignature();
        }
        return null;
    }

    @Override
    public void onCreatePreferences(@Nullable Bundle savedInstanceState, @Nullable String rootKey) {
        super.onCreatePreferences(savedInstanceState, rootKey);

        setPreferencesFromResource(R.xml.prefs_settings_camera_backup_advance_2, rootKey);
    }

    private boolean isFirstLoadData = true;

    @Override
    public void onResume() {
        super.onResume();
        if (isFirstLoadData) {

            onFirstResume();

            isFirstLoadData = false;
        }
    }

    public void onFirstResume() {

        initPrefView();

        initPrefLiveData();

        // delay updates to avoid flickering
        new Handler().postDelayed(new Runnable() {
            @Override
            public void run() {

                dataSwitch.setChecked(dataSwitch.isChecked());
                videoSwitch.setChecked(videoSwitch.isChecked());

                updateSelectedBucketIdsSummary();

                initSettingsArray[0] = dataSwitch.isChecked();
                initSettingsArray[1] = videoSwitch.isChecked();
                initSettingsArray[2] = bucketsSwitch.isChecked();
                initSettingsArray[3] = selectedBucketPref.getSummary();
            }
        }, 500);
    }

    private TextSwitchPreference bucketsSwitch, dataSwitch, videoSwitch;
    private TextTitleSummaryPreference selectedBucketPref;

    private void initPrefView() {
        bucketsSwitch = findPreference(getString(R.string.pref_key_album_backup_advanced_buckets_switch));
        dataSwitch = findPreference(getString(R.string.pref_key_album_backup_advanced_data_plan_switch));
        videoSwitch = findPreference(getString(R.string.pref_key_album_backup_advanced_allow_video_switch));

        //
        selectedBucketPref = findPreference(getString(R.string.pref_key_album_backup_advanced_buckets_select));
        if (selectedBucketPref != null) {
            selectedBucketPref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    launchAlbumSelect();
                    return true;
                }
            });
        }
    }

    private void initPrefLiveData() {
        Settings.ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean isChecked) {
                SLogs.d(SettingsAlbumBackupAdvanced2Fragment.class, "AlbumBackup-Advance-DataPlan：" + isChecked);
            }
        });

        Settings.ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean isChecked) {
                SLogs.d(SettingsAlbumBackupAdvanced2Fragment.class, "AlbumBackup-Advance-Video：" + isChecked);
            }
        });

        Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean isChecked) {
                SLogs.d(SettingsAlbumBackupAdvanced2Fragment.class, "AlbumBackup-Advance-CustomBucket：" + isChecked);

                switchBucket(isChecked);
            }
        });

        Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SELECT.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.d(SettingsAlbumBackupAdvanced2Fragment.class, "AlbumBackup-Advance-BucketList：" + s);
                if (isFirstLoadData) {
                    return;
                }

                selectedBucketPref.setSummary(s);
            }
        });
    }

    protected boolean isSettingsChanged() {
        compareSettingsArray[0] = dataSwitch.isChecked();
        compareSettingsArray[1] = videoSwitch.isChecked();
        compareSettingsArray[2] = bucketsSwitch.isChecked();
        compareSettingsArray[3] = selectedBucketPref.getSummary();

        for (int i = 0; i < initSettingsArray.length; i++) {
            if (!initSettingsArray[i].equals(compareSettingsArray[i])) {
                return true;
            }
        }
        return false;
    }


    private void switchBucket(boolean isChecked) {
        bucketsSwitch.setChecked(isChecked);

        if (!isChecked) {
            selectedBucketPref.setSummary(R.string.settings_camera_upload_dir_auto_scan);
            AlbumBackupSharePreferenceHelper.writeBucketIds(null);
        }

        initBucket(isChecked);
    }

    private void initBucket(boolean isChecked) {
        if (isChecked) {
            bucketsSwitch.setDividerPosition(2);
            bucketsSwitch.setRadiusPosition(0);
        } else {
            bucketsSwitch.setDividerPosition(0);
            bucketsSwitch.setRadiusPosition(3);
        }

        selectedBucketPref.setVisible(isChecked);
    }

    private void launchAlbumSelect() {
        Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
        intent.putExtra(CameraUploadConfigActivity.CAMERA_UPLOAD_LOCAL_DIRECTORIES, true);
        selectAlbumLauncher.launch(intent);
    }

    private final ActivityResultLauncher<Intent> selectAlbumLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {

            if (o.getResultCode() != RESULT_OK) {
                return;
            }

            updateSelectedBucketIdsSummary();
        }
    });


    private void updateSelectedBucketIdsSummary() {
        List<String> bucketNames = new ArrayList<>();

        List<String> bucketIds = AlbumBackupSharePreferenceHelper.readBucketIds();
        List<GalleryBucketUtils.Bucket> tempBuckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
        if (tempBuckets == null) {
            return;
        }

        LinkedHashSet<GalleryBucketUtils.Bucket> bucketsSet = new LinkedHashSet<>(tempBuckets.size());
        bucketsSet.addAll(tempBuckets);
        List<GalleryBucketUtils.Bucket> allBuckets = new ArrayList<>(bucketsSet.size());
        allBuckets.addAll(bucketsSet);

        for (GalleryBucketUtils.Bucket bucket : allBuckets) {
            if (bucketIds.contains(bucket.bucketId)) {
                bucketNames.add(bucket.bucketName);
            }
        }

        if (bucketNames.isEmpty()) {
            selectedBucketPref.setSummary(R.string.settings_camera_upload_dir_auto_scan);
            bucketsSwitch.setChecked(false);

            initBucket(false);
        } else {
            selectedBucketPref.setSummary(TextUtils.join(", ", bucketNames));

            bucketsSwitch.setChecked(true);

            initBucket(true);
        }
    }
}
