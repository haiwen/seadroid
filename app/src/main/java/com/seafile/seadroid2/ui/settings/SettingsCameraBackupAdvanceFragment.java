package com.seafile.seadroid2.ui.settings;

import static android.app.Activity.RESULT_OK;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.SwitchPreferenceCompat;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadConfigActivity;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

public class SettingsCameraBackupAdvanceFragment extends PreferenceFragmentCompat {

    private SwitchPreferenceCompat mCameraBackupCustomBucketsSwitch;
    private SwitchPreferenceCompat cbDataPlan;
    private SwitchPreferenceCompat cbVideoAllowed;
    private Preference mCameraBackupLocalBucketPref;

    @Override
    public void onCreatePreferences(@Nullable Bundle savedInstanceState, @Nullable String rootKey) {
        setPreferencesFromResource(R.xml.settings_camera_backup_advance, rootKey);
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        init();
    }

    private void init() {

    }

    @Override
    public void onViewCreated(View view, Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initView();
    }

    private void initView() {
        initCameraBackupView();
    }

    private void initCameraBackupView() {

        //data plan
        cbDataPlan = findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY);

        boolean isDataPlanChecked = AlbumBackupManager.readAllowDataPlanSwitch();
        cbDataPlan.setChecked(isDataPlanChecked);
        cbDataPlan.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(@NonNull Preference preference, Object newValue) {
                boolean isCustom = (Boolean) newValue;
                AlbumBackupManager.writeAllowDataPlanSwitch(isCustom);

                BackgroundJobManagerImpl.getInstance().restartMediaUploadWorker(isCustom);

                return true;
            }
        });

        // videos
        cbVideoAllowed = findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY);

        boolean isAllowVideoChecked = AlbumBackupManager.readAllowVideoSwitch();
        cbVideoAllowed.setChecked(isAllowVideoChecked);
        cbVideoAllowed.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(@NonNull Preference preference, Object newValue) {
                boolean isCustom = (Boolean) newValue;
                AlbumBackupManager.writeAllowVideoSwitch(isCustom);

                BackgroundJobManagerImpl.getInstance().restartMediaUploadWorker(isCustom);

                return true;
            }
        });

        //custom album
        mCameraBackupCustomBucketsSwitch = findPreference(SettingsManager.CAMERA_UPLOAD_CUSTOM_BUCKETS_KEY);

        boolean isCustomChecked = AlbumBackupManager.readCustomAlbumSwitch();
        mCameraBackupCustomBucketsSwitch.setChecked(isCustomChecked);

        mCameraBackupCustomBucketsSwitch.setOnPreferenceChangeListener((preference, newValue) -> {
            boolean isCustom = (Boolean) newValue;
            AlbumBackupManager.writeCustomAlbumSwitch(isCustom);

            mCameraBackupLocalBucketPref.setVisible(isCustom);
            scanCustomDirs(isCustom);
            return false;
        });

        //
        mCameraBackupLocalBucketPref = findPreference(SettingsManager.CAMERA_UPLOAD_BUCKETS_KEY);

        mCameraBackupLocalBucketPref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(@NonNull Preference preference) {
                // choose media buckets
                scanCustomDirs(true);

                return true;
            }
        });

        mCameraBackupLocalBucketPref.setVisible(mCameraBackupCustomBucketsSwitch.isChecked());

        refreshPreferenceView();
    }

    private void scanCustomDirs(boolean isCustomScanOn) {
        if (isCustomScanOn) {
            Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
            intent.putExtra(SettingsFragment.CAMERA_UPLOAD_LOCAL_DIRECTORIES, true);
            selectLocalDirLauncher.launch(intent);
        } else {
            List<String> selectedBuckets = new ArrayList<>();
            AlbumBackupManager.writeBucketIds(selectedBuckets);

            BackgroundJobManagerImpl.getInstance().restartMediaUploadWorker(false);

            refreshPreferenceView();
        }
    }

    private void refreshPreferenceView() {
        List<String> bucketNames = new ArrayList<>();


        List<String> bucketIds = AlbumBackupManager.readBucketIds();
        List<GalleryBucketUtils.Bucket> tempBuckets = GalleryBucketUtils.getMediaBuckets(getActivity().getApplicationContext());
        LinkedHashSet<GalleryBucketUtils.Bucket> bucketsSet = new LinkedHashSet<>(tempBuckets.size());
        bucketsSet.addAll(tempBuckets);
        List<GalleryBucketUtils.Bucket> allBuckets = new ArrayList<>(bucketsSet.size());
        allBuckets.addAll(bucketsSet);

        for (GalleryBucketUtils.Bucket bucket : allBuckets) {
            if (bucketIds.contains(bucket.id)) {
                bucketNames.add(bucket.name);
            }
        }

        if (bucketNames.isEmpty()) {
            AlbumBackupManager.writeCustomAlbumSwitch(false);
            mCameraBackupCustomBucketsSwitch.setChecked(false);
            mCameraBackupLocalBucketPref.setVisible(false);
        } else {
            AlbumBackupManager.writeCustomAlbumSwitch(true);
            mCameraBackupCustomBucketsSwitch.setChecked(true);
            mCameraBackupLocalBucketPref.setVisible(true);
            mCameraBackupLocalBucketPref.setSummary(TextUtils.join(", ", bucketNames));
        }
    }

    private final ActivityResultLauncher<Intent> selectLocalDirLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != RESULT_OK) {

                AlbumBackupManager.writeCustomAlbumSwitch(false);
                mCameraBackupCustomBucketsSwitch.setChecked(false);
                mCameraBackupLocalBucketPref.setVisible(false);

                return;
            }

            BackgroundJobManagerImpl.getInstance().restartMediaUploadWorker(true);

            refreshPreferenceView();
        }
    });
}
