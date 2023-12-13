package com.seafile.seadroid2.ui.fragment;

import android.content.Intent;
import android.hardware.camera2.CameraManager;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.preference.CheckBoxPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.SwitchPreferenceCompat;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.cameraupload.CameraUploadConfigActivity;
import com.seafile.seadroid2.cameraupload.CameraUploadManager;
import com.seafile.seadroid2.cameraupload.GalleryBucketUtils;
import com.seafile.seadroid2.ui.activity.SeafilePathChooserActivity;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

public class SettingsCameraBackupAdvanceFragment extends PreferenceFragmentCompat {

    private SwitchPreferenceCompat mCameraBackupCustomBucketsSwitch;
    private SwitchPreferenceCompat cbDataPlan;
    private SwitchPreferenceCompat cbVideoAllowed;
    private Preference mCameraBackupLocalBucketPref;

    private SettingsManager settingsMgr;
    private CameraUploadManager cameraUploaderManager;

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
        settingsMgr = SettingsManager.instance();
        cameraUploaderManager = new CameraUploadManager(requireContext());
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
        mCameraBackupCustomBucketsSwitch = (SwitchPreferenceCompat) findPreference(SettingsManager.CAMERA_UPLOAD_CUSTOM_BUCKETS_KEY);
        mCameraBackupLocalBucketPref = findPreference(SettingsManager.CAMERA_UPLOAD_BUCKETS_KEY);

        cbDataPlan = findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY);
        cbDataPlan.setChecked(settingsMgr.isDataPlanAllowed());


        // videos
        cbVideoAllowed = findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY);
        cbVideoAllowed.setChecked(settingsMgr.isVideosUploadAllowed());

        mCameraBackupCustomBucketsSwitch.setOnPreferenceChangeListener((preference, newValue) -> {
            boolean isBool = newValue instanceof Boolean;
            if (!isBool) {
                return true;
            }
            boolean isCustom = (Boolean) newValue;

            mCameraBackupLocalBucketPref.setVisible(isCustom);
            scanCustomDirs(isCustom);
            return false;
        });

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
            settingsMgr.setCameraUploadBucketList(selectedBuckets);
            refreshPreferenceView();
        }
    }

    private void refreshPreferenceView() {
        List<String> bucketNames = new ArrayList<>();

        List<String> bucketIds = settingsMgr.getCameraUploadBucketList();
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
            mCameraBackupLocalBucketPref.setVisible(false);
            mCameraBackupCustomBucketsSwitch.setChecked(false);
        } else {
            mCameraBackupCustomBucketsSwitch.setChecked(true);
            mCameraBackupLocalBucketPref.setVisible(true);
            mCameraBackupLocalBucketPref.setSummary(TextUtils.join(", ", bucketNames));
        }
    }


    private final ActivityResultLauncher<Intent> selectLocalDirLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult result) {
            if (result == null || result.getData() == null) {
                return;
            }

            final String repoName = result.getData().getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
            final String repoId = result.getData().getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
            final Account account = result.getData().getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
            if (repoName != null && repoId != null) {
                cameraUploaderManager.setCameraAccount(account);
                settingsMgr.saveCameraUploadRepoInfo(repoId, repoName);
            }

            refreshPreferenceView();
        }
    });
}
