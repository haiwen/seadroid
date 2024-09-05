package com.seafile.seadroid2.ui.settings;

import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.preference.Preference;
import androidx.preference.SwitchPreferenceCompat;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.preferences.RenameSharePreferenceFragmentCompat;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadConfigActivity;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;

import java.util.ArrayList;
import java.util.LinkedHashSet;
import java.util.List;

public class SettingsAlbumBackupAdvancedFragment extends RenameSharePreferenceFragmentCompat {

    private SwitchPreferenceCompat bucketsSwitch;
    private Preference selectedBucketPref;
    private final Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();

    public static SettingsAlbumBackupAdvancedFragment newInstance() {
        return new SettingsAlbumBackupAdvancedFragment();
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

        setPreferencesFromResource(R.xml.prefs_settings_camera_backup_advance, rootKey);
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

        updateSelectedBucketIdsSummary();
    }

    private void loadSpData() {

    }

    private void initPrefView() {
        bucketsSwitch = findPreference(getString(R.string.pref_key_album_backup_advanced_buckets_switch));

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
                SLogs.e("相册备份-高级-允许移动流量：" + isChecked);
            }
        });

        Settings.ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean isChecked) {
                SLogs.e("相册备份-高级-允许视频上传：" + isChecked);
            }
        });

        Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean isChecked) {
                SLogs.e("相册备份-高级-自选相册：" + isChecked);
                selectedBucketPref.setVisible(isChecked);
            }
        });

        Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SELECT.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.e("相册备份-高级-自选的相册列表：" + s);
                if (isFirstLoadData) {
                    return;
                }

                selectedBucketPref.setSummary(s);
            }
        });
    }

    private void launchAlbumSelect() {
        Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
        intent.putExtra(CameraUploadConfigActivity.CAMERA_UPLOAD_LOCAL_DIRECTORIES, true);
        selectAlbumLauncher.launch(intent);
    }

    private final ActivityResultLauncher<Intent> selectAlbumLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            updateSelectedBucketIdsSummary();
        }
    });


    private void updateSelectedBucketIdsSummary() {
        List<String> bucketNames = new ArrayList<>();

        List<String> bucketIds = AlbumBackupSharePreferenceHelper.readBucketIds();
        List<GalleryBucketUtils.Bucket> tempBuckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
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
            selectedBucketPref.setSummary(R.string.not_set);
            bucketsSwitch.setChecked(false);
        } else {
            selectedBucketPref.setSummary(TextUtils.join(", ", bucketNames));
            bucketsSwitch.setChecked(true);
        }
    }
}
