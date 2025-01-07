package com.seafile.seadroid2.ui.settings;

import static android.app.Activity.RESULT_OK;
import static androidx.core.text.HtmlCompat.FROM_HTML_MODE_LEGACY;

import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.text.Spanned;
import android.text.TextUtils;
import android.view.View;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.core.text.HtmlCompat;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.preference.ListPreference;
import androidx.preference.Preference;
import androidx.preference.SwitchPreferenceCompat;
import androidx.work.Data;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.bus.TransferBusHelper;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.enums.NetworkMode;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.util.PermissionUtil;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupScannerWorker;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupScannerWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadMediaFileAutomaticallyWorker;
import com.seafile.seadroid2.preferences.RenameSharePreferenceFragmentCompat;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadConfigActivity;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.dialog_fragment.ClearCacheDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.ClearPasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.SignOutDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.SwitchStorageDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupConfigActivity;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupSelectedPathActivity;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.widget.prefs.SimpleMenuPreference;
import com.seafile.seadroid2.widget.prefs.TextSwitchPreference;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class TabSettings2Fragment extends RenameSharePreferenceFragmentCompat {

    private final Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();

    private SettingsFragmentViewModel viewModel;
//    private SwitchPreferenceCompat gestureSwitch;

    // album backup
    private TextSwitchPreference mAlbumBackupSwitch;
    private Preference mAlbumBackupRepo;
    private Preference mAlbumBackupAdvanced;
    private Preference mAlbumBackupState;

    //folder backup
    private TextSwitchPreference mFolderBackupSwitch;
    private ListPreference mFolderBackupNetworkMode;
    private Preference mFolderBackupSelectRepo;
    private Preference mFolderBackupSelectFolder;
    private Preference mFolderBackupState;


    public static TabSettings2Fragment newInstance() {
        return new TabSettings2Fragment();
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
        //NOTICE: super()
        super.onCreatePreferences(savedInstanceState, rootKey);

        setPreferencesFromResource(R.xml.prefs_settings_2, rootKey);

        SimpleMenuPreference.setLightFixEnabled(true);

    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        getListView().setPadding(0, 0, 0, Constants.DP.DP_32);
        getListView().setBackgroundColor(ContextCompat.getColor(requireContext(), R.color.bar_background_color));

    }

    private boolean isFirstLoadData = true;

    @Override
    public void onResume() {
        super.onResume();
        if (isFirstLoadData) {

            onFirstResume();

            isFirstLoadData = false;
        }

        if (canLoad()) {
            loadData();
        }
    }

    public void onFirstResume() {
        initPref();

//        initGestureConfig();

        initPrefLiveData();

        initWorkerListener();

        // delay updates to avoid flickering
        new Handler().postDelayed(new Runnable() {
            @Override
            public void run() {
                switchAlbumBackupState(mAlbumBackupSwitch.isChecked());
                switchFolderBackupState(mFolderBackupSwitch.isChecked());
            }
        }, 500);

    }


    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        viewModel = new ViewModelProvider(this).get(SettingsFragmentViewModel.class);
    }

    private long last_time = 0L;

    private boolean canLoad() {
        long now = TimeUtils.getNowMills();
        if (now - last_time > 300000) {//5m
            last_time = now;
            return true;
        }
        return false;
    }

    private void initPref() {
        if (currentAccount == null) {
            return;
        }

        initAccountPref();

        initSignOutPref();

        initAlbumBackupPref();

        initFolderBackupPref();

        initCachePref();

        initAboutPref();

        initPolicyPref();
    }

    private void initAccountPref() {
        //user pref
        Preference userPref = findPreference(getString(R.string.pref_key_user_info));
        if (userPref != null) {
            userPref.setOnPreferenceClickListener(preference -> {
                Intent newIntent = new Intent(requireActivity(), AccountsActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(newIntent);
                return true;
            });
        }

//        gestureSwitch = findPreference(getString(R.string.pref_key_gesture_lock));
    }

    private void initSignOutPref() {
        //sign out
//       ButtonPreference buttonPreference = findPreference(getString(R.string.pref_key_sign_out));
//        buttonPreference.getButton().setOnClickListener(new View.OnClickListener() {
//            @Override
//            public void onClick(View v) {
//                onPreferenceSignOutClicked();
//            }
//        });
        findPreference(getString(R.string.pref_key_sign_out)).setOnPreferenceClickListener(preference -> {
            onPreferenceSignOutClicked();
            return true;
        });

        //clear pwd
        findPreference(getString(R.string.pref_key_security_clear_password)).setOnPreferenceClickListener(preference -> {
            // clear password
            clearPassword();
            return true;
        });
    }

    private void initAlbumBackupPref() {
        // Camera Upload
        mAlbumBackupSwitch = findPreference(getString(R.string.pref_key_album_backup_switch));
        mAlbumBackupRepo = findPreference(getString(R.string.pref_key_album_backup_repo_select));
        mAlbumBackupState = findPreference(getString(R.string.pref_key_album_backup_state));
        mAlbumBackupAdvanced = findPreference(getString(R.string.pref_key_album_backup_advanced));

        if (mAlbumBackupAdvanced != null) {
            mAlbumBackupAdvanced.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    Intent intent = new Intent(requireActivity(), SettingsAlbumBackupAdvancedActivity.class);
                    albumBackupAdvanceLauncher.launch(intent);
                    return true;
                }
            });
        }

        if (mAlbumBackupRepo != null) {
            mAlbumBackupRepo.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    // choose remote library
                    Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
                    intent.putExtra(CameraUploadConfigActivity.CAMERA_UPLOAD_REMOTE_LIBRARY, true);
                    cameraBackupConfigLauncher.launch(intent);
                    return true;
                }
            });
        }

    }

    private void initFolderBackupPref() {
        //folder backup
        mFolderBackupSwitch = findPreference(getString(R.string.pref_key_folder_backup_switch));
        mFolderBackupSelectRepo = findPreference(getString(R.string.pref_key_folder_backup_repo_select));
        mFolderBackupSelectFolder = findPreference(getString(R.string.pref_key_folder_backup_folder_select));
        mFolderBackupState = findPreference(getString(R.string.pref_key_folder_backup_state));
        mFolderBackupNetworkMode = findPreference(getString(R.string.pref_key_folder_backup_network_mode));

        //repo
        if (mFolderBackupSelectRepo != null) {
            mFolderBackupSelectRepo.setOnPreferenceClickListener(preference -> {

                Intent intent = new Intent(requireActivity(), FolderBackupConfigActivity.class);
                intent.putExtra(FolderBackupConfigActivity.FOLDER_BACKUP_SELECT_TYPE, "repo");
                folderBackupConfigLauncher.launch(intent);

                return true;
            });
        }

        //
        if (mFolderBackupSelectFolder != null) {
            mFolderBackupSelectFolder.setOnPreferenceClickListener(preference -> {

                List<String> backupPathList = FolderBackupSharePreferenceHelper.readBackupPathsAsList();

                Intent intent;
                if (CollectionUtils.isEmpty(backupPathList)) {
                    intent = new Intent(requireActivity(), FolderBackupConfigActivity.class);
                } else {
                    intent = new Intent(requireActivity(), FolderBackupSelectedPathActivity.class);
                }
                intent.putExtra(FolderBackupConfigActivity.FOLDER_BACKUP_SELECT_TYPE, "folder");
                folderBackupConfigLauncher.launch(intent);

                return true;
            });
        }
    }

    private void initCachePref() {
        // Clear cache
        Preference cachePref = findPreference(getString(R.string.pref_key_cache_clear));
        if (cachePref != null) {
            cachePref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    clearCache();
                    return true;
                }
            });
        }

        Preference locPref = findPreference(getString(R.string.pref_key_cache_location));
        if (locPref != null) {
            // Storage selection only works on KitKat or later
            if (StorageManager.getInstance().supportsMultipleStorageLocations()) {
                updateStorageLocationSummary();
                locPref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                    @Override
                    public boolean onPreferenceClick(@NonNull Preference preference) {
                        SwitchStorageDialogFragment dialogFragment = SwitchStorageDialogFragment.newInstance();
                        dialogFragment.show(getChildFragmentManager(), SwitchStorageDialogFragment.class.getSimpleName());
                        return true;
                    }
                });
            } else {
                locPref.setVisible(false);
            }
        }
    }

    private void initAboutPref() {
        String appVersion = AppUtils.getAppVersionName();

        // App Version
        Preference versionPref = findPreference(getString(R.string.pref_key_about_version));
        if (versionPref != null) {
            versionPref.setSummary(appVersion);
        }

        // About author
        Preference authorPref = findPreference(getString(R.string.pref_key_about_author));
        if (authorPref != null) {
            authorPref.setOnPreferenceClickListener(preference -> {
                MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireActivity());
                Spanned span = HtmlCompat.fromHtml(getString(R.string.settings_about_author_info, appVersion), FROM_HTML_MODE_LEGACY);
                builder.setMessage(span);
                builder.show();
                return true;
            });
        }
    }

    private void initPolicyPref() {
        String country = Locale.getDefault().getCountry();
        String language = Locale.getDefault().getLanguage();
        Preference policyPref = findPreference(getString(R.string.pref_key_about_privacy));
        if (policyPref != null) {
            if (TextUtils.equals("CN", country) || TextUtils.equals("zh", language)) {
                policyPref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                    @Override
                    public boolean onPreferenceClick(@NonNull Preference preference) {
                        SeaWebViewActivity.openUrlDirectly(requireContext(), Constants.URL_PRIVACY);
                        return true;
                    }
                });
            } else {
                policyPref.setVisible(false);
            }
        }
    }

    private void onPreferenceSignOutClicked() {
        SignOutDialogFragment dialogFragment = new SignOutDialogFragment();
        dialogFragment.setRefreshListener(isDone -> {
            if (isDone) {
                Intent intent = new Intent(requireActivity(), MainActivity.class);
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
                requireActivity().finish();
            }
        });
        dialogFragment.show(getChildFragmentManager(), SignOutDialogFragment.class.getSimpleName());
    }

    private void initGestureConfig() {
//        boolean isChecked = Settings.SETTINGS_GESTURE.queryValue();
//        gestureSwitch.setChecked(isChecked);
//        Settings.USER_GESTURE_LOCK_SWITCH.putValue(isChecked);
    }

    private void initPrefLiveData() {
        //////////////////
        /// user
        //////////////////
        Settings.USER_INFO.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                findPreference(getString(R.string.pref_key_user_info)).setTitle(s);

            }
        });

        Settings.USER_SERVER_INFO.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                findPreference(getString(R.string.pref_key_user_server)).setSummary(s);
            }
        });

        Settings.SPACE_INFO.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                findPreference(getString(R.string.pref_key_user_space)).setSummary(s);
            }
        });

//        Settings.USER_GESTURE_LOCK_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
//            @Override
//            public void onChanged(Boolean aBoolean) {
//
//                if (aBoolean) {
//                    // inverse checked status
//                    Intent newIntent = new Intent(getActivity(), CreateGesturePasswordActivity.class);
//                    newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
//                    gestureLauncher.launch(newIntent);
//
//                } else {
//                    LockPatternUtils mLockPatternUtils = new LockPatternUtils(getActivity());
//                    mLockPatternUtils.clearLock();
//
//                    Settings.SETTINGS_GESTURE_LOCK_TIMESTAMP.putValue(0L);
//                }
//
//                Settings.SETTINGS_GESTURE.putValue(aBoolean);
//            }
//        });

        //////////////////
        /// album backup
        //////////////////
        Settings.ALBUM_BACKUP_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                SLogs.e("album switch：" + aBoolean);

                if (aBoolean) {
                    requestCameraStoragePermission();
                } else {
                    AlbumBackupSharePreferenceHelper.writeRepoConfig(null);
                    switchAlbumBackupState(false);
                    dispatchAlbumBackupWork(false);
                }

            }
        });

        Settings.ALBUM_BACKUP_STATE.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.e("album state：" + s);
                mAlbumBackupState.setSummary(s);
            }
        });

        //////////////////
        /// folder backup
        //////////////////
        Settings.FOLDER_BACKUP_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                SLogs.e("folder switch：" + aBoolean);

                if (Boolean.TRUE.equals(aBoolean)) {
                    requestFolderStoragePermission();
                } else {
                    //clear
                    FolderBackupSharePreferenceHelper.writeRepoConfig(null);

                    switchFolderBackupState(false);
                    dispatchFolderBackupWork(false);
                }
            }
        });

        Settings.FOLDER_BACKUP_NETWORK_MODE.observe(getViewLifecycleOwner(), new Observer<NetworkMode>() {
            @Override
            public void onChanged(NetworkMode netWorkMode) {
                SLogs.e("folder network：" + netWorkMode.name());
            }
        });

        Settings.FOLDER_BACKUP_STATE.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.e("folder state：" + s);

                if (mFolderBackupState != null) {
                    mFolderBackupState.setSummary(s);
                }
            }
        });

        //////////////////
        /// cache
        //////////////////
        Settings.CACHE_SIZE.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.e("cache size：" + s);
                findPreference(getString(R.string.pref_key_cache_info)).setSummary(s);
            }
        });
    }


    private void initWorkerListener() {
        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(MediaBackupScannerWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkScanWorkInfo(TransferDataSource.ALBUM_BACKUP, workInfo);
                    }
                });

        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(FolderBackupScannerWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkScanWorkInfo(TransferDataSource.FOLDER_BACKUP, workInfo);
                    }
                });

        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(UploadFolderFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkOutputData(workInfo);
                    }
                });

        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(UploadMediaFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkOutputData(workInfo);
                    }
                });
    }

    private void checkScanWorkInfo(TransferDataSource dataSource, WorkInfo workInfo) {
        if (null == workInfo) {
            return;
        }

        Data outData = workInfo.getOutputData();
        String outDataEvent = outData.getString(TransferWorker.KEY_DATA_EVENT);
        String outDataType = outData.getString(TransferWorker.KEY_DATA_TYPE);

        //scan end
        if (TextUtils.equals(String.valueOf(TransferDataSource.ALBUM_BACKUP), outDataType)) {
            if (TransferEvent.EVENT_SCAN_END.equals(outDataEvent)) {
//                mAlbumBackupState.setSummary(R.string.done);
                SLogs.e("album scan end");
                return;
            }
        } else if (TextUtils.equals(String.valueOf(TransferDataSource.FOLDER_BACKUP), outDataType)) {
            if (TransferEvent.EVENT_SCAN_END.equals(outDataEvent)) {
//                mFolderBackupState.setSummary(R.string.done);
                SLogs.e("folder scan end");
                return;
            }
        }

        Data progressData = workInfo.getProgress();
        String pDataEvent = progressData.getString(TransferWorker.KEY_DATA_EVENT);
        if (TextUtils.isEmpty(pDataEvent)) {
            return;
        }

        if (TransferDataSource.ALBUM_BACKUP == dataSource) {
            if (TransferEvent.EVENT_SCANNING.equals(pDataEvent)) {
                mAlbumBackupState.setSummary(R.string.is_scanning);
            }
        } else if (TransferDataSource.FOLDER_BACKUP == dataSource) {
            if (TransferEvent.EVENT_SCANNING.equals(pDataEvent)) {
                mFolderBackupState.setSummary(R.string.is_scanning);
            }
        }
    }

    private void checkOutputData(WorkInfo workInfo) {
        if (null == workInfo) {
            return;
        }

        Data outData = workInfo.getOutputData();
        String outDataEvent = outData.getString(TransferWorker.KEY_DATA_EVENT);
        String outDataType = outData.getString(TransferWorker.KEY_DATA_TYPE);

        if (TextUtils.equals(String.valueOf(TransferDataSource.ALBUM_BACKUP), outDataType)) {
            if (TransferEvent.EVENT_FINISH.equals(outDataEvent)) {
                mAlbumBackupState.setSummary(R.string.settings_cuc_finish_title);
            } else if (TransferEvent.EVENT_CANCEL_WITH_OUT_OF_QUOTA.equals(outDataEvent)) {
                mAlbumBackupState.setSummary(R.string.above_quota);
            } else if (TransferEvent.EVENT_CANCEL_WITH_BY_STOPPED.equals(outDataEvent)) {
                mAlbumBackupState.setSummary(R.string.canceled);
            }
        } else if (TextUtils.equals(String.valueOf(TransferDataSource.FOLDER_BACKUP), outDataType)) {
            if (TransferEvent.EVENT_FINISH.equals(outDataEvent)) {
                mFolderBackupState.setSummary(R.string.folder_backup_waiting_state);
            } else if (TransferEvent.EVENT_CANCEL_WITH_OUT_OF_QUOTA.equals(outDataEvent)) {
                mFolderBackupState.setSummary(R.string.above_quota);
            } else if (TransferEvent.EVENT_CANCEL_WITH_BY_STOPPED.equals(outDataEvent)) {
                mFolderBackupState.setSummary(R.string.canceled);
            }
        } else {
            checkProgressData(workInfo);
        }
    }

    private void checkProgressData(WorkInfo workInfo) {
        if (null == workInfo) {
            return;
        }

        Data progressData = workInfo.getProgress();
        String dataType = progressData.getString(TransferWorker.KEY_DATA_TYPE);
        String progressEvent = progressData.getString(TransferWorker.KEY_DATA_EVENT);
        String progressFileName = progressData.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);

        if (TextUtils.equals(String.valueOf(TransferDataSource.ALBUM_BACKUP), dataType)) {
            if (TransferEvent.EVENT_TRANSFERRING.equals(progressEvent)) {
                viewModel.countAlbumBackupPendingList(requireContext());
            }
        } else if (TextUtils.equals(String.valueOf(TransferDataSource.FOLDER_BACKUP), dataType)) {
            if (TransferEvent.EVENT_TRANSFERRING.equals(progressEvent)) {
                viewModel.countFolderBackupPendingList(requireContext());
            }
        }
    }

    private void loadData() {
        //get account data
        viewModel.getAccountInfo();

        // Cache size
        calculateCacheSize();

        //
        if (mAlbumBackupSwitch.isChecked()) {
            viewModel.countAlbumBackupPendingList(requireContext());
        }

        if (mFolderBackupSwitch.isChecked()) {
            viewModel.countFolderBackupPendingList(requireContext());
        }
    }

    //0 : no one
    private int whoIsRequestingPermission = 0;

    private void requestCameraStoragePermission() {
        if (PermissionUtil.checkExternalStoragePermission(requireContext())) {

            Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
            cameraBackupConfigLauncher.launch(intent);

        } else {
            whoIsRequestingPermission = 1;

            PermissionUtil.requestExternalStoragePermission(requireContext(), multiplePermissionLauncher, manageStoragePermissionLauncher, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    //on cancel click
                    ToastUtils.showLong(R.string.permission_manage_external_storage_rationale);
                    switchAlbumBackupState(false);
                }
            });
        }
    }

    private void requestFolderStoragePermission() {
        if (PermissionUtil.checkExternalStoragePermission(requireContext())) {
            switchFolderBackupState(true);
            dispatchFolderBackupWork(true);
        } else {
            whoIsRequestingPermission = 2;
            PermissionUtil.requestExternalStoragePermission(requireContext(), multiplePermissionLauncher, manageStoragePermissionLauncher, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    //on cancel click
                    ToastUtils.showLong(R.string.permission_manage_external_storage_rationale);
                    switchFolderBackupState(false);
                }
            });
        }
    }

    private void switchAlbumBackupState(boolean isEnable) {
        mAlbumBackupSwitch.setChecked(isEnable);

        if (isEnable) {
            //all
            mAlbumBackupSwitch.setRadiusPosition(2);
            mAlbumBackupSwitch.setDividerPosition(2);
        } else {
            mAlbumBackupSwitch.setRadiusPosition(1);
            mAlbumBackupSwitch.setDividerPosition(0);
            mAlbumBackupState.setSummary(null);
        }

        //change UI
        mAlbumBackupRepo.setVisible(isEnable);
        mAlbumBackupState.setVisible(isEnable);
        mAlbumBackupAdvanced.setVisible(isEnable);

        updateAlbumBackupSelectedRepoSummary();
    }

    private void updateAlbumBackupSelectedRepoSummary() {
        Account camAccount = CameraUploadManager.getInstance().getCameraAccount();
        RepoConfig repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (camAccount != null && repoConfig != null) {
            mAlbumBackupRepo.setSummary(repoConfig.getRepoName());
        } else {
            mAlbumBackupRepo.setSummary(null);
        }
    }

    private void dispatchAlbumBackupWork(boolean isEnable) {
        if (isEnable) {
            CameraUploadManager.getInstance().setCameraAccount(currentAccount);
            CameraUploadManager.getInstance().performSync();
        } else {
            CameraUploadManager.getInstance().disableCameraUpload();
        }
    }

    private void switchFolderBackupState(boolean isEnable) {
        mFolderBackupSwitch.setChecked(isEnable);

        if (isEnable) {
            //all
            mFolderBackupSwitch.setRadiusPosition(2);
            mFolderBackupSwitch.setDividerPosition(2);
        } else {
            mFolderBackupSwitch.setRadiusPosition(1);
            mFolderBackupSwitch.setDividerPosition(0);
            mFolderBackupState.setSummary(null);
        }

        mFolderBackupNetworkMode.setVisible(isEnable);
        mFolderBackupSelectRepo.setVisible(isEnable);
        mFolderBackupSelectFolder.setVisible(isEnable);
        mFolderBackupState.setVisible(isEnable);

        updateFolderBackupSelectedRepoAndFolderSummary();
    }

    private void updateFolderBackupSelectedRepoAndFolderSummary() {
        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();

        if (repoConfig != null && !TextUtils.isEmpty(repoConfig.getRepoName())) {
            mFolderBackupSelectRepo.setSummary(repoConfig.getRepoName());
        } else {
            mFolderBackupSelectRepo.setSummary(getString(R.string.folder_backup_select_repo_hint));
        }

        List<String> pathList = FolderBackupSharePreferenceHelper.readBackupPathsAsList();
        if (CollectionUtils.isEmpty(pathList)) {
            mFolderBackupSelectFolder.setSummary("0");
        } else {
            mFolderBackupSelectFolder.setSummary(String.valueOf(pathList.size()));
        }
    }

    private void dispatchFolderBackupWork(boolean isEnable) {
        if (!isEnable) {
            TransferBusHelper.resetFileMonitor();
            return;
        }

        RepoConfig repoConfig = FolderBackupSharePreferenceHelper.readRepoConfig();
        List<String> pathList = FolderBackupSharePreferenceHelper.readBackupPathsAsList();

        if (!CollectionUtils.isEmpty(pathList) && repoConfig != null) {
            TransferBusHelper.startFileMonitor();

            BackgroundJobManagerImpl.getInstance().startFolderAutoBackupWorkerChain(true);
        }
    }

    private void clearPassword() {
        ClearPasswordDialogFragment dialogFragment = ClearPasswordDialogFragment.newInstance();
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    ToastUtils.showLong(R.string.clear_password_successful);
                } else {
                    ToastUtils.showLong(R.string.clear_password_failed);
                }
            }
        });
        dialogFragment.show(getChildFragmentManager(), ClearPasswordDialogFragment.class.getSimpleName());
    }

    private void updateStorageLocationSummary() {
        String summary = StorageManager.getInstance().getStorageLocation().description;
        findPreference(getString(R.string.pref_key_cache_location)).setSummary(summary);
    }

    private void clearCache() {
        ClearCacheDialogFragment dialogFragment = ClearCacheDialogFragment.newInstance();
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    calculateCacheSize();
                    ToastUtils.showLong(R.string.settings_clear_cache_success);
                } else {
                    ToastUtils.showLong(R.string.settings_clear_cache_failed);
                }
            }
        });
        dialogFragment.show(getChildFragmentManager(), ClearCacheDialogFragment.class.getSimpleName());
    }

    private void calculateCacheSize() {
        viewModel.calculateCacheSize();
    }

    private final ActivityResultLauncher<Intent> folderBackupConfigLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != RESULT_OK) {
                return;
            }

            Intent data = o.getData();
            if (null == data) {
                return;
            }

            String selectType = data.getStringExtra(FolderBackupConfigActivity.FOLDER_BACKUP_SELECT_TYPE);
            if ("repo".equals(selectType)) {

                RepoConfig repoConfig = null;
                if (data.hasExtra(ObjSelectorActivity.DATA_REPO_ID)) {
                    String repoId = data.getStringExtra(ObjSelectorActivity.DATA_REPO_ID);
                    String repoName = data.getStringExtra(ObjSelectorActivity.DATA_REPO_NAME);
                    Account account = data.getParcelableExtra(ObjSelectorActivity.DATA_ACCOUNT);

                    repoConfig = new RepoConfig(repoId, repoName, account.getEmail(), account.getSignature());
                }

                FolderBackupSharePreferenceHelper.writeRepoConfig(repoConfig);

            } else if ("folder".equals(selectType)) {

                ArrayList<String> selectedFolderPaths = data.getStringArrayListExtra(FolderBackupConfigActivity.BACKUP_SELECT_PATHS);
                FolderBackupSharePreferenceHelper.writeBackupPathsAsString(selectedFolderPaths);

            }

            updateFolderBackupSelectedRepoAndFolderSummary();

            dispatchFolderBackupWork(mFolderBackupSwitch.isChecked());
        }
    });

    private final ActivityResultLauncher<Intent> albumBackupAdvanceLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != RESULT_OK) {
                return;
            }

            updateAlbumBackupSelectedRepoSummary();

            BackgroundJobManagerImpl.getInstance().startMediaWorkerChain(true);
        }
    });

    private final ActivityResultLauncher<Intent> cameraBackupConfigLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() == RESULT_OK) {
                //The dispatch function needs to be put first
                dispatchAlbumBackupWork(true);

                switchAlbumBackupState(true);
            } else {
                //The dispatch function needs to be put first
                dispatchAlbumBackupWork(false);

                if (o.getData() != null) {
                    boolean isChooseRepo = o.getData().getBooleanExtra(CameraUploadConfigActivity.CAMERA_UPLOAD_REMOTE_LIBRARY, false);
                    boolean isChooseDir = o.getData().getBooleanExtra(CameraUploadConfigActivity.CAMERA_UPLOAD_LOCAL_DIRECTORIES, false);
                    if (!isChooseRepo && !isChooseDir) {
                        switchAlbumBackupState(false);
                    } else {
                        SLogs.d("isChooseRepo?" + isChooseRepo);
                        SLogs.d("isChooseDir?" + isChooseDir);
                    }
                } else {
                    switchAlbumBackupState(false);
                }


            }
        }
    });

//    private final ActivityResultLauncher<Intent> gestureLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
//        @Override
//        public void onActivityResult(ActivityResult o) {
//            if (o.getResultCode() != RESULT_OK) {
//                gestureSwitch.setChecked(false);
//            }
//        }
//    });

    private final ActivityResultLauncher<String[]> multiplePermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestMultiplePermissions(), new ActivityResultCallback<Map<String, Boolean>>() {
        @Override
        public void onActivityResult(Map<String, Boolean> o) {
            if (o.isEmpty()) {
                return;
            }

            for (Map.Entry<String, Boolean> stringBooleanEntry : o.entrySet()) {
                if (Boolean.FALSE.equals(stringBooleanEntry.getValue())) {

                    ToastUtils.showLong(R.string.permission_manage_external_storage_rationale);

                    if (whoIsRequestingPermission == 1) {
                        switchAlbumBackupState(false);
                    } else if (whoIsRequestingPermission == 2) {
                        switchFolderBackupState(false);
                    }
                    return;
                }
            }

            if (whoIsRequestingPermission == 1) {

                Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
                cameraBackupConfigLauncher.launch(intent);

            } else if (whoIsRequestingPermission == 2) {
                //on livedata change
            }
        }
    });

    private final ActivityResultLauncher<Intent> manageStoragePermissionLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != RESULT_OK) {
                ToastUtils.showLong(R.string.get_storage_permission_failed);

                if (whoIsRequestingPermission == 1) {
                    switchAlbumBackupState(false);
                } else if (whoIsRequestingPermission == 2) {
                    switchFolderBackupState(false);
                }
                return;
            }

            if (whoIsRequestingPermission == 1) {

                Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
                cameraBackupConfigLauncher.launch(intent);

            } else if (whoIsRequestingPermission == 2) {
                //on livedata change

            }
        }
    });
}
