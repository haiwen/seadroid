package com.seafile.seadroid2.ui.settings;

import static android.app.Activity.RESULT_OK;
import static androidx.core.text.HtmlCompat.FROM_HTML_MODE_LEGACY;
import static com.seafile.seadroid2.framework.notification.base.NotificationUtils.NOTIFICATION_MESSAGE_KEY;
import static com.seafile.seadroid2.framework.notification.base.NotificationUtils.NOTIFICATION_OPEN_DOWNLOAD_TAB;
import static com.seafile.seadroid2.framework.notification.base.NotificationUtils.NOTIFICATION_OPEN_UPLOAD_TAB;

import android.Manifest;
import android.content.Intent;
import android.net.Uri;
import android.os.Build;
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
import androidx.core.content.FileProvider;
import androidx.core.text.HtmlCompat;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.preference.ListPreference;
import androidx.preference.Preference;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.FileUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.config.ObjKey;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.NetworkMode;
import com.seafile.seadroid2.enums.ObjSelectType;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.service.TransferService;
import com.seafile.seadroid2.framework.util.PermissionUtil;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.worker.GlobalTransferCacheList;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.preferences.RenameSharePreferenceFragmentCompat;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.SplashActivity;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadConfigActivity;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.camera_upload.GalleryBucketUtils;
import com.seafile.seadroid2.ui.dialog_fragment.ClearCacheDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.ClearPasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.SignOutDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.SwitchStorageDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupConfigActivity;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupSelectedPathActivity;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.ui.transfer_list.TransferActivity;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.widget.prefs.DividerPositionEnum;
import com.seafile.seadroid2.widget.prefs.RadiusPositionEnum;
import com.seafile.seadroid2.widget.prefs.SimpleMenuPreference;
import com.seafile.seadroid2.widget.prefs.TextSwitchPreference;
import com.seafile.seadroid2.widget.prefs.TextTitleSummaryPreference;

import java.io.File;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.concurrent.CompletableFuture;

public class TabSettings2Fragment extends RenameSharePreferenceFragmentCompat {
    private final String TAG = "TabSettings2Fragment";

    public static final String FB_SELECT_TYPE = "folder_backup_select_type";

    private final Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
    private SettingsFragmentViewModel viewModel;
//    private SwitchPreferenceCompat gestureSwitch;

    // album backup
    private TextSwitchPreference mAlbumBackupSwitch;
    private Preference mAlbumBackupRepo;
    private TextSwitchPreference mAlbumBackupAdvancedDataPlanSwitch;
    private TextSwitchPreference mAlbumBackupAdvancedVideoSwitch;
    //    private TextSwitchPreference mAlbumBackupAdvancedCustomBucketSwitch;
    private TextTitleSummaryPreference mAlbumBackupAdvancedSelectedBucket;
//    private Preference mAlbumBackupState;

    //folder backup
    private TextSwitchPreference mFolderBackupSwitch;
    private ListPreference mFolderBackupNetworkMode;
    private Preference mFolderBackupSelectRepo;
    private Preference mFolderBackupSelectFolder;

    private Preference mExportLogFiles;
//    private Preference mFolderBackupState;

    private Preference mTransferDownloadState;
    private Preference mTransferUploadState;


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
        getListView().setBackgroundColor(ContextCompat.getColor(requireContext(), R.color.window_background_color));
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

        initPrefLiveData();

        initWorkerBusObserver();

        // delay updates to avoid flickering
        runMainThreadDelay(() -> {
            switchAlbumBackupState(mAlbumBackupSwitch.isChecked());
            switchFolderBackupState(mFolderBackupSwitch.isChecked());
        });

    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        viewModel = new ViewModelProvider(this).get(SettingsFragmentViewModel.class);
    }

    private long last_time = 0L;

    private boolean canLoad() {
        long now = TimeUtils.getNowMills();
        if (now - last_time > 60000) {//1m
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

        initTransferPref();

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

    }

    private void initSignOutPref() {
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
//        mAlbumBackupState = findPreference(getString(R.string.pref_key_album_backup_state));
//        mAlbumBackupAdvanced = findPreference(getString(R.string.pref_key_album_backup_advanced));

        mAlbumBackupAdvancedDataPlanSwitch = findPreference(getString(R.string.pref_key_album_backup_advanced_data_plan_switch));
        mAlbumBackupAdvancedVideoSwitch = findPreference(getString(R.string.pref_key_album_backup_advanced_allow_video_switch));
//        mAlbumBackupAdvancedCustomBucketSwitch = findPreference(getString(R.string.pref_key_album_backup_advanced_buckets_switch));
        mAlbumBackupAdvancedSelectedBucket = findPreference(getString(R.string.pref_key_album_backup_advanced_buckets_select));
        if (mAlbumBackupAdvancedSelectedBucket != null) {
            mAlbumBackupAdvancedSelectedBucket.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
                    intent.putExtra(CameraUploadConfigActivity.CAMERA_UPLOAD_LOCAL_DIRECTORIES, true);
                    albumBackupSelectCustomAlbumLauncher.launch(intent);
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
                    albumBackupSelectRepoLauncher.launch(intent);
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
//        mFolderBackupState = findPreference(getString(R.string.pref_key_folder_backup_state));
        mFolderBackupNetworkMode = findPreference(getString(R.string.pref_key_folder_backup_network_mode));

        //repo
        if (mFolderBackupSelectRepo != null) {
            mFolderBackupSelectRepo.setOnPreferenceClickListener(preference -> {

                Bundle bundle = new Bundle();
                bundle.putBoolean("isFilterUnavailable", false);
                bundle.putString(TabSettings2Fragment.FB_SELECT_TYPE, "repo");
                Intent intent = ObjSelectorActivity.getCurrentAccountIntent(requireContext(), ObjSelectType.REPO, ObjSelectType.REPO, bundle);
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
                intent.putExtra(TabSettings2Fragment.FB_SELECT_TYPE, "folder");
                folderBackupConfigLauncher.launch(intent);

                return true;
            });
        }
    }

    private void initTransferPref() {
        mTransferDownloadState = findPreference(getString(R.string.pref_key_transfer_download_state));
        mTransferUploadState = findPreference(getString(R.string.pref_key_transfer_upload_state));
        if (mTransferDownloadState != null) {
            mTransferDownloadState.setOnPreferenceClickListener(preference -> {
                Intent intent = new Intent(requireActivity(), TransferActivity.class);
                intent.putExtra(NOTIFICATION_MESSAGE_KEY, NOTIFICATION_OPEN_DOWNLOAD_TAB);
                intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
                return true;
            });
        }
        if (mTransferUploadState != null) {
            mTransferUploadState.setOnPreferenceClickListener(preference -> {
                Intent intent = new Intent(requireActivity(), TransferActivity.class);
                intent.putExtra(NOTIFICATION_MESSAGE_KEY, NOTIFICATION_OPEN_UPLOAD_TAB);
                intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
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

        Preference exportLogFiles = findPreference(getString(R.string.pref_key_export_log_files));
        if (exportLogFiles != null) {
            exportLogFiles.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    exportLogFile();
                    return false;
                }
            });
        }
    }

    private void exportLogFile() {

        String logPath = StorageManager.getInstance().getLogDir().getAbsolutePath();
        if (!FileUtils.isDir(logPath)) {
            Toasts.show(R.string.export_log_file_not_exists);
            return;
        }

        List<File> listFile = FileUtils.listFilesInDir(logPath, new Comparator<File>() {
            @Override
            public int compare(File o1, File o2) {
                return Long.compare(o2.lastModified(), o1.lastModified());
            }
        });

        if (listFile.isEmpty()) {
            Toasts.show(R.string.export_log_file_not_exists);
            return;
        }

        File latestFile = listFile.get(0);
        if (!latestFile.exists()) {
            Toasts.show(R.string.export_log_file_not_exists);
            return;
        }

        Intent openIntent = new Intent();
        openIntent.setAction(Intent.ACTION_VIEW);
        Uri uri = FileProvider.getUriForFile(requireContext(), BuildConfig.FILE_PROVIDER_AUTHORITIES, latestFile);
        openIntent.setDataAndType(uri, "text/plain");
        openIntent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION | Intent.FLAG_GRANT_WRITE_URI_PERMISSION | Intent.FLAG_GRANT_PERSISTABLE_URI_PERMISSION);
        boolean isAvailable = WidgetUtils.isIntentAvailable(requireContext(), openIntent);
        if (isAvailable) {
            startActivity(openIntent);
        } else {
            String message = String.format(requireContext().getString(R.string.op_exception_suitable_app_not_found), "text/plain");
            Toasts.show(message);
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
                Intent intent = new Intent(requireActivity(), SplashActivity.class);
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(intent);
                requireActivity().finish();
            }
        });
        dialogFragment.show(getChildFragmentManager(), SignOutDialogFragment.class.getSimpleName());
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
                SLogs.d(TAG, "album switch：" + aBoolean);

                if (aBoolean) {
                    requestCameraStoragePermission();
                } else {
                    AlbumBackupSharePreferenceHelper.writeRepoConfig(null);
                    switchAlbumBackupState(false);
                    launchAlbumBackupWhenReady(true);
                }
            }
        });

        Settings.ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                SLogs.d(TAG, "album data plan switch：" + aBoolean);
                launchAlbumBackupWhenReady(false);
            }
        });

        Settings.ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                SLogs.d(TAG, "album allow video switch：" + aBoolean);
                launchAlbumBackupWhenReady(true);
            }
        });

//        Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
//            @Override
//            public void onChanged(Boolean aBoolean) {
//                SLogs.d(TAG, "album buckets switch：" + aBoolean);
//                if (aBoolean) {
//                    Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
//                    intent.putExtra(CameraUploadConfigActivity.CAMERA_UPLOAD_LOCAL_DIRECTORIES, true);
//                    albumBackupSelectCustomAlbumLauncher.launch(intent);
//                } else {
//                    AlbumBackupSharePreferenceHelper.writeBucketIds(null);
//                    updateAlbumBackupPrefSummary();
//                    launchAlbumBackupWhenReady();
//                }
//            }
//        });

//        Settings.ALBUM_BACKUP_STATE.observe(getViewLifecycleOwner(), new Observer<String>() {
//            @Override
//            public void onChanged(String s) {
//                SLogs.d(TAG,"album state：" + s);
//                mAlbumBackupState.setSummary(s);
//            }
//        });

        //////////////////
        /// folder backup
        //////////////////
        Settings.FOLDER_BACKUP_SWITCH.observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                SLogs.d(TAG, "folder switch：" + aBoolean);

                if (Boolean.TRUE.equals(aBoolean)) {
                    requestFolderStoragePermission();
                } else {
                    //clear
                    FolderBackupSharePreferenceHelper.writeRepoConfig(null);
                    FolderBackupSharePreferenceHelper.writeBackupPathsAsString(null);

                    switchFolderBackupState(false);

                    launchFolderBackupWhenReady(true);
                }
            }
        });

        Settings.FOLDER_BACKUP_NETWORK_MODE.observe(getViewLifecycleOwner(), new Observer<NetworkMode>() {
            @Override
            public void onChanged(NetworkMode netWorkMode) {
                SLogs.d(TAG, "folder network：" + netWorkMode.name());

                launchFolderBackupWhenReady(false);
//                TransferService.restartFolderBackupService(requireContext());
            }
        });

//        Settings.FOLDER_BACKUP_STATE.observe(getViewLifecycleOwner(), new Observer<String>() {
//            @Override
//            public void onChanged(String s) {
//               SLogs.d(TAG,"folder state：" + s);
//
//                if (mFolderBackupState != null) {
//                    mFolderBackupState.setSummary(s);
//                }
//            }
//        });

        Settings.TRANSFER_DOWNLOAD_STATE.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.d(TAG, "transfer state：" + s);
                if (mTransferDownloadState != null) {
                    mTransferDownloadState.setSummary(s);
                }
            }
        });
        Settings.TRANSFER_UPLOAD_STATE.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.d(TAG, "transfer state：" + s);
                if (mTransferUploadState != null) {
                    mTransferUploadState.setSummary(s);
                }
            }
        });

        //////////////////
        /// cache
        //////////////////
        Settings.CACHE_SIZE.observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.d(TAG, "cache size：" + s);
                findPreference(getString(R.string.pref_key_cache_info)).setSummary(s);
            }
        });
    }

    private void initWorkerBusObserver() {
        BusHelper.getTransferProgressObserver().observe(getViewLifecycleOwner(), new Observer<Bundle>() {
            @Override
            public void onChanged(Bundle bundle) {
                doBusWork(bundle);
            }
        });
    }

    private void doBusWork(Bundle map) {

        String dataSource = map.getString(TransferWorker.KEY_DATA_SOURCE);
        String statusEvent = map.getString(TransferWorker.KEY_DATA_STATUS);
        String result = map.getString(TransferWorker.KEY_DATA_RESULT);
        String transferId = map.getString(TransferWorker.KEY_TRANSFER_ID);
        int transferCount = map.getInt(TransferWorker.KEY_TRANSFER_COUNT);

        SLogs.d(TAG, "on event: " + statusEvent + ", dataSource: " + dataSource + ", transferCount: " + transferCount);

        if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCANNING)) {
            refreshPendingCount(dataSource, statusEvent, true, result);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCAN_COMPLETE)) {
            refreshPendingCount(dataSource, statusEvent, true, result);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_IN_TRANSFER)) {
            refreshPendingCount(dataSource, statusEvent, false, result);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_FAILED)) {
            refreshPendingCount(dataSource, statusEvent, false, result);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_SUCCESS)) {
            refreshPendingCount(dataSource, statusEvent, false, result);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_TRANSFER_TASK_COMPLETE)) {
            refreshPendingCount(dataSource, statusEvent, true, result);
        }
    }

    private void refreshPendingCount(String dataSource, String statusEvent, boolean isFinish, String result) {
        if (!TextUtils.isEmpty(result)) {
            mTransferUploadState.setSummary(result);
            return;
        }

        if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCANNING)) {
            mTransferUploadState.setSummary(R.string.is_scanning);
            return;
        }

        if (FeatureDataSource.ALBUM_BACKUP.name().equals(dataSource)
                || FeatureDataSource.FOLDER_BACKUP.name().equals(dataSource)
                || FeatureDataSource.MANUAL_FILE_UPLOAD.name().equals(dataSource)) {
            int totalPendingCount = GlobalTransferCacheList.getUploadPendingCount();
            if (totalPendingCount == 0 && !isFinish) {
                totalPendingCount = 1;
            }
            String p = String.valueOf(totalPendingCount);
            mTransferUploadState.setSummary(p);
        } else if (FeatureDataSource.DOWNLOAD.name().equals(dataSource)) {
            int totalPendingCount = GlobalTransferCacheList.getDownloadPendingCount();
            if (totalPendingCount == 0 && !isFinish) {
                totalPendingCount = 1;
            }
            String p = String.valueOf(totalPendingCount);
            mTransferDownloadState.setSummary(p);
        }
    }

    private void loadData() {
        //get account data
        viewModel.getAccountInfo();

        // Cache size
        calculateCacheSize();
    }

    //0 : no one, 1 : camera, 2 : folder
    private int whoIsRequestingPermission = 0;

    private void requestCameraStoragePermission() {
        if (PermissionUtil.hasStoragePermission(requireContext())) {
            Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
            albumBackupConfigLauncher.launch(intent);

        } else {
            whoIsRequestingPermission = 1;
            showRequestStoragePermissionDialog();
        }
    }

    private void requestFolderStoragePermission() {
        switchFolderBackupState(true);

        if (PermissionUtil.hasStoragePermission(requireContext())) {
            launchFolderBackupWhenReady(true);
        } else {
            whoIsRequestingPermission = 2;
            showRequestStoragePermissionDialog();
        }
    }

    private void showRequestStoragePermissionDialog() {
        int msg;
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.R) {
            msg = R.string.permission_read_external_storage_rationale;
        } else {
            msg = R.string.permission_manage_external_storage_rationale;
        }

        new MaterialAlertDialogBuilder(requireContext())
                .setMessage(msg)
                .setCancelable(false)
                .setNegativeButton(R.string.cancel, (dialog, which) -> {
                    dialog.dismiss();

                    Toasts.show(msg);

                    if (whoIsRequestingPermission == 1) {
                        switchAlbumBackupState(false);
                    } else if (whoIsRequestingPermission == 2) {
                        switchFolderBackupState(false);
                    }
                })
                .setPositiveButton(R.string.ok, (dialog, which) -> {
                    dialog.dismiss();
                    if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                        manageAllFilesPermissionLauncher.launch(PermissionUtil.getManageAllFilesIntent(requireContext()));
                    } else {
                        readWritePermissionLauncher.launch(new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE, Manifest.permission.READ_EXTERNAL_STORAGE});
                    }
                })
                .show();
    }

    private void switchAlbumBackupState(boolean isEnable) {
        mAlbumBackupSwitch.setChecked(isEnable);

        if (isEnable) {
            //all
            mAlbumBackupSwitch.setRadiusPosition(RadiusPositionEnum.TOP);
            mAlbumBackupSwitch.setDividerPosition(DividerPositionEnum.BOTTOM);
        } else {
            mAlbumBackupSwitch.setRadiusPosition(RadiusPositionEnum.ALL);
            mAlbumBackupSwitch.setDividerPosition(DividerPositionEnum.NONE);
        }

        //change UI
        mAlbumBackupRepo.setVisible(isEnable);
        mAlbumBackupAdvancedDataPlanSwitch.setVisible(isEnable);
        mAlbumBackupAdvancedVideoSwitch.setVisible(isEnable);
//        mAlbumBackupAdvancedCustomBucketSwitch.setVisible(isEnable);
        mAlbumBackupAdvancedSelectedBucket.setVisible(isEnable);

//        mAlbumBackupState.setVisible(isEnable);
//        mAlbumBackupAdvanced.setVisible(isEnable);

        runMainThreadDelay(() -> updateAlbumBackupPrefSummary());

    }

    private void updateAlbumBackupPrefSummary() {
        RepoConfig repoConfig = AlbumBackupSharePreferenceHelper.readRepoConfig();
        if (repoConfig != null) {
            mAlbumBackupRepo.setSummary(repoConfig.getRepoName());
        } else {
            mAlbumBackupRepo.setSummary(getString(R.string.folder_backup_select_repo_hint));
        }

        boolean dataSwitch = Settings.ALBUM_BACKUP_ADVANCE_DATA_PLAN_SWITCH.queryValue();
        if (mAlbumBackupAdvancedDataPlanSwitch != null) {
            mAlbumBackupAdvancedDataPlanSwitch.setChecked(dataSwitch);
        }

        boolean videoSwitch = Settings.ALBUM_BACKUP_ADVANCE_ALLOW_VIDEO_SWITCH.queryValue();
        if (mAlbumBackupAdvancedVideoSwitch != null) {
            mAlbumBackupAdvancedVideoSwitch.setChecked(videoSwitch);
        }

//        boolean bucketSwitch = Settings.ALBUM_BACKUP_ADVANCE_BUCKETS_SWITCH.queryValue();
//        if (mAlbumBackupAdvancedCustomBucketSwitch != null) {
//            mAlbumBackupAdvancedCustomBucketSwitch.setChecked(bucketSwitch);
//        }

        if (mAlbumBackupAdvancedSelectedBucket != null) {
            List<String> bucketIds = AlbumBackupSharePreferenceHelper.readBucketIds();
            if (CollectionUtils.isEmpty(bucketIds)) {
                mAlbumBackupAdvancedSelectedBucket.setSummary(R.string.settings_camera_upload_dir_auto_scan);
                int color = ContextCompat.getColor(requireContext(), R.color.light_grey);
                mAlbumBackupAdvancedSelectedBucket.setSummaryTextColor(color);
            } else {
                List<String> bucketNames = new ArrayList<>();
                List<GalleryBucketUtils.Bucket> tempBuckets = GalleryBucketUtils.getMediaBuckets(SeadroidApplication.getAppContext());
                if (tempBuckets == null) {
                    return;
                }

                for (GalleryBucketUtils.Bucket bucket : tempBuckets) {
                    if (bucketIds.contains(bucket.bucketId)) {
                        bucketNames.add(bucket.bucketName);
                    }
                }
                mAlbumBackupAdvancedSelectedBucket.setSummary(TextUtils.join(", ", bucketNames));
                int color = ContextCompat.getColor(requireContext(), R.color.bar_title_color);
                mAlbumBackupAdvancedSelectedBucket.setSummaryTextColor(color);
            }
        }
    }

    private void launchAlbumBackupWhenReady(boolean isForce) {
        if (currentAccount == null) {
            return;
        }

        if (isForce) {
            AlbumBackupSharePreferenceHelper.resetLastScanTime();
        }

        GlobalTransferCacheList.ALBUM_BACKUP_QUEUE.clear();

        if (AlbumBackupSharePreferenceHelper.isAlbumBackupEnable()) {
            CameraUploadManager.getInstance().setCameraAccount(currentAccount);
            CameraUploadManager.getInstance().performSync(isForce);
        } else {
            //stop
            if (TransferService.getServiceRunning()) {
                CompletableFuture<Void> future = TransferService.getActiveTasks().getOrDefault(FeatureDataSource.ALBUM_BACKUP, null);
                if (future != null && !future.isDone()) {
                    TransferService.stopPhotoBackupService(requireContext());
                    CameraUploadManager.getInstance().disableCameraUpload();
                }
            }
        }
    }

    private void switchFolderBackupState(boolean isEnable) {
        mFolderBackupSwitch.setChecked(isEnable);

        if (isEnable) {
            //all
            mFolderBackupSwitch.setRadiusPosition(RadiusPositionEnum.TOP);
            mFolderBackupSwitch.setDividerPosition(DividerPositionEnum.BOTTOM);
        } else {
            mFolderBackupSwitch.setRadiusPosition(RadiusPositionEnum.ALL);
            mFolderBackupSwitch.setDividerPosition(DividerPositionEnum.NONE);
//            mFolderBackupState.setSummary(null);
        }

        mFolderBackupNetworkMode.setVisible(isEnable);
        mFolderBackupSelectRepo.setVisible(isEnable);
        mFolderBackupSelectFolder.setVisible(isEnable);
//        mFolderBackupState.setVisible(isEnable);

        updateFolderBackupPrefSummary();
    }

    private void updateFolderBackupPrefSummary() {
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

    private void launchFolderBackupWhenReady(boolean isForce) {

        if (isForce) {
            FolderBackupSharePreferenceHelper.resetLastScanTime();
        }

        GlobalTransferCacheList.FOLDER_BACKUP_QUEUE.clear();

        //
        BusHelper.getCommonObserver().post("RESTART_FILE_MONITOR");

        if (FolderBackupSharePreferenceHelper.isFolderBackupEnable()) {
            TransferService.restartFolderBackupService(requireContext(), isForce);
        } else {
            if (TransferService.getServiceRunning()) {
                CompletableFuture<Void> future = TransferService.getActiveTasks().getOrDefault(FeatureDataSource.FOLDER_BACKUP, null);
                if (future != null && !future.isDone()) {
                    TransferService.stopFolderBackupService(requireContext());
                }
            }
        }
    }

    private void runMainThreadDelay(Runnable runnable) {
        new Handler().postDelayed(runnable, 500);
    }


    private void clearPassword() {
        ClearPasswordDialogFragment dialogFragment = ClearPasswordDialogFragment.newInstance();
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    Toasts.show(R.string.clear_password_successful);
                } else {
                    Toasts.show(R.string.clear_password_failed);
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
                    Toasts.show(R.string.settings_clear_cache_success);
                } else {
                    Toasts.show(R.string.settings_clear_cache_failed);
                }
            }
        });
        dialogFragment.show(getChildFragmentManager(), ClearCacheDialogFragment.class.getSimpleName());
    }

    private void calculateCacheSize() {
        viewModel.calculateCacheSize();
    }

    /**
     * custom album backup activity result launcher
     */
    private final ActivityResultLauncher<Intent> albumBackupSelectCustomAlbumLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() == RESULT_OK) {
                updateAlbumBackupPrefSummary();
                launchAlbumBackupWhenReady(true);
            }
        }
    });

    /**
     * select repo activity result launcher
     */
    private final ActivityResultLauncher<Intent> albumBackupSelectRepoLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() == RESULT_OK) {
                updateAlbumBackupPrefSummary();
                launchAlbumBackupWhenReady(true);
            }
        }
    });

    /**
     * launch config activity result launcher
     */
    private final ActivityResultLauncher<Intent> albumBackupConfigLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o == null) {
                return;
            }

            // close switch when user cancel
            if (o.getResultCode() != RESULT_OK) {
                switchAlbumBackupState(false);
                return;
            }

            switchAlbumBackupState(true);

            launchAlbumBackupWhenReady(true);
        }
    });

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

            String selectType = data.getStringExtra(FB_SELECT_TYPE);
            if ("repo".equals(selectType)) {

                RepoConfig repoConfig = null;
                if (data.hasExtra(ObjKey.REPO_ID)) {
                    Account account = data.getParcelableExtra(ObjKey.ACCOUNT);
                    String repoId = data.getStringExtra(ObjKey.REPO_ID);
                    String repoName = data.getStringExtra(ObjKey.REPO_NAME);

                    repoConfig = new RepoConfig(repoId, repoName, account.getEmail(), account.getSignature());
                }

                FolderBackupSharePreferenceHelper.writeRepoConfig(repoConfig);

            } else if ("folder".equals(selectType)) {

                ArrayList<String> selectedFolderPaths = data.getStringArrayListExtra(FolderBackupConfigActivity.BACKUP_SELECT_PATHS);
                FolderBackupSharePreferenceHelper.writeBackupPathsAsString(selectedFolderPaths);

            }

            updateFolderBackupPrefSummary();

            launchFolderBackupWhenReady(true);
        }
    });

    private final ActivityResultLauncher<String[]> readWritePermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestMultiplePermissions(), new ActivityResultCallback<Map<String, Boolean>>() {
        @Override
        public void onActivityResult(Map<String, Boolean> o) {
            if (o.isEmpty()) {
                return;
            }

            for (Map.Entry<String, Boolean> stringBooleanEntry : o.entrySet()) {
                if (Boolean.FALSE.equals(stringBooleanEntry.getValue())) {

                    Toasts.show(R.string.permission_manage_external_storage_rationale);

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
                albumBackupConfigLauncher.launch(intent);

            } else if (whoIsRequestingPermission == 2) {
                //on livedata change
            }
        }
    });

    private final ActivityResultLauncher<Intent> manageAllFilesPermissionLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (PermissionUtil.hasStoragePermission(requireContext())) {
                if (whoIsRequestingPermission == 1) {

                    Intent intent = new Intent(requireActivity(), CameraUploadConfigActivity.class);
                    albumBackupConfigLauncher.launch(intent);
                } else if (whoIsRequestingPermission == 2) {
                    //on livedata change
                }
            } else {
                Toasts.show(R.string.get_storage_permission_failed);
                if (whoIsRequestingPermission == 1) {
                    switchAlbumBackupState(false);
                } else if (whoIsRequestingPermission == 2) {
                    switchFolderBackupState(false);
                }
            }
        }
    });
}
