package com.seafile.seadroid2.ui.settings;

import static android.app.Activity.RESULT_OK;
import static androidx.core.text.HtmlCompat.FROM_HTML_MODE_LEGACY;

import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.text.Spanned;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.text.HtmlCompat;
import androidx.fragment.app.FragmentResultListener;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.preference.ListPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.SwitchPreferenceCompat;
import androidx.work.Data;
import androidx.work.NetworkType;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.bumptech.glide.Glide;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.AnalyticsEvent;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.data.ServerInfo;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.GestureLockManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.util.PermissionUtil;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.file_monitor.FileSyncService;
import com.seafile.seadroid2.framework.worker.SupportWorkManager;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.FolderBackupScannerWorker;
import com.seafile.seadroid2.framework.worker.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.MediaBackupScannerWorker;
import com.seafile.seadroid2.framework.worker.UploadMediaFileAutomaticallyWorker;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
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
import com.seafile.seadroid2.ui.gesture.CreateGesturePasswordActivity;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.view.ListPreferenceCompat;

import java.util.List;
import java.util.Locale;
import java.util.Map;

public class SettingsFragment extends PreferenceFragmentCompat {
    private static final String DEBUG_TAG = "SettingsFragment";

    public static final String CAMERA_UPLOAD_REMOTE_LIBRARY = "com.seafile.seadroid2.camera.upload.library";
    public static final String CAMERA_UPLOAD_LOCAL_DIRECTORIES = "com.seafile.seadroid2.camera.upload.directories";

    private SettingsFragmentViewModel viewModel;
    private SettingsActivityViewModel activityViewModel;

    // Camera upload
    private SwitchPreferenceCompat mCameraBackupSwitch;

    private Preference mCameraBackupAdvanced;
    private Preference mCameraBackupState;
    private Preference mCameraBackupRepoPref;
    private Account currentAccount;
    private SettingsActivity mActivity;

    //folder backup
    private SwitchPreferenceCompat mFolderBackupSwitch;
    private ListPreference mFolderBackupNetworkMode;
    private Preference mFolderBackupRepo;
    private Preference mFolderBackupFolderPref;
    private Preference mFolderBackupState;

    private FileSyncService fileSyncService;

    public static SettingsFragment newInstance() {

        Bundle args = new Bundle();

        SettingsFragment fragment = new SettingsFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreatePreferences(@Nullable Bundle savedInstanceState, @Nullable String rootKey) {
        setPreferencesFromResource(R.xml.settings, rootKey);
    }


    @Override
    public void onAttach(@NonNull Context context) {
        super.onAttach(context);

        // global variables
        mActivity = (SettingsActivity) getActivity();
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        viewModel = new ViewModelProvider(this).get(SettingsFragmentViewModel.class);
        activityViewModel = new ViewModelProvider(requireActivity()).get(SettingsActivityViewModel.class);

        bindService();
    }

    @Override
    public void onViewCreated(View view, Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onViewCreated");
        super.onViewCreated(view, savedInstanceState);

        initView();

        initViewModel();
        initWorkerListener();

        loadData();
    }

    private void initViewModel() {
        viewModel.getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                activityViewModel.getRefreshLiveData().setValue(aBoolean);
            }
        });

        viewModel.getAccountInfoLiveData().observe(getViewLifecycleOwner(), accountInfo -> {
            // update Account info settings
            findPreference(SettingsManager.SETTINGS_ACCOUNT_INFO_KEY).setSummary(accountInfo.getDisplayName());
            findPreference(SettingsManager.SETTINGS_ACCOUNT_SPACE_KEY).setSummary(accountInfo.getSpaceUsed());

            refreshServerView();
        });

        viewModel.getCacheSizeLiveData().observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                findPreference(SettingsManager.SETTINGS_CACHE_SIZE_KEY).setSummary(s);
            }
        });

        viewModel.getFolderBackupStateLiveData().observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                if (mFolderBackupState != null) {
                    mFolderBackupState.setSummary(s);
                }
            }
        });

        viewModel.getAlbumBackupStateLiveData().observe(getViewLifecycleOwner(), new Observer<String>() {
            @Override
            public void onChanged(String s) {
                if (CameraUploadManager.getInstance().isCameraUploadEnabled() && mCameraBackupState != null) {
                    mCameraBackupState.setSummary(s);
                }
            }
        });
    }

    private void initWorkerListener() {
        SupportWorkManager.getWorkManager()
                .getWorkInfoByIdLiveData(MediaBackupScannerWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkScanWorkInfo(TransferDataSource.ALBUM_BACKUP, workInfo);
                    }
                });

        SupportWorkManager.getWorkManager()
                .getWorkInfoByIdLiveData(FolderBackupScannerWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        checkScanWorkInfo(TransferDataSource.FOLDER_BACKUP, workInfo);
                    }
                });

        SupportWorkManager.getWorkManager()
                .getWorkInfoByIdLiveData(UploadFolderFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        doWorkInfoLiveData(workInfo);
                    }
                });

        SupportWorkManager.getWorkManager()
                .getWorkInfoByIdLiveData(UploadMediaFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        doWorkInfoLiveData(workInfo);
                    }
                });
    }

    private void checkScanWorkInfo(TransferDataSource dataSource, WorkInfo workInfo) {
        if (null == workInfo) {
            return;
        }

        Data progressData = workInfo.getProgress();
        Data outData = workInfo.getOutputData();

        String pDataEvent = progressData.getString(TransferWorker.KEY_DATA_EVENT);
        String oDataEvent = outData.getString(TransferWorker.KEY_DATA_EVENT);

        if (TextUtils.isEmpty(oDataEvent) && TextUtils.isEmpty(pDataEvent)) {
            return;
        }

        if (TransferDataSource.ALBUM_BACKUP == dataSource) {
            if (TransferEvent.EVENT_SCANNING.equals(pDataEvent)) {
                mCameraBackupState.setSummary(R.string.is_scanning);
            } else if (TransferEvent.EVENT_SCAN_END.equals(oDataEvent)) {
                mCameraBackupState.setSummary(R.string.waiting);
            }
        } else if (TransferDataSource.FOLDER_BACKUP == dataSource) {
            if (TransferEvent.EVENT_SCANNING.equals(pDataEvent)) {
                mFolderBackupState.setSummary(R.string.is_scanning);
            } else if (TransferEvent.EVENT_SCAN_END.equals(oDataEvent)) {
                mFolderBackupState.setSummary(R.string.waiting);
            }
        }
    }

    private void doWorkInfoLiveData(WorkInfo workInfo) {
        if (null == workInfo) {
            return;
        }

        Data outData = workInfo.getOutputData();
        Data progressData = workInfo.getProgress();

        String outDataEvent = outData.getString(TransferWorker.KEY_DATA_EVENT);
        String outDataType = outData.getString(TransferWorker.KEY_DATA_TYPE);
        if (!TextUtils.isEmpty(outDataEvent)) {
            if (String.valueOf(TransferDataSource.ALBUM_BACKUP).equals(outDataType)) {
                if (TransferEvent.EVENT_TRANSFERRED_WITHOUT_DATA.equals(outDataEvent)) {
                    mCameraBackupState.setSummary(R.string.settings_cuc_finish_title);
                } else if (TransferEvent.EVENT_TRANSFERRED_WITH_DATA.equals(outDataEvent)) {
                    mCameraBackupState.setSummary(R.string.settings_cuc_finish_title);
                } else if (TransferEvent.EVENT_CANCEL_OUT_OF_QUOTA.equals(outDataEvent)) {
                    mCameraBackupState.setSummary(R.string.above_quota);
                }
            } else if (String.valueOf(TransferDataSource.FOLDER_BACKUP).equals(outDataType)) {
                if (TransferEvent.EVENT_TRANSFERRED_WITHOUT_DATA.equals(outDataEvent)) {
                    mFolderBackupState.setSummary(R.string.folder_backup_waiting_state);
                } else if (TransferEvent.EVENT_TRANSFERRED_WITH_DATA.equals(outDataEvent)) {
                    mFolderBackupState.setSummary(R.string.folder_backup_waiting_state);
                } else if (TransferEvent.EVENT_CANCEL_OUT_OF_QUOTA.equals(outDataEvent)) {
                    mFolderBackupState.setSummary(R.string.above_quota);
                }
            }

            return;
        }

        String dataType = progressData.getString(TransferWorker.KEY_DATA_TYPE);
        String progressDataEvent = progressData.getString(TransferWorker.KEY_DATA_EVENT);
        String progressFileName = progressData.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);

        if (TextUtils.isEmpty(dataType)) {
            return;
        }

        if (String.valueOf(TransferDataSource.ALBUM_BACKUP).equals(dataType)) {
            if (TransferEvent.EVENT_TRANSFERRING.equals(progressDataEvent)) {
                viewModel.countAlbumBackupState(requireContext());
            }
        } else if (String.valueOf(TransferDataSource.FOLDER_BACKUP).equals(dataType)) {
            if (TransferEvent.EVENT_TRANSFERRING.equals(progressDataEvent)) {
                viewModel.countFolderBackupState(requireContext());
            }
        }


    }

    private void loadData() {
        viewModel.getAccountInfo();

        if (mCameraBackupSwitch.isChecked()) {
            viewModel.countAlbumBackupState(requireContext());
        }

        if (mFolderBackupSwitch.isChecked()) {
            viewModel.countFolderBackupState(requireContext());
        }
    }

    private void initView() {
        // Space used
        currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (currentAccount == null) {
            // User info
            return;
        }

        initAccountView();

        initAppView();

        initCameraBackupView();

        initFolderBackupView();

        initCacheView();

        initAboutView();

        initPolicyView();

        refreshCameraUploadView();
        refreshFolderBackupView(false);

        // Cache size
        calculateCacheSize();
    }

    private void initAccountView() {
        findPreference(SettingsManager.SETTINGS_ACCOUNT_INFO_KEY).setSummary(currentAccount.getDisplayName());

        refreshServerView();
    }

    private void refreshServerView() {
        Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (currentAccount == null) {
            // User info
            return;
        }

        Preference clientEncPref = findPreference(SettingsManager.CLIENT_ENC_SWITCH_KEY);
        if (clientEncPref == null) {
            return;
        }

        ServerInfo serverInfo = SupportAccountManager.getInstance().getServerInfo(currentAccount);
        if (serverInfo.canLocalDecrypt()) {
            // Client side encryption for encrypted Library
            clientEncPref.setVisible(true);
            clientEncPref.setOnPreferenceChangeListener((preference, newValue) -> {
                if (newValue instanceof Boolean) {
                    boolean isChecked = (Boolean) newValue;
                    // inverse checked status
                    AppDataManager.writeClientEncSwitch(isChecked);
                    return true;
                }

                return false;
            });
        } else {
            clientEncPref.setVisible(false);
            clientEncPref.setOnPreferenceClickListener(null);
        }
    }

    private void initAppView() {
        //gesture lock
        SwitchPreferenceCompat gestureSwitch = findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY);
        gestureSwitch.setChecked(GestureLockManager.readGestureLockSwitch());
        gestureSwitch.setOnPreferenceChangeListener((preference, newValue) -> {
            boolean isChecked = (Boolean) newValue;
            GestureLockManager.writeGestureLockSwitch(isChecked);

            if (isChecked) {
                // inverse checked status
                Intent newIntent = new Intent(getActivity(), CreateGesturePasswordActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                gestureLauncher.launch(newIntent);
            } else {
                LockPatternUtils mLockPatternUtils = new LockPatternUtils(getActivity());
                mLockPatternUtils.clearLock();
            }
            return true;
        });

        //sign out
        findPreference(SettingsManager.SETTINGS_ACCOUNT_SIGN_OUT_KEY).setOnPreferenceClickListener(preference -> {
            onPreferenceSignOutClicked();
            return true;
        });

        //clear pwd
        findPreference(SettingsManager.CLEAR_PASSOWR_SWITCH_KEY).setOnPreferenceClickListener(preference -> {
            // clear password
            clearPassword();
            return true;
        });
    }

    private void initCameraBackupView() {
        // Camera Upload
        mCameraBackupSwitch = findPreference(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY);
        mCameraBackupAdvanced = findPreference(SettingsManager.CAMERA_UPLOAD_ADVANCED_CATEGORY_KEY);
        mCameraBackupRepoPref = findPreference(SettingsManager.CAMERA_UPLOAD_REPO_KEY);
        mCameraBackupState = findPreference(SettingsManager.CAMERA_UPLOAD_STATE);

        boolean backSwitch = AlbumBackupManager.readBackupSwitch();
        mCameraBackupSwitch.setChecked(backSwitch);

        //
        mCameraBackupAdvanced.setFragment(SettingsCameraBackupAdvanceFragment.class.getName());


        mCameraBackupSwitch.setOnPreferenceChangeListener((preference, newValue) -> {
            if (Boolean.TRUE.equals(newValue)) {
                requestCameraStoragePermission();
                return true;
            }

            AlbumBackupManager.writeBackupSwitch(false);
            refreshCameraUploadView();
            switchCameraWorker(false);

            return true;
        });

        mCameraBackupRepoPref.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(@NonNull Preference preference) {
                // choose remote library
                Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
                intent.putExtra(CAMERA_UPLOAD_REMOTE_LIBRARY, true);
                cameraBackupConfigLauncher.launch(intent);
                return true;
            }
        });

        setCameraPreferencesVisible(mCameraBackupSwitch.isChecked());
    }


    private void initFolderBackupView() {
        //folder backup
        mFolderBackupSwitch = findPreference(SettingsManager.FOLDER_BACKUP_SWITCH_KEY);
        mFolderBackupRepo = findPreference(SettingsManager.FOLDER_BACKUP_LIBRARY_KEY);
        mFolderBackupFolderPref = findPreference(SettingsManager.FOLDERS_BACKUP_SELECTED_PATH_KEY);
        mFolderBackupState = findPreference(SettingsManager.FOLDER_BACKUP_STATE);
        mFolderBackupNetworkMode = findPreference(SettingsManager.FOLDER_BACKUP_NETWORK_MODE);

        boolean isChecked = FolderBackupManager.readBackupSwitch();
        mFolderBackupSwitch.setChecked(isChecked);

        mFolderBackupSwitch.setOnPreferenceChangeListener((preference, newValue) -> {
            if (Boolean.TRUE.equals(newValue)) {
                requestFolderStoragePermission();
                return true;
            }

            FolderBackupManager.writeBackupSwitch(false);
            refreshFolderBackupView();
            return true;
        });

        //network mode
        if (mFolderBackupNetworkMode != null) {
//            mFolderBackupNetworkMode.setOnPreferenceChangeListener((preference, newValue) -> {
//                String newString = (String) newValue;
//                FolderBackupManager.writeNetworkMode(newString);

//                BackgroundJobManagerImpl.getInstance().scheduleFilesUploadScanWorker(true);
//                refreshFolderBackNetworkMode();

//                return true;
//            });
            refreshFolderBackNetworkMode();
        }

        //repo
        if (mFolderBackupRepo != null) {
            mFolderBackupRepo.setOnPreferenceClickListener(preference -> {

                Intent intent = new Intent(mActivity, FolderBackupConfigActivity.class);
                intent.putExtra(FolderBackupConfigActivity.FOLDER_BACKUP_SELECT_MODE, "repo");
                folderBackupConfigLauncher.launch(intent);

                return true;
            });
        }

        //
        if (mFolderBackupFolderPref != null) {
            mFolderBackupFolderPref.setOnPreferenceClickListener(preference -> {

                List<String> backupPathList = FolderBackupManager.readBackupPaths();

                Intent intent;
                if (CollectionUtils.isEmpty(backupPathList)) {
                    intent = new Intent(mActivity, FolderBackupConfigActivity.class);
                } else {
                    intent = new Intent(mActivity, FolderBackupSelectedPathActivity.class);
                }
                intent.putExtra(FolderBackupConfigActivity.FOLDER_BACKUP_SELECT_MODE, "folder");
                folderBackupConfigLauncher.launch(intent);

                return true;
            });
        }

        setFolderPreferencesVisible(mFolderBackupSwitch.isChecked());
    }


    private void initCacheView() {
        // Clear cache
        findPreference(SettingsManager.SETTINGS_CLEAR_CACHE_KEY).setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(@NonNull Preference preference) {
                clearCache();
                return true;
            }
        });

        // Storage selection only works on KitKat or later
        if (StorageManager.getInstance().supportsMultipleStorageLocations()) {
            updateStorageLocationSummary();
            findPreference(SettingsManager.SETTINGS_CACHE_DIR_KEY).setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    SwitchStorageDialogFragment dialogFragment = SwitchStorageDialogFragment.newInstance();
                    dialogFragment.show(getChildFragmentManager(), SwitchStorageDialogFragment.class.getSimpleName());
                    return true;
                }
            });
        } else {
            findPreference(SettingsManager.SETTINGS_CACHE_DIR_KEY).setVisible(false);
        }
    }

    private void initAboutView() {
        // App Version
        String appVersion = AppUtils.getAppVersionName();
        findPreference(SettingsManager.SETTINGS_ABOUT_VERSION_KEY).setSummary(appVersion);

        // About author
        findPreference(SettingsManager.SETTINGS_ABOUT_AUTHOR_KEY).setOnPreferenceClickListener(preference -> {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(mActivity);
            Spanned span = HtmlCompat.fromHtml(getString(R.string.settings_about_author_info, appVersion), FROM_HTML_MODE_LEGACY);
            builder.setMessage(span);
            builder.show();
            return true;
        });
    }

    private void initPolicyView() {
        String country = Locale.getDefault().getCountry();
        String language = Locale.getDefault().getLanguage();
        if (TextUtils.equals("CN", country) || TextUtils.equals("zh", language)) {
            findPreference(SettingsManager.SETTINGS_PRIVACY_POLICY_KEY).setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    SeaWebViewActivity.openUrl(requireContext(), Constants.URL_PRIVACY);
                    return true;
                }
            });
        } else {
            findPreference(SettingsManager.SETTINGS_PRIVACY_POLICY_KEY).setVisible(false);
        }
    }

    @Override
    public void onDisplayPreferenceDialog(@NonNull Preference preference) {
        if (preference instanceof ListPreference) {
            showListPreferenceDialog(preference);
        } else {
            super.onDisplayPreferenceDialog(preference);
        }
    }

    private void showListPreferenceDialog(Preference preference) {
        ListPreferenceCompat dialogFragment = new ListPreferenceCompat();
        Bundle bundle = new Bundle(1);
        bundle.putString(ListPreferenceCompat.ARG_KEY, preference.getKey());//key is "key"
        dialogFragment.setArguments(bundle);
        dialogFragment.setTargetFragment(this, 0);
        dialogFragment.show(getParentFragmentManager(), "androidx.preference.PreferenceFragment.DIALOG");

        getParentFragmentManager().setFragmentResultListener(preference.getKey(), this, new FragmentResultListener() {
            @Override
            public void onFragmentResult(@NonNull String requestKey, @NonNull Bundle result) {
                String which = result.getString("result");

                String localMode = FolderBackupManager.readNetworkMode();
                if (TextUtils.equals(which, localMode)) {
                    return;
                }

                FolderBackupManager.writeNetworkMode(which);
                refreshFolderBackNetworkMode(which);

                //restart
                NetworkType networkType = NetworkType.UNMETERED;
                if (FolderBackupManager.readDataPlanAllowed()) {
                    networkType = NetworkType.CONNECTED;
                }

                BackgroundJobManagerImpl.getInstance().restartFolderUploadWorker(networkType);

            }
        });
    }

    private void onPreferenceSignOutClicked() {
        SignOutDialogFragment dialogFragment = new SignOutDialogFragment();
        dialogFragment.setRefreshListener(isDone -> {
            if (isDone) {
                Intent intent = new Intent(mActivity, MainActivity.class);
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                mActivity.startActivity(intent);
                mActivity.finish();
            }
        });
        dialogFragment.show(getChildFragmentManager(), SignOutDialogFragment.class.getSimpleName());
    }


    //0 : no one
    private int whoIsRequestingPermission = 0;

    private void requestCameraStoragePermission() {
        if (PermissionUtil.checkExternalStoragePermission(requireContext())) {

            Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
            cameraBackupConfigLauncher.launch(intent);

        } else {
            whoIsRequestingPermission = 1;

            PermissionUtil.requestExternalStoragePermission(requireContext(), multiplePermissionLauncher, manageStoragePermissionLauncher, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    //on cancel click
                    ToastUtils.showLong(R.string.permission_manage_external_storage_rationale);
                    mCameraBackupSwitch.setChecked(false);
                }
            });
        }

    }

    private void requestFolderStoragePermission() {
        if (PermissionUtil.checkExternalStoragePermission(requireContext())) {
            FolderBackupManager.writeBackupSwitch(true);
            refreshFolderBackupView();
        } else {
            whoIsRequestingPermission = 2;
            PermissionUtil.requestExternalStoragePermission(requireContext(), multiplePermissionLauncher, manageStoragePermissionLauncher, new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    //on cancel click
                    ToastUtils.showLong(R.string.permission_manage_external_storage_rationale);
                    mFolderBackupSwitch.setChecked(false);
                }
            });
        }
    }


    private void refreshCameraUploadView() {
        boolean isEnable = AlbumBackupManager.readBackupSwitch();

        if (isEnable) {
            Account camAccount = CameraUploadManager.getInstance().getCameraAccount();
            RepoConfig repoConfig = AlbumBackupManager.readRepoConfig();
            if (camAccount != null && repoConfig != null) {
                mCameraBackupRepoPref.setSummary(camAccount.getSignature() + "/" + repoConfig.getRepoName());
            }
        } else {
            mCameraBackupRepoPref.setSummary(null);
        }

        setCameraPreferencesVisible(isEnable);
    }

    private void setCameraPreferencesVisible(boolean isChecked) {
        mCameraBackupRepoPref.setVisible(isChecked);
        mCameraBackupState.setVisible(isChecked);
        mCameraBackupAdvanced.setVisible(isChecked);
    }

    private void switchCameraWorker(boolean isChecked) {
        if (isChecked) {
            CameraUploadManager.getInstance().setCameraAccount(currentAccount);
            BackgroundJobManagerImpl.getInstance().restartMediaBackupWorker(true);

            //firebase - event -login
            Bundle eventBundle = new Bundle();
            eventBundle.putString(FirebaseAnalytics.Param.METHOD, "switchCameraWorker");
            FirebaseAnalytics.getInstance(requireContext()).logEvent(AnalyticsEvent.ALBUM_BACKUP, eventBundle);

        } else {
            CameraUploadManager.getInstance().disableCameraUpload();
            BackgroundJobManagerImpl.getInstance().cancelMediaWorker();
        }
    }

    private void refreshFolderBackupView() {
        refreshFolderBackupView(true);
    }

    private void refreshFolderBackupView(boolean isSync) {
        boolean isFolderAutomaticBackup = FolderBackupManager.readBackupSwitch();

        setFolderPreferencesVisible(isFolderAutomaticBackup);

        if (!isFolderAutomaticBackup) {
            BackgroundJobManagerImpl.getInstance().cancelFolderWorker();
            if (fileSyncService != null) {
                fileSyncService.stopFolderMonitor();
            }
            return;
        }

        RepoConfig repoConfig = FolderBackupManager.readRepoConfig();
        if (repoConfig != null && !TextUtils.isEmpty(repoConfig.getRepoName())) {
            mFolderBackupRepo.setSummary(repoConfig.getEmail() + "/" + repoConfig.getRepoName());
        } else {
            mFolderBackupRepo.setSummary(getString(R.string.folder_backup_select_repo_hint));
        }

        List<String> pathList = FolderBackupManager.readBackupPaths();
        if (CollectionUtils.isEmpty(pathList)) {
            mFolderBackupFolderPref.setSummary("0");
        } else {
            mFolderBackupFolderPref.setSummary(String.valueOf(pathList.size()));
        }

        if (isSync && !CollectionUtils.isEmpty(pathList) && repoConfig != null) {
            if (fileSyncService != null) {
                fileSyncService.startFolderMonitor(pathList);
            }

            BackgroundJobManagerImpl.getInstance().scheduleFolderBackupScannerWorker(true);
        }

        //firebase - event - album_backup
        Bundle eventBundle = new Bundle();
        eventBundle.putString(FirebaseAnalytics.Param.METHOD, "refreshFolderBackupView");
        FirebaseAnalytics.getInstance(requireContext()).logEvent(AnalyticsEvent.ALBUM_BACKUP, eventBundle);

    }

    private void refreshFolderBackNetworkMode() {
        String localMode = FolderBackupManager.readNetworkMode();
        refreshFolderBackNetworkMode(localMode);
    }

    private void refreshFolderBackNetworkMode(String localMode) {
        CharSequence[] charSequences = mFolderBackupNetworkMode.getEntryValues();
        CharSequence[] entrySequences = mFolderBackupNetworkMode.getEntries();
        if (TextUtils.equals(localMode, charSequences[0])) {
            mFolderBackupNetworkMode.setValueIndex(0);
            mFolderBackupNetworkMode.setSummary(entrySequences[0]);
        } else {
            mFolderBackupNetworkMode.setValueIndex(1);
            mFolderBackupNetworkMode.setSummary(entrySequences[1]);
        }
    }


    private void setFolderPreferencesVisible(boolean isChecked) {
        mFolderBackupNetworkMode.setVisible(isChecked);
        mFolderBackupRepo.setVisible(isChecked);
        mFolderBackupFolderPref.setVisible(isChecked);
        mFolderBackupState.setVisible(isChecked);
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
        findPreference(SettingsManager.SETTINGS_CACHE_DIR_KEY).setSummary(summary);
    }


    private void clearCache() {
        ClearCacheDialogFragment dialogFragment = ClearCacheDialogFragment.newInstance();
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    calculateCacheSize();
                    //clear Glide cache
                    Glide.get(SeadroidApplication.getAppContext()).clearMemory();
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

            refreshFolderBackupView();
        }
    });

    private final ActivityResultLauncher<Intent> cameraBackupConfigLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != RESULT_OK) {
                if (o.getData() != null) {
                    boolean isChooseRepo = o.getData().getBooleanExtra(CAMERA_UPLOAD_REMOTE_LIBRARY, false);
                    boolean isChooseDir = o.getData().getBooleanExtra(CAMERA_UPLOAD_LOCAL_DIRECTORIES, false);
                    if (!isChooseRepo && !isChooseDir) {
                        mCameraBackupSwitch.setChecked(false);
                        AlbumBackupManager.writeBackupSwitch(false);
                    } else {
                        SLogs.d("isChooseRepo?" + isChooseRepo);
                        SLogs.d("isChooseDir?" + isChooseDir);
                    }
                } else {
                    mCameraBackupSwitch.setChecked(false);
                    AlbumBackupManager.writeBackupSwitch(false);
                }
                return;
            }

            AlbumBackupManager.writeBackupSwitch(true);
            refreshCameraUploadView();
            switchCameraWorker(true);
        }
    });

    private final ActivityResultLauncher<Intent> gestureLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != RESULT_OK) {
                ((SwitchPreferenceCompat) findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY)).setChecked(false);
            }
        }
    });


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
                        mCameraBackupSwitch.setChecked(false);
                    } else if (whoIsRequestingPermission == 2) {
                        mFolderBackupSwitch.setChecked(false);
                    }
                    return;
                }
            }

            if (whoIsRequestingPermission == 1) {

                Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
                cameraBackupConfigLauncher.launch(intent);

            } else if (whoIsRequestingPermission == 2) {

                FolderBackupManager.writeBackupSwitch(true);
                refreshFolderBackupView();
            }
        }
    });

    private final ActivityResultLauncher<Intent> manageStoragePermissionLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != RESULT_OK) {
                ToastUtils.showLong(R.string.get_storage_permission_failed);

                if (whoIsRequestingPermission == 1) {
                    mCameraBackupSwitch.setChecked(false);
                } else if (whoIsRequestingPermission == 2) {
                    mFolderBackupSwitch.setChecked(false);
                }
                return;
            }

            if (whoIsRequestingPermission == 1) {

                Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
                cameraBackupConfigLauncher.launch(intent);

            } else if (whoIsRequestingPermission == 2) {

                FolderBackupManager.writeBackupSwitch(true);
                refreshFolderBackupView();
            }
        }
    });


    private boolean isBound = false;

    private final ServiceConnection syncConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            FileSyncService.FileSyncBinder binder = (FileSyncService.FileSyncBinder) service;
            fileSyncService = binder.getService();
            isBound = true;
            SLogs.d("SettingsFragment: bond FileSyncService");
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            fileSyncService = null;
            isBound = false;
            SLogs.d("SettingsFragment: FileSyncService disconnected");
        }
    };

    private void bindService() {
        if (!isBound) {
            Context context = requireContext();

            Intent syncIntent = new Intent(context, FileSyncService.class);
            context.bindService(syncIntent, syncConnection, Context.BIND_AUTO_CREATE);
        }
    }

    private void unbindService() {
        if (isBound) {
            Context context = requireContext();
            context.unbindService(syncConnection);
        }
    }

    @Override
    public void onDestroy() {

        unbindService();

        super.onDestroy();
    }

}
