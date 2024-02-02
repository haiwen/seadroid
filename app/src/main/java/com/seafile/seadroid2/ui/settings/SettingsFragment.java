package com.seafile.seadroid2.ui.settings;

import static androidx.core.text.HtmlCompat.FROM_HTML_MODE_LEGACY;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.text.Html;
import android.text.Spanned;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.core.text.HtmlCompat;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.preference.ListPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.SwitchPreferenceCompat;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.bumptech.glide.Glide;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.hjq.permissions.OnPermissionCallback;
import com.hjq.permissions.Permission;
import com.hjq.permissions.XXPermissions;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.data.CameraSyncEvent;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ServerInfo;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadConfigActivity;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;
import com.seafile.seadroid2.ui.dialog_fragment.ClearCacheDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.ClearPasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.SignOutDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.SwitchStorageDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupConfigActivity;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupEvent;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupSelectedPathActivity;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;
import com.seafile.seadroid2.ui.gesture.CreateGesturePasswordActivity;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.ui.selector.ObjSelectorActivity;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;
import com.seafile.seadroid2.util.CameraSyncStatus;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.util.sp.FolderBackupConfigSPs;
import com.seafile.seadroid2.util.sp.SettingsManager;
import com.seafile.seadroid2.view.ListPreferenceCompat;

import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

import java.util.List;
import java.util.Locale;

public class SettingsFragment extends PreferenceFragmentCompat {
    private static final String DEBUG_TAG = "SettingsFragment";

    public static final String CAMERA_UPLOAD_REMOTE_LIBRARY = "com.seafile.seadroid2.camera.upload.library";
    public static final String CAMERA_UPLOAD_LOCAL_DIRECTORIES = "com.seafile.seadroid2.camera.upload.directories";
    public static final int CHOOSE_CAMERA_UPLOAD_REQUEST = 2;
    public static final int CHOOSE_BACKUP_UPLOAD_REQUEST = 5;

    private SettingsFragmentViewModel viewModel;
    private SettingsActivityViewModel activityViewModel;

    // Camera upload
    private SwitchPreferenceCompat mCameraBackupSwitch;

    private Preference mCameraBackupAdvanced;
    private Preference mCameraBackupRepoState;
    private Preference mCameraBackupRepoPref;

    private SettingsActivity mActivity;

    private CameraUploadManager cameraManager;
    private DataManager dataMgr;

    //folder backup
    private SwitchPreferenceCompat mFolderBackupSwitch;
    private ListPreference mFolderBackupNetworkMode;
    private Preference mFolderBackupRepo;
    private Preference mFolderBackupFolderPref;
    private Preference mFolderBackupState;

    private RepoConfig selectRepoConfig;

    private final SharedPreferences.OnSharedPreferenceChangeListener spChangeListener = (sharedPreferences, key) -> {
        switch (key) {
            case SettingsManager.SHARED_PREF_STORAGE_DIR: {
                updateStorageLocationSummary();
            }
            break;
            case SettingsManager.FOLDER_BACKUP_MODE: {
                if (mFolderBackupNetworkMode != null) {
                    mFolderBackupNetworkMode.setSummary(mFolderBackupNetworkMode.getEntry());
                }
            }
            break;
        }
    };

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

        init();
    }

    private void init() {
        //settings manager
        SettingsManager.getInstance().registerSharedPreferencesListener(spChangeListener);

        Account act = SupportAccountManager.getInstance().getCurrentAccount();
        dataMgr = new DataManager(act);

        cameraManager = new CameraUploadManager();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        Log.d(DEBUG_TAG, "onDestroy()");
        SettingsManager.getInstance().unregisterSharedPreferencesListener(spChangeListener);
    }

    @Override
    public void onViewCreated(View view, Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onViewCreated");
        super.onViewCreated(view, savedInstanceState);

        initViewModel();

        initView();

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
    }

    private void loadData() {
        viewModel.getAccountInfo();
    }

    private void initView() {

        initAccountView();

        initAppView();

        initCameraBackupView();

        initFolderBackupView();

        initCacheView();

        initAboutView();

        initPolicyView();

        refreshCameraUploadView();
        refreshFolderBackupView();

        // Cache size
        calculateCacheSize();
    }

    private void initAccountView() {
        // Space used
        Account currentAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (currentAccount == null) {
            // User info
            return;
        }

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
                    SettingsManager.getInstance().setupEncrypt(!isChecked);
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
        findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY).setOnPreferenceChangeListener((preference, newValue) -> {
            boolean isChecked = (Boolean) newValue;
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
        mCameraBackupRepoState = findPreference(SettingsManager.CAMERA_UPLOAD_STATE);

        //
        mCameraBackupAdvanced.setFragment(SettingsCameraBackupAdvanceFragment.class.getName());

        mCameraBackupSwitch.setOnPreferenceChangeListener((preference, newValue) -> {
            boolean isBool = newValue instanceof Boolean;
            if (!isBool) {
                return true;
            }

            onPreferenceCameraBackupSwitchChanged((Boolean) newValue);
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

        if (cameraManager.isCameraUploadEnabled()) {
            mCameraBackupRepoState.setSummary(Utils.getUploadStateShow(getActivity()));
        }

        setCameraPreferencesVisible(mCameraBackupSwitch.isChecked());
    }

    private void initFolderBackupView() {
        //folder backup
        mFolderBackupSwitch = findPreference(SettingsManager.FOLDER_BACKUP_SWITCH_KEY);
        mFolderBackupRepo = findPreference(SettingsManager.FOLDER_BACKUP_LIBRARY_KEY);
        mFolderBackupFolderPref = findPreference(SettingsManager.SELECTED_BACKUP_FOLDERS_KEY);
        mFolderBackupState = findPreference(SettingsManager.FOLDER_BACKUP_STATE);
        mFolderBackupNetworkMode = findPreference(SettingsManager.FOLDER_BACKUP_MODE);

        mFolderBackupSwitch.setOnPreferenceChangeListener((preference, newValue) -> {
            boolean isBool = newValue instanceof Boolean;
            if (!isBool) {
                return true;
            }

            onPreferenceFolderBackupSwitchChanged((Boolean) newValue);
            return true;
        });

        //network mode
        if (mFolderBackupNetworkMode != null) {
            mFolderBackupNetworkMode.setOnPreferenceChangeListener((preference, newValue) -> {
                String newString = (String) newValue;
                int i = mFolderBackupNetworkMode.findIndexOfValue(newString);
                SettingsManager.getInstance().saveFolderBackupDataPlanAllowed(i != 0);
                return true;
            });

            String localMode = getPreferenceManager().getSharedPreferences().getString(SettingsManager.FOLDER_BACKUP_MODE, "WIFI");
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

        //repo
        if (mFolderBackupRepo != null) {
            mFolderBackupRepo.setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    Intent intent = new Intent(mActivity, FolderBackupConfigActivity.class);
                    intent.putExtra(FolderBackupConfigActivity.FOLDER_BACKUP_SELECT_MODE, "repo");

                    folderBackupConfigLauncher.launch(intent);
                    return true;
                }
            });
        }

        //
        if (mFolderBackupFolderPref != null) {
            mFolderBackupFolderPref.setOnPreferenceClickListener(preference -> {

                List<String> backupPathList = FolderBackupConfigSPs.getBackupPathList();

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
        bundle.putString("key", preference.getKey());
        dialogFragment.setArguments(bundle);
        dialogFragment.setTargetFragment(this, 0);
//        getParentFragmentManager().setFragmentResultListener();
        dialogFragment.show(getParentFragmentManager(), "androidx.preference.PreferenceFragment.DIALOG");
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

    private void onPreferenceFolderBackupSwitchChanged(boolean isChecked) {
        setFolderPreferencesVisible(isChecked);

        if (!isChecked) {
            SettingsManager.getInstance().saveFolderAutomaticBackup(false);
            return;
        }

        XXPermissions.with(requireContext()).permission(Permission.MANAGE_EXTERNAL_STORAGE).request(new OnPermissionCallback() {

            @Override
            public void onGranted(List<String> permissions, boolean all) {
                if (all) {
                    SettingsManager.getInstance().saveFolderAutomaticBackup(true);
                    refreshCameraUploadView();
                }
            }

            @Override
            public void onDenied(List<String> permissions, boolean never) {
                if (never) {
                    Toast.makeText(getActivity(), mActivity.getString(R.string.authorization_storage_permission), Toast.LENGTH_LONG).show();
                    XXPermissions.startPermissionActivity(getActivity(), permissions);
                } else {
                    Toast.makeText(getActivity(), mActivity.getString(R.string.get_storage_permission_failed), Toast.LENGTH_LONG).show();
                    mFolderBackupSwitch.setChecked(false);
                }
            }
        });
    }

    private void onPreferenceCameraBackupSwitchChanged(boolean isChecked) {
        setCameraPreferencesVisible(isChecked);

        if (!isChecked) {
            cameraManager.disableCameraUpload();
            return;
        }

        XXPermissions.with(requireActivity()).permission(Permission.MANAGE_EXTERNAL_STORAGE).request(new OnPermissionCallback() {

            @Override
            public void onGranted(List<String> permissions, boolean all) {
                if (all) {
                    Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
                    cameraBackupConfigLauncher.launch(intent);
                }
            }

            @Override
            public void onDenied(List<String> permissions, boolean never) {
                if (never) {
                    Toast.makeText(getActivity(), mActivity.getString(R.string.authorization_storage_permission), Toast.LENGTH_LONG).show();
                    XXPermissions.startPermissionActivity(getActivity(), permissions);
                } else {
                    Toast.makeText(getActivity(), mActivity.getString(R.string.get_storage_permission_failed), Toast.LENGTH_LONG).show();
                    mCameraBackupSwitch.setChecked(false);
                }
            }
        });
    }

    private void setFolderPreferencesVisible(boolean isChecked) {
        mFolderBackupNetworkMode.setVisible(isChecked);
        mFolderBackupRepo.setVisible(isChecked);
        mFolderBackupFolderPref.setVisible(isChecked);
        mFolderBackupState.setVisible(isChecked);
    }

    private void setCameraPreferencesVisible(boolean isChecked) {
        mCameraBackupRepoPref.setVisible(isChecked);
        mCameraBackupRepoState.setVisible(isChecked);
        mCameraBackupAdvanced.setVisible(isChecked);
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

    private void refreshCameraUploadView() {
        Account camAccount = cameraManager.getCameraAccount();
        if (camAccount != null && SettingsManager.getInstance().getCameraUploadRepoName() != null) {
            mCameraBackupRepoPref.setSummary(camAccount.getSignature() + "/" + SettingsManager.getInstance().getCameraUploadRepoName());
        }

        mCameraBackupSwitch.setChecked(cameraManager.isCameraUploadEnabled());

        setCameraPreferencesVisible(mCameraBackupSwitch.isChecked());

    }

    private void refreshFolderBackupView() {
        boolean isFolderAutomaticBackup = SettingsManager.getInstance().isFolderAutomaticBackup();
        mFolderBackupSwitch.setChecked(isFolderAutomaticBackup);
        if (!isFolderAutomaticBackup) {
            return;
        }

        String backupEmail = FolderBackupConfigSPs.getBackupEmail();
        if (!TextUtils.isEmpty(backupEmail)) {
            selectRepoConfig = FolderBackupConfigSPs.getBackupConfigByAccount(backupEmail);
        }

        if (selectRepoConfig != null && !TextUtils.isEmpty(selectRepoConfig.getRepoName())) {
            mFolderBackupRepo.setSummary(backupEmail + "/" + selectRepoConfig.getRepoName());
        } else {
            mFolderBackupRepo.setSummary(getString(R.string.folder_backup_select_repo_hint));
        }

        List<String> stringList = FolderBackupConfigSPs.getBackupPathList();
        if (CollectionUtils.isEmpty(stringList)) {
            mFolderBackupFolderPref.setSummary("0");
        } else {
            mFolderBackupFolderPref.setSummary(String.valueOf(stringList.size()));
        }
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

    @Override
    public void onStart() {
        super.onStart();
        EventBus.getDefault().register(this);
    }

    @Override
    public void onStop() {
        super.onStop();
        EventBus.getDefault().unregister(this);
    }

    @Subscribe(threadMode = ThreadMode.MAIN)
    public void onEvent(CameraSyncEvent result) {
        int scanUploadStatus = SeadroidApplication.getInstance().getScanUploadStatus();
        if (cameraManager.isCameraUploadEnabled() && scanUploadStatus > 0) {
            if (scanUploadStatus == CameraSyncStatus.SCAN_END) {
                SeadroidApplication.getInstance().setScanUploadStatus(CameraSyncStatus.NORMAL);
            }
            mCameraBackupRepoState.setSummary(Utils.getUploadStateShow(getActivity()));
        }

    }

    @Subscribe(threadMode = ThreadMode.MAIN)
    public void onEvent(FolderBackupEvent result) {
        int totalBackup = SeadroidApplication.getInstance().getTotalBackup();
        int waitingBackup = SeadroidApplication.getInstance().getWaitingBackup();
        if (mFolderBackupState != null) {
            mFolderBackupState.setSummary(getActivity().getString(R.string.uploaded) + " " + (totalBackup - waitingBackup) + " / " + totalBackup);
        }
    }

    private final ActivityResultLauncher<Intent> folderBackupConfigLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                return;
            }

            ToastUtils.showLong(R.string.folder_backup_select_repo_update);
            refreshFolderBackupView();
        }
    });

    private final ActivityResultLauncher<Intent> cameraBackupConfigLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                refreshCameraUploadView();
                return;
            }

            if (o.getData() == null) {
                return;
            }

            final String repoName = o.getData().getStringExtra(ObjSelectorActivity.DATA_REPO_NAME);
            final String repoId = o.getData().getStringExtra(ObjSelectorActivity.DATA_REPO_ID);
            final Account account = o.getData().getParcelableExtra(ObjSelectorActivity.DATA_ACCOUNT);
            if (repoName != null && repoId != null) {
                cameraManager.setCameraAccount(account);
                SettingsManager.getInstance().saveCameraUploadRepoInfo(repoId, repoName);
            }

            refreshCameraUploadView();
        }
    });

    private final ActivityResultLauncher<Intent> gestureLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o.getResultCode() != Activity.RESULT_OK) {
                ((SwitchPreferenceCompat) findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY)).setChecked(false);
            }
        }
    });

}
