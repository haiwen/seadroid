package com.seafile.seadroid2.ui.fragment;

import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.Html;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AlertDialog;
import androidx.preference.ListPreference;
import androidx.preference.Preference;
import androidx.preference.PreferenceFragmentCompat;
import androidx.preference.SwitchPreferenceCompat;

import com.blankj.utilcode.util.AppUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.bumptech.glide.Glide;
import com.google.common.collect.Maps;
import com.hjq.permissions.OnPermissionCallback;
import com.hjq.permissions.Permission;
import com.hjq.permissions.XXPermissions;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.cameraupload.CameraUploadConfigActivity;
import com.seafile.seadroid2.cameraupload.CameraUploadManager;
import com.seafile.seadroid2.data.CameraSyncEvent;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ServerInfo;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.folderbackup.FolderBackupConfigActivity;
import com.seafile.seadroid2.folderbackup.FolderBackupDBHelper;
import com.seafile.seadroid2.folderbackup.FolderBackupEvent;
import com.seafile.seadroid2.folderbackup.FolderBackupSelectedPathActivity;
import com.seafile.seadroid2.folderbackup.RepoConfig;
import com.seafile.seadroid2.folderbackup.selectfolder.StringTools;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.activity.CreateGesturePasswordActivity;
import com.seafile.seadroid2.ui.activity.PrivacyPolicyActivity;
import com.seafile.seadroid2.ui.activity.SeafilePathChooserActivity;
import com.seafile.seadroid2.ui.activity.SettingsActivity;
import com.seafile.seadroid2.ui.dialog.ClearCacheTaskDialog;
import com.seafile.seadroid2.ui.dialog.ClearPasswordTaskDialog;
import com.seafile.seadroid2.ui.dialog.SwitchStorageTaskDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog.TaskDialogListener;
import com.seafile.seadroid2.util.CameraSyncStatus;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import org.apache.commons.io.FileUtils;
import org.greenrobot.eventbus.EventBus;
import org.greenrobot.eventbus.Subscribe;
import org.greenrobot.eventbus.ThreadMode;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;

public class SettingsFragment extends PreferenceFragmentCompat {
    private static final String DEBUG_TAG = "SettingsFragment";

    public static final String CAMERA_UPLOAD_BOTH_PAGES = "com.seafile.seadroid2.camera.upload";
    public static final String CAMERA_UPLOAD_REMOTE_LIBRARY = "com.seafile.seadroid2.camera.upload.library";
    public static final String FOLDER_BACKUP_REMOTE_PATH = "com.seafile.seadroid2.folder.backup.path";
    public static final String CAMERA_UPLOAD_LOCAL_DIRECTORIES = "com.seafile.seadroid2.camera.upload.directories";
    public static final String FOLDER_BACKUP_REMOTE_LIBRARY = "com.seafile.seadroid2.folder.backup.library";
    public static final int CHOOSE_CAMERA_UPLOAD_REQUEST = 2;
    public static final int CHOOSE_BACKUP_UPLOAD_REQUEST = 5;

    // Account Info
    private static Map<String, AccountInfo> accountInfoMap = Maps.newHashMap();

    // Camera upload
    private SwitchPreferenceCompat mCameraBackupSwitch;

    private Preference mCameraBackupAdvanced;
    private Preference mCameraBackupRepoState;
    private Preference mCameraBackupRepoPref;

    private SettingsActivity mActivity;

    public SettingsManager settingsMgr;
    private CameraUploadManager cameraManager;

    private AccountManager accountMgr;
    private DataManager dataMgr;
    private final StorageManager storageManager = StorageManager.getInstance();

    //folder backup
    private SwitchPreferenceCompat mFolderBackupSwitch;
    private ListPreference mFolderBackupNetworkMode;
    private Preference mFolderBackupRepo;
    private Preference mFolderBackupDirsPref;
    private Preference mFolderBackupState;

    private List<String> backupSelectPaths;
    private FolderBackupDBHelper databaseHelper;
    private RepoConfig selectRepoConfig;

    private final SharedPreferences.OnSharedPreferenceChangeListener spChangeListener = (sharedPreferences, key) -> {
        switch (key) {
            case SettingsManager.SHARED_PREF_STORAGE_DIR: {
                ConcurrentAsyncTask.execute(new UpdateStorageLocationSummaryTask());
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

    public void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onCreate");
        super.onCreate(savedInstanceState);

        if (!Utils.isNetworkOn()) {
            mActivity.showShortToast(mActivity, R.string.network_down);
            return;
        }


        init();

        runTask();
    }

    private void init() {
        //settings manager
        settingsMgr = SettingsManager.instance();
        settingsMgr.registerSharedPreferencesListener(spChangeListener);

        accountMgr = new AccountManager(mActivity);

        Account act = accountMgr.getCurrentAccount();
        dataMgr = new DataManager(act);

        cameraManager = new CameraUploadManager(mActivity.getApplicationContext());
        databaseHelper = FolderBackupDBHelper.getDatabaseHelper();

        String backupPaths = SettingsManager.instance().getBackupPaths();
        if (!TextUtils.isEmpty(backupPaths)) {
            backupSelectPaths = StringTools.getJsonToList(backupPaths);
        }
    }

    private void runTask() {
        Account account = accountMgr.getCurrentAccount();
        ConcurrentAsyncTask.execute(new RequestAccountInfoTask(), account);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        Log.d(DEBUG_TAG, "onDestroy()");
        settingsMgr.unregisterSharedPreferencesListener(spChangeListener);
    }

    @Override
    public void onViewCreated(View view, Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onViewCreated");
        super.onViewCreated(view, savedInstanceState);

        initView();
    }


    private void initView() {

        initAccountView();

        initCameraBackupView();

        initFolderBackupView();

        initCacheView();

        initAboutView();

        initPolicyView();

        refreshCameraUploadView();

        // Cache size
        calculateCacheSize();
    }

    private void initAccountView() {
        // User info
        String identifier = getCurrentUserIdentifier();
        findPreference(SettingsManager.SETTINGS_ACCOUNT_INFO_KEY).setSummary(identifier);

        // Space used
        Account currentAccount = accountMgr.getCurrentAccount();
        if (currentAccount != null) {
            String signature = currentAccount.getSignature();
            AccountInfo info = getAccountInfoBySignature(signature);
            if (info != null) {
                String spaceUsed = info.getSpaceUsed();
                findPreference(SettingsManager.SETTINGS_ACCOUNT_SPACE_KEY).setSummary(spaceUsed);
            }
        }

        //gesture lock
        findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY).setOnPreferenceChangeListener((preference, newValue) -> {
            boolean isChecked = (Boolean) newValue;
            if (isChecked) {
                // inverse checked status
                Intent newIntent = new Intent(getActivity(), CreateGesturePasswordActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivityForResult(newIntent, SettingsManager.GESTURE_LOCK_REQUEST);
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

        //client encrypt
        if (currentAccount == null) {
            return;
        }

        ServerInfo serverInfo = accountMgr.getServerInfo(currentAccount);
        if (serverInfo == null || serverInfo.canLocalDecrypt()) {
            // Client side encryption for encrypted Library
            Preference clientEncPref = findPreference(SettingsManager.CLIENT_ENC_SWITCH_KEY);
            clientEncPref.setVisible(true);
            clientEncPref.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
                @Override
                public boolean onPreferenceChange(Preference preference, Object newValue) {
                    if (newValue instanceof Boolean) {
                        boolean isChecked = (Boolean) newValue;
                        // inverse checked status
                        settingsMgr.setupEncrypt(!isChecked);
                        return true;
                    }

                    return false;
                }
            });
        }
    }

    private void initCameraBackupView() {
        // Camera Upload
        mCameraBackupSwitch = findPreference(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY);
        mCameraBackupAdvanced = findPreference(SettingsManager.CAMERA_UPLOAD_ADVANCED_CATEGORY_KEY);
        mCameraBackupRepoPref = findPreference(SettingsManager.CAMERA_UPLOAD_REPO_KEY);
        mCameraBackupRepoState = findPreference(SettingsManager.CAMERA_UPLOAD_STATE);

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
                startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REQUEST);
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
        mFolderBackupDirsPref = findPreference(SettingsManager.SELECTED_BACKUP_FOLDERS_KEY);
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
                SettingsManager.instance().saveFolderBackupDataPlanAllowed(i != 0);
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
                    intent.putExtra(FOLDER_BACKUP_REMOTE_LIBRARY, true);
                    startActivityForResult(intent, CHOOSE_BACKUP_UPLOAD_REQUEST);
                    return true;
                }
            });
        }

        //
        if (mFolderBackupDirsPref != null) {
            mFolderBackupDirsPref.setOnPreferenceClickListener(preference -> {
                Intent intent;
                if (CollectionUtils.isEmpty(backupSelectPaths)) {
                    intent = new Intent(mActivity, FolderBackupConfigActivity.class);
                } else {
                    intent = new Intent(mActivity, FolderBackupSelectedPathActivity.class);
                }
                intent.putExtra(FOLDER_BACKUP_REMOTE_PATH, true);
                startActivityForResult(intent, CHOOSE_BACKUP_UPLOAD_REQUEST);
                return true;
            });
        }


        setFolderPreferencesVisible(mFolderBackupSwitch.isChecked());
    }

    private void initCacheView() {
        // Cache size
        calculateCacheSize();

        // Clear cache
        findPreference(SettingsManager.SETTINGS_CLEAR_CACHE_KEY).setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(@NonNull Preference preference) {
                clearCache();
                return true;
            }
        });

        // Storage selection only works on KitKat or later
        if (storageManager.supportsMultipleStorageLocations()) {
            updateStorageLocationSummary();
            findPreference(SettingsManager.SETTINGS_CACHE_DIR_KEY).setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    new SwitchStorageTaskDialog().show(getChildFragmentManager(), "Select cache location");
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
        findPreference(SettingsManager.SETTINGS_ABOUT_AUTHOR_KEY).setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(@NonNull Preference preference) {
                AlertDialog.Builder builder = new AlertDialog.Builder(mActivity);
                // builder.setIcon(R.drawable.icon);
                builder.setMessage(Html.fromHtml(getString(R.string.settings_about_author_info, appVersion)));
                builder.show();
                return true;
            }
        });
    }

    private void initPolicyView() {
        String country = Locale.getDefault().getCountry();
        String language = Locale.getDefault().getLanguage();
        if (TextUtils.equals("CN", country) || TextUtils.equals("zh", language)) {
            findPreference(SettingsManager.SETTINGS_PRIVACY_POLICY_KEY).setOnPreferenceClickListener(new Preference.OnPreferenceClickListener() {
                @Override
                public boolean onPreferenceClick(@NonNull Preference preference) {
                    Intent intent = new Intent(mActivity, PrivacyPolicyActivity.class);
                    mActivity.startActivity(intent);
                    return true;
                }
            });
        } else {
            findPreference(SettingsManager.SETTINGS_PRIVACY_POLICY_KEY).setVisible(false);
        }
    }

    private void onPreferenceSignOutClicked() {
        // popup a dialog to confirm sign out request
        final AlertDialog.Builder builder = new AlertDialog.Builder(mActivity);
        builder.setTitle(getString(R.string.settings_account_sign_out_title));
        builder.setMessage(getString(R.string.settings_account_sign_out_confirm));
        builder.setPositiveButton(getString(R.string.confirm), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                Account account = accountMgr.getCurrentAccount();

                // sign out operations
                accountMgr.signOutAccount(account);

                // restart BrowserActivity (will go to AccountsActivity)
                Intent intent = new Intent(mActivity, BrowserActivity.class);
                intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                mActivity.startActivity(intent);
                mActivity.finish();
            }
        });
        builder.setNegativeButton(getString(R.string.cancel), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                // dismiss
                dialog.dismiss();
            }
        });
        builder.show();
    }

    private void onPreferenceFolderBackupSwitchChanged(boolean isChecked) {
        setFolderPreferencesVisible(isChecked);

        if (!isChecked) {
            SettingsManager.instance().saveFolderAutomaticBackup(false);
            return;
        }

        XXPermissions.with(requireContext()).permission(Permission.MANAGE_EXTERNAL_STORAGE).request(new OnPermissionCallback() {

            @Override
            public void onGranted(List<String> permissions, boolean all) {
                if (all) {
                    SettingsManager.instance().saveFolderAutomaticBackup(true);
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
                    intent.putExtra(CAMERA_UPLOAD_BOTH_PAGES, true);
                    startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REQUEST);
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

    private void setFolderPreferencesVisible(boolean isChecked) {
        mFolderBackupNetworkMode.setVisible(isChecked);
        mFolderBackupRepo.setVisible(isChecked);
        mFolderBackupDirsPref.setVisible(isChecked);
        mFolderBackupState.setVisible(isChecked);
    }

    private void setCameraPreferencesVisible(boolean isChecked) {
        mCameraBackupRepoPref.setVisible(isChecked);
        mCameraBackupRepoState.setVisible(isChecked);
        mCameraBackupAdvanced.setVisible(isChecked);
    }

    private void clearPassword() {
        ClearPasswordTaskDialog dialog = new ClearPasswordTaskDialog();
        dialog.setTaskDialogLisenter(new TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                mActivity.showShortToast(mActivity, R.string.clear_password_successful);
            }

            @Override
            public void onTaskFailed(SeafException e) {
                mActivity.showShortToast(mActivity, R.string.clear_password_failed);
            }
        });
        dialog.show(getChildFragmentManager(), "DialogFragment");
    }

    private void updateStorageLocationSummary() {
        String summary = storageManager.getStorageLocation().description;
        findPreference(SettingsManager.SETTINGS_CACHE_DIR_KEY).setSummary(summary);
    }

    private void refreshCameraUploadView() {
        Account camAccount = cameraManager.getCameraAccount();
        String backupEmail = SettingsManager.instance().getBackupEmail();
        if (camAccount != null && settingsMgr.getCameraUploadRepoName() != null) {
            mCameraBackupRepoPref.setSummary(camAccount.getSignature() + "/" + settingsMgr.getCameraUploadRepoName());
        }

        mCameraBackupSwitch.setChecked(cameraManager.isCameraUploadEnabled());

        boolean isFolderAutomaticBackup = SettingsManager.instance().isFolderAutomaticBackup();
        mFolderBackupSwitch.setChecked(isFolderAutomaticBackup);
        if (isFolderAutomaticBackup) {
            if (CollectionUtils.isEmpty(backupSelectPaths)) {
                mFolderBackupDirsPref.setSummary("0");
            } else {
                mFolderBackupDirsPref.setSummary(backupSelectPaths.size() + "");
            }

            if (!TextUtils.isEmpty(backupEmail)) {
                try {
                    selectRepoConfig = databaseHelper.getRepoConfig(backupEmail);
                } catch (Exception e) {
                    Utils.utilsLogInfo(true, "=refreshCameraUploadView=======================" + e);
                }
            }

            if (selectRepoConfig != null && !TextUtils.isEmpty(selectRepoConfig.getRepoName())) {
                mFolderBackupRepo.setSummary(backupEmail + "/" + selectRepoConfig.getRepoName());
            } else {
                mFolderBackupRepo.setSummary(getActivity().getString(R.string.folder_backup_select_repo_hint));
            }
        }
    }

    private void clearCache() {
        ClearCacheTaskDialog dialog = new ClearCacheTaskDialog();
        dialog.setTaskDialogLisenter(new TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                // refresh cache size
                calculateCacheSize();
                //clear Glide cache
                Glide.get(SeadroidApplication.getAppContext()).clearMemory();
                Toast.makeText(mActivity, getString(R.string.settings_clear_cache_success), Toast.LENGTH_SHORT).show();
            }

            @Override
            public void onTaskFailed(SeafException e) {
                Toast.makeText(mActivity, getString(R.string.settings_clear_cache_failed), Toast.LENGTH_SHORT).show();
            }
        });
        dialog.show(getFragmentManager(), "DialogFragment");
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {
            case SettingsManager.GESTURE_LOCK_REQUEST:
                if (resultCode == Activity.RESULT_OK) {


                } else if (resultCode == Activity.RESULT_CANCELED) {
                    ((SwitchPreferenceCompat) findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY)).setChecked(false);
                }
                break;
            case CHOOSE_CAMERA_UPLOAD_REQUEST:
                if (resultCode == Activity.RESULT_OK) {
                    if (data == null) {
                        return;
                    }
                    final String repoName = data.getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
                    final String repoId = data.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
                    final Account account = data.getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
                    if (repoName != null && repoId != null) {
                        // Log.d(DEBUG_TAG, "Activating camera upload to " + account + "; " + repoName);
                        cameraManager.setCameraAccount(account);
                        settingsMgr.saveCameraUploadRepoInfo(repoId, repoName);
                    }

                } else if (resultCode == Activity.RESULT_CANCELED) {

                }
                refreshCameraUploadView();
                break;
            case CHOOSE_BACKUP_UPLOAD_REQUEST:
                if (resultCode == Activity.RESULT_OK) {
                    if (data == null) {
                        return;
                    }

                    final boolean pathOn = data.getBooleanExtra(FolderBackupConfigActivity.BACKUP_SELECT_PATHS_SWITCH, false);
                    final ArrayList<String> pathListExtra = data.getStringArrayListExtra(FolderBackupConfigActivity.BACKUP_SELECT_PATHS);
                    if (pathOn && pathListExtra != null) {
                        if (backupSelectPaths == null) {
                            backupSelectPaths = new ArrayList<>();
                        } else {
                            backupSelectPaths.clear();
                        }
                        backupSelectPaths.addAll(pathListExtra);
                        mFolderBackupDirsPref.setSummary(pathListExtra.size() + "");
                    } else if (pathListExtra == null) {
                        if (backupSelectPaths != null) {
                            backupSelectPaths.clear();
                        }
                        mFolderBackupDirsPref.setSummary("0");
                    }
                }
                refreshCameraUploadView();
                break;

            default:
                break;
        }

    }

    /**
     * automatically update Account info, like space usage, total space size, from background.
     */
    private class RequestAccountInfoTask extends AsyncTask<Account, Void, AccountInfo> {

        @Override
        protected void onPreExecute() {
            mActivity.setSupportProgressBarIndeterminateVisibility(true);
        }

        @Override
        protected AccountInfo doInBackground(Account... params) {
            AccountInfo accountInfo = null;

            if (params == null) return null;

            try {
                // get account info from server
                accountInfo = dataMgr.getAccountInfo();
            } catch (Exception e) {
                Log.e(DEBUG_TAG, "could not get account info!", e);
            }

            return accountInfo;
        }

        @Override
        protected void onPostExecute(AccountInfo accountInfo) {
            mActivity.setSupportProgressBarIndeterminateVisibility(false);

            if (accountInfo == null) return;

            // update Account info settings
            findPreference(SettingsManager.SETTINGS_ACCOUNT_INFO_KEY).setSummary(getCurrentUserIdentifier());
            String spaceUsage = accountInfo.getSpaceUsed();
            findPreference(SettingsManager.SETTINGS_ACCOUNT_SPACE_KEY).setSummary(spaceUsage);
            Account currentAccount = accountMgr.getCurrentAccount();
            if (currentAccount != null)
                saveAccountInfo(currentAccount.getSignature(), accountInfo);
        }
    }

    public String getCurrentUserIdentifier() {
        Account account = accountMgr.getCurrentAccount();

        if (account == null)
            return "";

        return account.getDisplayName();
    }

    public void saveAccountInfo(String signature, AccountInfo accountInfo) {
        accountInfoMap.put(signature, accountInfo);
    }

    public AccountInfo getAccountInfoBySignature(String signature) {
        if (accountInfoMap.containsKey(signature))
            return accountInfoMap.get(signature);
        else
            return null;
    }

    private void calculateCacheSize() {
        ConcurrentAsyncTask.execute(new CalculateCacheTask());
    }

    private class CalculateCacheTask extends AsyncTask<String, Void, Long> {

        @Override
        protected Long doInBackground(String... params) {
            return storageManager.getUsedSpace();
        }

        @Override
        protected void onPostExecute(Long aLong) {
            String total = FileUtils.byteCountToDisplaySize(aLong);
            findPreference(SettingsManager.SETTINGS_CACHE_SIZE_KEY).setSummary(total);
        }

    }

    private class UpdateStorageLocationSummaryTask extends AsyncTask<Void, Void, Void> {

        @Override
        protected Void doInBackground(Void... params) {
            return null;
        }

        @Override
        protected void onPostExecute(Void ret) {
            updateStorageLocationSummary();
        }

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

}
