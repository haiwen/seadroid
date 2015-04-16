package com.seafile.seadroid2.ui.fragment;

import java.io.File;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.*;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.AsyncTask;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceCategory;
import android.preference.PreferenceScreen;
import android.support.v4.content.LocalBroadcastManager;
import android.text.Html;
import android.util.Log;
import android.util.TypedValue;
import android.view.View;
import android.widget.Button;
import android.widget.Toast;

import com.google.common.collect.Maps;
import com.seafile.seadroid2.*;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.cameraupload.CameraUploadService;
import com.seafile.seadroid2.cameraupload.CameraUploadConfigActivity;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
import com.seafile.seadroid2.transfer.TransferManager;
import com.seafile.seadroid2.ui.SeafileStyleDialogBuilder;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.activity.AccountsActivity;
import com.seafile.seadroid2.ui.activity.CreateGesturePasswordActivity;
import com.seafile.seadroid2.ui.activity.SeafilePathChooserActivity;
import com.seafile.seadroid2.ui.activity.SettingsActivity;
import com.seafile.seadroid2.ui.dialog.ClearCacheTaskDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog.TaskDialogListener;
import com.seafile.seadroid2.util.Utils;
import org.json.JSONException;

import java.io.IOException;
import java.util.Map;

public class SettingsPreferenceFragment extends CustomPreferenceFragment implements
        OnPreferenceChangeListener, OnPreferenceClickListener {
    private static final String DEBUG_TAG = "SettingsPreferenceFragment";

    public static final String CAMERA_UPLOAD_BOTH_PAGES = "com.seafile.seadroid2.camera.upload";
    public static final String CAMERA_UPLOAD_REMOTE_LIBRARY = "com.seafile.seadroid2.camera.upload.library";
    public static final String CAMERA_UPLOAD_LOCAL_DIRECTORIES = "com.seafile.seadroid2.camera.upload.directories";

    // Account Info
    private static Map<String, AccountInfo> accountInfoMap = Maps.newHashMap();

    private Preference actInfoPref;
    private Preference spaceAvailablePref;
    private Preference signOutPref;
    private CheckBoxPreference gestureLockSwitch;
    private PreferenceCategory cameraUploadCategory;
    private PreferenceScreen cameraUploadAdvancedScreen;
    private PreferenceCategory cameraUploadAdvancedCategory;
    private CheckBoxPreference cameraUploadSwitch;
    private CheckBoxPreference allowMobileConnections;
    private CheckBoxPreference allowVideoUpload;
    private CheckBoxPreference cameraUploadCustomDirSwitch;
    private Preference cameraUploadRepo;
    private Preference cameraLocalDirectories;
    private Preference versionName;
    private Preference authorInfo;
    private Preference cacheSizePrf;
    private Preference clearCache;
    private CheckBoxPreference clickToCloseGalleryPref;
    private SettingsActivity mActivity;
    private Intent cameraUploadIntent;
    private boolean isUploadEnabled;
    private boolean isCustomUploadDirectoriesEnabled;
    private Intent cUploadIntent;
    private String repoName;
    private String customDirs;
    private String appVersion;
    private SettingsManager settingsMgr;
    private AccountManager accountMgr;
    private DataManager dataMgr;

    @Override
    public void onAttach(Activity activity) {
        Log.d(DEBUG_TAG, "onAttach");
        super.onAttach(activity);

        // global variables
        mActivity = (SettingsActivity) getActivity();
        settingsMgr = SettingsManager.instance();
        accountMgr = new AccountManager(mActivity);
        Account act = settingsMgr.getCurrentAccount();
        dataMgr = new DataManager(act);

        LocalBroadcastManager
                .getInstance(mActivity)
                .registerReceiver(transferReceiver,
                        new IntentFilter(TransferManager.BROADCAST_ACTION));
    }

    @Override
    public void onDetach() {
        Log.d(DEBUG_TAG, "onDetach");
        super.onDetach();

        LocalBroadcastManager
                .getInstance(mActivity)
                .unregisterReceiver(transferReceiver);
        transferReceiver = null;
    }

    public void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onCreate");
        super.onCreate(savedInstanceState);

        Account account = accountMgr.getCurrentAccount();
        if (!Utils.isNetworkOn()) {
            ToastUtils.show(mActivity, R.string.network_down);
            return;
        }

        ConcurrentAsyncTask.execute(new RequestAccountInfoTask(), account);

    }

    @Override
    public void onViewCreated(View view, Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onViewCreated");
        super.onViewCreated(view, savedInstanceState);

        addPreferencesFromResource(R.xml.settings);

        // Account
        actInfoPref = findPreference(SettingsManager.SETTINGS_ACCOUNT_INFO_KEY);
        spaceAvailablePref = findPreference(SettingsManager.SETTINGS_ACCOUNT_SPACE_KEY);

        String identifier = getCurrentUserIdentifier();
        String signature = accountMgr.getCurrentAccount().getSignature();
        AccountInfo info = getAccountInfoBySignature(signature);
        if (info != null) {
            String spaceUsed = info.getSpaceUsed();
            spaceAvailablePref.setSummary(spaceUsed);
        }

        actInfoPref.setSummary(identifier);
        signOutPref = findPreference(SettingsManager.SETTINGS_ACCOUNT_SIGN_OUT_KEY);
        signOutPref.setOnPreferenceClickListener(this);

        // Gesture Lock
        gestureLockSwitch = (CheckBoxPreference) findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY);
        gestureLockSwitch.setOnPreferenceChangeListener(this);
        gestureLockSwitch.setOnPreferenceClickListener(this);
        gestureLockSwitch.setChecked(settingsMgr.isGestureLockEnabled());

        // Camera Upload
        cameraUploadCategory = (PreferenceCategory) findPreference(SettingsManager.CAMERA_UPLOAD_CATEGORY_KEY);
        cameraUploadAdvancedScreen = (PreferenceScreen) findPreference(SettingsManager.CAMERA_UPLOAD_ADVANCED_SCREEN_KEY);
        cameraUploadAdvancedCategory = (PreferenceCategory) findPreference(SettingsManager.CAMERA_UPLOAD_ADVANCED_CATEGORY_KEY);
        cameraUploadSwitch = (CheckBoxPreference) findPreference(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY);
        cameraUploadRepo = findPreference(SettingsManager.CAMERA_UPLOAD_REPO_KEY);
        cameraLocalDirectories = findPreference(SettingsManager.CAMERA_UPLOAD_DIRECTORY_KEY);
        allowMobileConnections = (CheckBoxPreference) findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY);
        allowVideoUpload = (CheckBoxPreference) findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY);
        cameraUploadCustomDirSwitch = (CheckBoxPreference) findPreference(SettingsManager.CAMERA_UPLOAD_CUSTOM_DIRECTORIES_KEY);
        cameraUploadAdvancedCategory.setOnPreferenceClickListener(this);
        cameraUploadCategory.setOnPreferenceClickListener(this);
        cameraUploadSwitch.setOnPreferenceClickListener(this);
        cameraUploadRepo.setOnPreferenceClickListener(this);
        cameraLocalDirectories.setOnPreferenceClickListener(this);
        allowMobileConnections.setOnPreferenceClickListener(this);
        allowVideoUpload.setOnPreferenceClickListener(this);
        cameraUploadCustomDirSwitch.setOnPreferenceClickListener(this);

        cameraUploadIntent = new Intent(mActivity, CameraUploadService.class);
        repoName = settingsMgr.getCameraUploadRepoName();
        customDirs = settingsMgr.getLocalDirPath();

        if (repoName != null) {
            cameraUploadRepo.setSummary(repoName);
            cameraUploadRepo.setDefaultValue(repoName);
        } else {
            cameraUploadSwitch.setChecked(false);
            cameraUploadCategory.removePreference(cameraUploadRepo);
            cameraUploadCategory.removePreference(cameraUploadAdvancedScreen);
        }

        if (!cameraUploadSwitch.isChecked()) {
            cameraUploadCategory.removePreference(cameraUploadRepo);
            cameraUploadCategory.removePreference(cameraUploadAdvancedScreen);
        } else {
            cameraUploadCategory.addPreference(cameraUploadRepo);
            cameraUploadCategory.addPreference(cameraUploadAdvancedScreen);
        }

        if (customDirs != null) {
            cameraUploadAdvancedCategory.addPreference(cameraLocalDirectories);
            cameraLocalDirectories.setSummary(customDirs);
        } else {
            cameraUploadAdvancedCategory.removePreference(cameraLocalDirectories);
            cameraUploadCustomDirSwitch.setChecked(false);
        }

        // About
        versionName = findPreference(SettingsManager.SETTINGS_ABOUT_VERSION_KEY);
        try {
            appVersion = mActivity.getPackageManager().getPackageInfo(mActivity.getPackageName(), 0).versionName;
        } catch (NameNotFoundException e) {
            e.printStackTrace();
        }
        versionName.setSummary(appVersion);

        authorInfo = findPreference(SettingsManager.SETTINGS_ABOUT_AUTHOR_KEY);
        authorInfo.setOnPreferenceClickListener(this);
        // Cache
        cacheSizePrf = findPreference(SettingsManager.SETTINGS_CACHE_SIZE_KEY);
        calculateCacheSize();

        // Clear cache
        clearCache = findPreference(SettingsManager.SETTINGS_CLEAR_CACHE_KEY);
        clearCache.setOnPreferenceClickListener(this);

        // Other
        clickToCloseGalleryPref = (CheckBoxPreference) findPreference(SettingsManager.SETTINGS_OTHER_CLICK_TO_CLOSE_GALLERY);
        clickToCloseGalleryPref.setOnPreferenceChangeListener(new OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                return true;
            }
        });

        LocalBroadcastManager
        .getInstance(getActivity().getApplicationContext())
        .registerReceiver(transferReceiver,
                new IntentFilter(TransferManager.BROADCAST_ACTION));

    }

    @Override
    public void onResume() {
        super.onResume();
        gestureLockSwitch.setChecked(settingsMgr.isGestureLockEnabled());
        allowMobileConnections.setChecked(settingsMgr.isDataPlanAllowed());
        allowVideoUpload.setChecked(settingsMgr.isVideosUploadAllowed());
        if (!settingsMgr.isCustomScanDir())
            cameraUploadAdvancedCategory.removePreference(cameraLocalDirectories);
        else {
            cameraUploadAdvancedCategory.addPreference(cameraLocalDirectories);
            if (settingsMgr.getLocalDirPath() != null)
                cameraLocalDirectories.setSummary(settingsMgr.getLocalDirPath());
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
    }

    @Override
    public boolean onPreferenceClick(Preference preference) {
        if (preference.getKey().equals(SettingsManager.SETTINGS_ACCOUNT_SIGN_OUT_KEY)) {
            // popup a dialog to confirm sign out request
            final SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(mActivity);
            builder.setTitle(getString(R.string.settings_account_sign_out_title));
            builder.setMessage(getString(R.string.settings_account_sign_out_confirm));
            builder.setPositiveButton(getString(R.string.confirm), new DialogInterface.OnClickListener() {
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    Account account = accountMgr.getCurrentAccount();
                    // stop camera upload service if on
                    if (SettingsManager.instance().getCameraUploadAccountEmail() != null) {
                        if (SettingsManager.instance().getCameraUploadAccountEmail().equals(account.getEmail())
                                &&
                                SettingsManager.instance().getCameraUploadAccountServer().equals(account.getServer())) {
                            Intent cameraUploadIntent = new Intent(mActivity, CameraUploadService.class);
                            mActivity.stopService(cameraUploadIntent);
                        }
                    }

                    // sign out operations
                    accountMgr.signOutCurrentAccount();

                    // navigate to AccountsActivity
                    Intent intent = new Intent(mActivity, AccountsActivity.class);
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
            final AlertDialog dialog = builder.show();
            Button okButton = dialog.getButton(AlertDialog.BUTTON_POSITIVE);
            okButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, getResources().getDimension(R.dimen.dialog_btn_txt_size));
            Button cancelButton = dialog.getButton(AlertDialog.BUTTON_NEGATIVE);
            cancelButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, getResources().getDimension(R.dimen.dialog_btn_txt_size));

        } else if (preference.getKey().equals(SettingsManager.GESTURE_LOCK_SWITCH_KEY)) {

            if (!settingsMgr.isGestureLockEnabled()) {
                Intent newIntent = new Intent(getActivity(), CreateGesturePasswordActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivityForResult(newIntent, SettingsManager.GESTURE_LOCK_REQUEST);

            } else {
                LockPatternUtils mLockPatternUtils = new LockPatternUtils(getActivity());
                mLockPatternUtils.clearLock();
                gestureLockSwitch.setChecked(false);
            }
        } else if (preference.getKey().equals(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY)) {
            isUploadEnabled = settingsMgr.isCameraUploadEnabled();
            if (!isUploadEnabled) {
                cameraUploadCategory.removePreference(cameraUploadRepo);
                cameraUploadCategory.removePreference(cameraUploadAdvancedScreen);
                startCameraUploadService(false);
            } else {
                cameraUploadCategory.addPreference(cameraUploadRepo);
                cameraUploadCategory.addPreference(cameraUploadAdvancedScreen);
                startCameraUploadService(true);
            }
        } else if (preference.getKey().equals(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY)) {
            // user does not allow mobile connections, stop camera upload service
            if (!settingsMgr.checkCameraUploadNetworkAvailable()) {
                startCameraUploadService(false);
            }
        } else if (preference.getKey().equals(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY)) {
            settingsMgr.saveVideosAllowed(allowVideoUpload.isChecked());
        } else if (preference.getKey().equals(SettingsManager.CAMERA_UPLOAD_CUSTOM_DIRECTORIES_KEY)) {
            isCustomUploadDirectoriesEnabled = settingsMgr.isCustomScanDir();
            if (!isCustomUploadDirectoriesEnabled) {
                cameraUploadAdvancedCategory.removePreference(cameraLocalDirectories);
                scanCustomDirs(false);
            } else {
                cameraUploadAdvancedCategory.addPreference(cameraLocalDirectories);
                scanCustomDirs(true);
            }
        } else if (preference.getKey().equals(SettingsManager.CAMERA_UPLOAD_REPO_KEY)) {
            // choose remote library
            Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
            intent.putExtra(CAMERA_UPLOAD_REMOTE_LIBRARY, true);
            startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST);
        } else if (preference.getKey().equals(SettingsManager.CAMERA_UPLOAD_DIRECTORY_KEY)) {
            // choose local directories
            Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
            intent.putExtra(CAMERA_UPLOAD_LOCAL_DIRECTORIES, true);
            startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST);
        } else if(preference.getKey().equals(SettingsManager.SETTINGS_ABOUT_AUTHOR_KEY)) {
            SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(mActivity);
            // builder.setIcon(R.drawable.icon);
            builder.setTitle(mActivity.getResources().getString(R.string.app_name));
            builder.setMessage(Html.fromHtml(getString(R.string.settings_about_author_info, versionName)));
            builder.show();
        } else if (preference.getKey().equals(SettingsManager.SETTINGS_CLEAR_CACHE_KEY)) {
            clearCache();
        }
        return true;
    }

    private void clearCache() {
        String filesDir = dataMgr.getAccountDir();
        String cacheDir = DataManager.getExternalCacheDirectory();
        String tempDir = DataManager.getExternalTempDirectory();
        String thumbDir = DataManager.getThumbDirectory();

        ClearCacheTaskDialog dialog = new ClearCacheTaskDialog();
        Account account = settingsMgr.getCurrentAccount();
        dialog.init(account, filesDir, cacheDir, tempDir, thumbDir);
        dialog.setTaskDialogLisenter(new TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                // refresh cache size
                cacheSizePrf.setSummary(getString(R.string.settings_cache_empty));
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
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        return false;
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {

        case SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST:
            if (resultCode == Activity.RESULT_OK) {
                if (data == null) {
                    return;
                }
                cUploadIntent = data;
                repoName = cUploadIntent.getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
                String repoId = cUploadIntent.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
                if (repoName != null && repoId != null) {
                    // stop camera upload service
                    startCameraUploadService(false);
                    String dir = cUploadIntent.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
                    Account account = cUploadIntent.getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
                    settingsMgr.saveCameraUploadRepoInfo(repoId, repoName, dir, account);
                    cameraUploadRepo.setSummary(repoName);
                    settingsMgr.saveCameraUploadRepoName(repoName);
                    // start camera upload service
                    startCameraUploadService(true);
                    cameraUploadCategory.addPreference(cameraUploadRepo);
                    cameraUploadCategory.addPreference(cameraUploadAdvancedScreen);
                }

                if (!settingsMgr.isCustomScanDir()) {
                    cameraUploadCustomDirSwitch.setChecked(false);
                    cameraUploadAdvancedCategory.removePreference(cameraLocalDirectories);
                } else {

                    customDirs = cUploadIntent.getStringExtra(SeafilePathChooserActivity.DATA_DIRECTORY_PATH);
                    if (customDirs != null) {
                        cameraLocalDirectories.setSummary(customDirs);
                        cameraUploadAdvancedCategory.addPreference(cameraLocalDirectories);
                        cameraUploadCustomDirSwitch.setChecked(settingsMgr.isCustomScanDir());
                    }
                }
            } else if (resultCode == Activity.RESULT_CANCELED) {
                if (repoName == null) {
                    cameraUploadSwitch.setChecked(false);

                    cameraUploadCategory.removePreference(cameraUploadRepo);
                    cameraUploadCategory.removePreference(cameraUploadAdvancedScreen);
                }

                if (customDirs == null) {
                    cameraUploadCustomDirSwitch.setChecked(false);
                    cameraUploadAdvancedCategory.removePreference(cameraLocalDirectories);
                }
            }
           break;

        default:
            break;
        }

    }

    private void startCameraUploadService(boolean isStart) {
        if (!isStart) {
            // stop camera upload service
            mActivity.stopService(cameraUploadIntent);
        } else {
            if (repoName != null) {
                // show remote library name
                cameraUploadRepo.setSummary(repoName);
                //start service
                mActivity.startService(cameraUploadIntent);
            } else {
                // Pop-up window to let user choose remote library
                Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
                intent.putExtra(CAMERA_UPLOAD_BOTH_PAGES, true);
                startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST);
                return;
            }

        }
    }

    private void scanCustomDirs(boolean isCustomScanOn) {
        if (!isCustomScanOn)
            return;

        if (customDirs != null) {
            cameraLocalDirectories.setSummary(customDirs);
            return;
        }

        Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
        intent.putExtra(CAMERA_UPLOAD_LOCAL_DIRECTORIES, true);
        startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST);
    }

    private BroadcastReceiver transferReceiver = new BroadcastReceiver() {

        @Override
        public void onReceive(Context context, Intent intent) {

            String type = intent.getStringExtra("type");
            if (type == null) {
                return;
            }

            if (type.equals(CameraUploadService.BROADCAST_CAMERA_UPLOAD_LIBRARY_NOT_FOUND)) {
                repoName = null;
                cameraUploadRepo.setSummary(R.string.settings_camera_upload_repo_hint);
                settingsMgr.saveCameraUploadRepoName(null);
                cameraUploadSwitch.setChecked(false);
                cameraUploadRepo.setEnabled(false);
                startCameraUploadService(false);
                ToastUtils.show(mActivity, R.string.settings_camera_upload_lib_not_found);
            } else if (type.equals(CameraUploadService.BROADCAST_CAMERA_UPLOAD_SERVICE_STARTED)) {
                // settings_camera_upload_service_started);
            } else if (type.equals(CameraUploadService.BROADCAST_CAMERA_UPLOAD_SERVICE_STOPPED)) {
                // settings_camera_upload_service_stopped);
            }
        }
    };

    /**
     * automatically update Account info, like space usage, total space size, from background.
     */
    class RequestAccountInfoTask extends AsyncTask<Account, Void, AccountInfo> {

        @Override
        protected void onPreExecute() {
            mActivity.setSupportProgressBarIndeterminateVisibility(true);
        }

        @Override
        protected AccountInfo doInBackground(Account... params) {
            AccountInfo accountInfo = null;

            if (params == null) return null;

            Account account = params[0];
            SeafConnection seafConnection = new SeafConnection(account);
            try {
                // get account info from server
                String actInfo = seafConnection.getAccountInfo();
                // parse raw data
                accountInfo = accountMgr.parseAccountInfo(actInfo);
            } catch (IOException e) {
                e.printStackTrace();
            } catch (SeafException e) {
                e.printStackTrace();
            } catch (JSONException e) {
                e.printStackTrace();
            }

            if (accountInfo != null)
                accountInfo.setServer(account.getServer());

            return accountInfo;
        }

        @Override
        protected void onPostExecute(AccountInfo accountInfo) {
            mActivity.setSupportProgressBarIndeterminateVisibility(false);

            if (accountInfo == null) return;

            // update Account info settings
            actInfoPref.setSummary(getCurrentUserIdentifier());
            String spaceUsage = accountInfo.getSpaceUsed();
            spaceAvailablePref.setSummary(spaceUsage);
            saveAccountInfo(accountMgr.getCurrentAccount().getSignature(), accountInfo);
        }
    }

    public String getCurrentUserIdentifier() {
        Account account = settingsMgr.getCurrentAccount();

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
        String filesDir = dataMgr.getAccountDir();
        String cacheDir = DataManager.getExternalCacheDirectory();
        String tempDir = DataManager.getExternalTempDirectory();
        String thumbDir = DataManager.getThumbDirectory();

        ConcurrentAsyncTask.execute(new CalculateCacheTask(), filesDir, cacheDir, tempDir, thumbDir);
    }

    class CalculateCacheTask extends AsyncTask<String, Void, Long> {

        @Override
        protected Long doInBackground(String... params) {
            if (params ==  null) return 0l;
            String filesDir = params[0];
            String cacheDir = params[1];
            String tempDir = params[2];
            String thumbDir = params[3];
            File files = new File(filesDir);
            File caches = new File(cacheDir);
            File temp = new File(tempDir);
            File thumb = new File(thumbDir);

            long cacheSize = Utils.getDirSize(files) + Utils.getDirSize(caches) + Utils.getDirSize(temp) + Utils.getDirSize(thumb);
            return cacheSize;
        }

        @Override
        protected void onPostExecute(Long aLong) {
            String total = Utils.readableFileSize(aLong);
            cacheSizePrf.setSummary(total);
        }

    }

}
