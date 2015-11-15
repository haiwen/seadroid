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
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.activity.CreateGesturePasswordActivity;
import com.seafile.seadroid2.ui.activity.SeafilePathChooserActivity;
import com.seafile.seadroid2.ui.activity.SettingsActivity;
import com.seafile.seadroid2.ui.dialog.ClearCacheTaskDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog.TaskDialogListener;
import com.seafile.seadroid2.util.Utils;
import org.json.JSONException;

import java.io.IOException;
import java.util.Map;

public class SettingsFragment extends CustomPreferenceFragment {
    private static final String DEBUG_TAG = "SettingsFragment";

    public static final String CAMERA_UPLOAD_BOTH_PAGES = "com.seafile.seadroid2.camera.upload";
    public static final String CAMERA_UPLOAD_REMOTE_LIBRARY = "com.seafile.seadroid2.camera.upload.library";
    public static final String CAMERA_UPLOAD_LOCAL_DIRECTORIES = "com.seafile.seadroid2.camera.upload.directories";

    // Account Info
    private static Map<String, AccountInfo> accountInfoMap = Maps.newHashMap();

    // Camera upload
    private PreferenceCategory cUploadCategory;
    private PreferenceScreen cUploadAdvancedScreen;
    private PreferenceCategory cUploadAdvancedCategory;
    private Preference cUploadRepoPref;
    private CheckBoxPreference cCustomDirectoriesPref;
    private Preference cLocalDirectoriesPref;
    private Intent cUploadIntent;
    private String repoName;
    private String customDirs;

    private SettingsActivity mActivity;
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
        Account act = accountMgr.getCurrentAccount();
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

        // User info
        String identifier = getCurrentUserIdentifier();
        findPreference(SettingsManager.SETTINGS_ACCOUNT_INFO_KEY).setSummary(identifier);

        // Space used
        Account currentAccount = accountMgr.getCurrentAccount();
        String signature = currentAccount.getSignature();
        AccountInfo info = getAccountInfoBySignature(signature);
        if (info != null) {
            String spaceUsed = info.getSpaceUsed();
            findPreference(SettingsManager.SETTINGS_ACCOUNT_SPACE_KEY).setSummary(spaceUsed);
        }

        // Sign out
        findPreference(SettingsManager.SETTINGS_ACCOUNT_SIGN_OUT_KEY).setOnPreferenceClickListener(new OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference) {

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
                                mActivity.stopService(cUploadIntent);
                            }
                        }

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
                final AlertDialog dialog = builder.show();
                Button okButton = dialog.getButton(AlertDialog.BUTTON_POSITIVE);
                okButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, getResources().getDimension(R.dimen.dialog_btn_txt_size));
                Button cancelButton = dialog.getButton(AlertDialog.BUTTON_NEGATIVE);
                cancelButton.setTextSize(TypedValue.COMPLEX_UNIT_PX, getResources().getDimension(R.dimen.dialog_btn_txt_size));

                return true;
            }
        });

        // Gesture Lock
        findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY).setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                if (newValue instanceof Boolean) {
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
                }

                return false;
            }
        });

        // Camera Upload
        cUploadCategory = (PreferenceCategory) findPreference(SettingsManager.CAMERA_UPLOAD_CATEGORY_KEY);
        cUploadAdvancedScreen = (PreferenceScreen) findPreference(SettingsManager.CAMERA_UPLOAD_ADVANCED_SCREEN_KEY);
        cUploadAdvancedCategory = (PreferenceCategory) findPreference(SettingsManager.CAMERA_UPLOAD_ADVANCED_CATEGORY_KEY);

        findPreference(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY).setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                if (newValue instanceof Boolean) {
                    boolean isChecked = (Boolean) newValue;
                    if (!isChecked) {
                        cUploadCategory.removePreference(cUploadRepoPref);
                        cUploadCategory.removePreference(cUploadAdvancedScreen);
                        startCameraUploadService(false);
                    } else {
                        cUploadCategory.addPreference(cUploadRepoPref);
                        cUploadCategory.addPreference(cUploadAdvancedScreen);
                        startCameraUploadService(true);
                    }
                    return true;
                }

                return false;
            }
        });

        // Change upload library
        cUploadRepoPref = findPreference(SettingsManager.CAMERA_UPLOAD_REPO_KEY);
        cUploadRepoPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference) {

                // choose remote library
                Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
                intent.putExtra(CAMERA_UPLOAD_REMOTE_LIBRARY, true);
                startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST);

                return true;
            }
        });

        // Camera upload allow data plan
        findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY).setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                if (newValue instanceof Boolean) {
                    Log.d(DEBUG_TAG, "data plan pref changed " + newValue);
                    // user does not allow mobile connections, stop camera upload service
                    if (!settingsMgr.checkCameraUploadNetworkAvailable()) {
                        startCameraUploadService(false);
                    }

                    return true;
                }
                return false;
            }
        });

        // Camera upload allow videos upload
        findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY).setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                return true;
            }
        });

        // change local folder CheckBoxPreference
        cCustomDirectoriesPref = (CheckBoxPreference) findPreference(SettingsManager.CAMERA_UPLOAD_CUSTOM_DIRECTORIES_KEY);
        cCustomDirectoriesPref.setOnPreferenceChangeListener(new Preference.OnPreferenceChangeListener() {
            @Override
            public boolean onPreferenceChange(Preference preference, Object newValue) {
                if (newValue instanceof Boolean) {
                    boolean isCustom = (Boolean) newValue;
                    if (!isCustom) {
                        cUploadAdvancedCategory.removePreference(cLocalDirectoriesPref);
                        scanCustomDirs(false);
                    } else {
                        cUploadAdvancedCategory.addPreference(cLocalDirectoriesPref);
                        scanCustomDirs(true);
                    }
                    return true;
                }

                return false;
            }
        });

        // change local folder Preference
        cLocalDirectoriesPref = findPreference(SettingsManager.CAMERA_UPLOAD_DIRECTORY_KEY);
        cLocalDirectoriesPref.setOnPreferenceClickListener(new OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference) {

                // choose local directories
                Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
                intent.putExtra(CAMERA_UPLOAD_LOCAL_DIRECTORIES, true);
                startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST);

                return true;
            }
        });

        initCameraUploadData();
        refreshCameraUpladView();

        // App Version
        try {
            appVersion = mActivity.getPackageManager().getPackageInfo(mActivity.getPackageName(), 0).versionName;
        } catch (NameNotFoundException e) {
            Log.e(DEBUG_TAG, "app version name not found exception");
            appVersion = getString(R.string.not_available);
        }
        findPreference(SettingsManager.SETTINGS_ABOUT_VERSION_KEY).setSummary(appVersion);

        // About author
        findPreference(SettingsManager.SETTINGS_ABOUT_AUTHOR_KEY).setOnPreferenceClickListener(new OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference) {

                SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(mActivity);
                // builder.setIcon(R.drawable.icon);
                builder.setTitle(mActivity.getResources().getString(R.string.app_name));
                builder.setMessage(Html.fromHtml(getString(R.string.settings_about_author_info, appVersion)));
                builder.show();
                return true;
            }
        });

        // Cache size
        calculateCacheSize();

        // Clear cache
        findPreference(SettingsManager.SETTINGS_CLEAR_CACHE_KEY).setOnPreferenceClickListener(new OnPreferenceClickListener() {
            @Override
            public boolean onPreferenceClick(Preference preference) {
                clearCache();
                return true;
            }
        });

        LocalBroadcastManager
        .getInstance(getActivity().getApplicationContext())
        .registerReceiver(transferReceiver,
                new IntentFilter(TransferManager.BROADCAST_ACTION));

    }

    private void initCameraUploadData() {
        cUploadIntent = new Intent(mActivity, CameraUploadService.class);
        repoName = settingsMgr.getCameraUploadRepoName();
        customDirs = settingsMgr.getLocalDirPath();
    }

    private void refreshCameraUpladView() {
        if (repoName != null) {
            cUploadRepoPref.setSummary(repoName);
        }

        if (customDirs != null) {
            cLocalDirectoriesPref.setSummary(customDirs);
        }

        // data plan
        ((CheckBoxPreference) findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_DATA_PLAN_SWITCH_KEY)).setChecked(settingsMgr.isDataPlanAllowed());

        // videos
        ((CheckBoxPreference)findPreference(SettingsManager.CAMERA_UPLOAD_ALLOW_VIDEOS_SWITCH_KEY)).setChecked(settingsMgr.isVideosUploadAllowed());

        if (repoName == null
                || !settingsMgr.isCameraUploadEnabled()) {
            cUploadCategory.removePreference(cUploadRepoPref);
            cUploadCategory.removePreference(cUploadAdvancedScreen);
            settingsMgr.saveCameraUploadEnabled(false);
            ((CheckBoxPreference)findPreference(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY)).setChecked(false);
        } else {
            cUploadCategory.addPreference(cUploadRepoPref);
            cUploadCategory.addPreference(cUploadAdvancedScreen);
        }

        if (customDirs == null
                || !settingsMgr.isCustomScanDir())
            cUploadAdvancedCategory.removePreference(cLocalDirectoriesPref);
        else {
            // custom upload folders
            cCustomDirectoriesPref.setChecked(settingsMgr.isCustomScanDir());

            cUploadAdvancedCategory.addPreference(cLocalDirectoriesPref);
        }

    }

    private void clearCache() {
        String filesDir = dataMgr.getAccountDir();
        String cacheDir = DataManager.getExternalCacheDirectory();
        String tempDir = DataManager.getExternalTempDirectory();
        String thumbDir = DataManager.getThumbDirectory();

        ClearCacheTaskDialog dialog = new ClearCacheTaskDialog();
        Account account = accountMgr.getCurrentAccount();
        dialog.init(account, filesDir, cacheDir, tempDir, thumbDir);
        dialog.setTaskDialogLisenter(new TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                // refresh cache size
                findPreference(SettingsManager.SETTINGS_CACHE_SIZE_KEY).setSummary(getString(R.string.settings_cache_empty));
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

            case SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST:
                if (resultCode == Activity.RESULT_OK) {
                    if (data == null) {
                        return;
                    }
                    repoName = data.getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
                    final String repoId = data.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
                    final String dir = data.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
                    final Account account = data.getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
                    customDirs = data.getStringExtra(SeafilePathChooserActivity.DATA_DIRECTORY_PATH);
                    if (repoName != null && repoId != null) {
                        // stop camera upload service
                        startCameraUploadService(false);
                        settingsMgr.saveCameraUploadRepoInfo(repoId, repoName, dir, account);
                        // start camera upload service
                        startCameraUploadService(true);
                    }

                } else if (resultCode == Activity.RESULT_CANCELED) {

                }
                refreshCameraUpladView();
                break;

            default:
                break;
        }

    }

    private void startCameraUploadService(boolean isStart) {
        if (!isStart) {
            // stop camera upload service
            mActivity.stopService(cUploadIntent);
        } else {
            if (repoName != null) {
                // show remote library name
                cUploadRepoPref.setSummary(repoName);
                //start service
                mActivity.startService(cUploadIntent);
            } else {
                // Pop-up window to let user choose remote library
                Intent intent = new Intent(mActivity, CameraUploadConfigActivity.class);
                intent.putExtra(CAMERA_UPLOAD_BOTH_PAGES, true);
                startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REQUEST);
            }

        }
    }

    private void scanCustomDirs(boolean isCustomScanOn) {
        if (!isCustomScanOn)
            return;

        if (customDirs != null) {
            cLocalDirectoriesPref.setSummary(customDirs);
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
                cUploadRepoPref.setSummary(R.string.settings_camera_upload_repo_hint);
                settingsMgr.clearCameraUploadInfo();
                ((CheckBoxPreference) findPreference(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY)).setChecked(false);
                cUploadRepoPref.setEnabled(false);
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
            findPreference(SettingsManager.SETTINGS_CACHE_SIZE_KEY).setSummary(total);
        }

    }

}
