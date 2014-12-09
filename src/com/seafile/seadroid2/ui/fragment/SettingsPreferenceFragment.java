package com.seafile.seadroid2.ui.fragment;

import android.app.Activity;
import android.content.*;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.support.v4.content.LocalBroadcastManager;
import android.text.Html;
import android.util.Log;
import android.view.View;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.cameraupload.CameraUploadService;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.SeafileStyleDialogBuilder;
import com.seafile.seadroid2.ui.activity.AccountsActivity;
import com.seafile.seadroid2.ui.activity.CreateGesturePasswordActivity;
import com.seafile.seadroid2.ui.activity.SeafilePathChooserActivity;
import com.seafile.seadroid2.ui.activity.SettingsActivity;
import com.seafile.seadroid2.util.Utils;

public class SettingsPreferenceFragment extends CustomPreferenceFragment implements
        OnPreferenceChangeListener, OnPreferenceClickListener {
    private static final String DEBUG_TAG = "SettingsPreferenceFragment";

    public static final String EXTRA_CAMERA_UPLOAD = "com.seafile.seadroid2.camera.upload";
    private Preference actInfoPref;
    private Preference spaceAvailablePref;
    private Preference signOutPref;
    private CheckBoxPreference gestureLockSwitch;
    private CheckBoxPreference cameraUploadSwitch;
    private CheckBoxPreference allowMobileConnections;
    private Preference cameraUploadRepo;
    private Preference versionName;
    private SettingsActivity mActivity;
    private Intent cameraUploadIntent;
    private boolean isUploadEnabled;
    private Intent mCameraUploadRepoChooserData;
    private String repoName;
    private String appVersion;
    private SettingsManager settingsMgr;
    private AccountManager accountMgr;

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);

        // global variables
        mActivity = (SettingsActivity) getActivity();
        settingsMgr = SettingsManager.instance();
        accountMgr = new AccountManager(mActivity);

        LocalBroadcastManager
                .getInstance(mActivity)
                .registerReceiver(transferReceiver,
                        new IntentFilter(TransferService.BROADCAST_ACTION));
    }

    @Override
    public void onDetach() {
        super.onDetach();

        LocalBroadcastManager
                .getInstance(mActivity)
                .unregisterReceiver(transferReceiver);
        transferReceiver = null;
    }

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(DEBUG_TAG, "onCreate");
    }

    @Override
    public void onViewCreated(View view, Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        addPreferencesFromResource(R.xml.settings);

        // Account
        actInfoPref = findPreference(SettingsManager.SETTINGS_ACCOUNT_INFO_KEY);
        AccountInfo actInfo = accountMgr.getCurrentAccountInfo();
        actInfoPref.setSummary(actInfo.getEmail());
        spaceAvailablePref = findPreference(SettingsManager.SETTINGS_ACCOUNT_SPACE_KEY);
        spaceAvailablePref.setSummary(Utils.readableFileSize(actInfo.getUsage()) + "/" + Utils.readableFileSize(actInfo.getTotal()));
        signOutPref = findPreference(SettingsManager.SETTINGS_ACCOUNT_SIGN_OUT_KEY);
        signOutPref.setOnPreferenceClickListener(this);

        // Gesture Lock
        gestureLockSwitch = (CheckBoxPreference) findPreference(SettingsManager.GESTURE_LOCK_SWITCH_KEY);
        gestureLockSwitch.setOnPreferenceChangeListener(this);
        gestureLockSwitch.setOnPreferenceClickListener(this);
        gestureLockSwitch.setChecked(settingsMgr.isGestureLockEnabled());

        // Camera Upload
        cameraUploadSwitch = (CheckBoxPreference) findPreference(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY);
        cameraUploadRepo = (Preference) findPreference(SettingsManager.CAMERA_UPLOAD_REPO_KEY);
        allowMobileConnections = (CheckBoxPreference) findPreference(SettingsManager.ALLOW_MOBILE_CONNECTIONS_SWITCH_KEY);
        cameraUploadSwitch.setOnPreferenceClickListener(this);
        cameraUploadRepo.setOnPreferenceClickListener(this);
        allowMobileConnections.setOnPreferenceClickListener(this);

        cameraUploadIntent = new Intent(mActivity, CameraUploadService.class);
        repoName = settingsMgr.getCameraUploadRepoName();

        if (repoName != null) {
            cameraUploadRepo.setSummary(repoName);
            cameraUploadRepo.setDefaultValue(repoName);
        } else {
            cameraUploadSwitch.setChecked(false);
            allowMobileConnections.setEnabled(false);
            cameraUploadRepo.setEnabled(false);
        }

        if (!cameraUploadSwitch.isChecked()) {
            allowMobileConnections.setEnabled(false);
            cameraUploadRepo.setEnabled(false);
        } else {
            allowMobileConnections.setEnabled(true);
            cameraUploadRepo.setEnabled(true);
        }

        // About
        versionName = findPreference(SettingsManager.SETTINGS_ABOUT_VERSION_KEY);
        versionName.setOnPreferenceClickListener(this);
        try {
            appVersion = mActivity.getPackageManager().getPackageInfo(mActivity.getPackageName(), 0).versionName;
        } catch (NameNotFoundException e) {
            e.printStackTrace();
        }
        versionName.setSummary(appVersion);
    }

    @Override
    public void onResume() {
        super.onResume();
        gestureLockSwitch.setChecked(settingsMgr.isGestureLockEnabled());
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
            builder.show();

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
                cameraUploadRepo.setEnabled(false);
                allowMobileConnections.setEnabled(false);
                startCameraUploadService(false);
            } else {
                allowMobileConnections.setEnabled(true);
                cameraUploadRepo.setEnabled(true);
                startCameraUploadService(true);
            }
        } else if (preference.getKey().equals(SettingsManager.ALLOW_MOBILE_CONNECTIONS_SWITCH_KEY)) {
            // user does not allow mobile connections, stop camera upload service
            if (!settingsMgr.checkCameraUploadNetworkAvailable()) {
                startCameraUploadService(false);
            }
        } else if (preference.getKey().equals(SettingsManager.CAMERA_UPLOAD_REPO_KEY)) {
            // Pop-up window to let user choose remote library
            Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
            intent.putExtra(EXTRA_CAMERA_UPLOAD, true);
            this.startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
        } else if(preference.getKey().equals(SettingsManager.SETTINGS_ABOUT_VERSION_KEY)) {
            SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(mActivity);
            builder.setIcon(R.drawable.icon);            
            builder.setTitle(mActivity.getResources().getString(R.string.app_name));
            builder.setMessage(Html.fromHtml(getString(R.string.settings_about_version_info, versionName)));
            builder.show();
        }
        return true;
    }

    @Override
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        return false;
    }

    public void showToast(CharSequence msg) {
        Context context = getActivity().getApplicationContext();
        Toast toast = Toast.makeText(context, msg, Toast.LENGTH_SHORT);
        toast.show();
    }

    public void showToast(int id) {
        showToast(getString(id));
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {

        case SettingsManager.CHOOSE_CAMERA_UPLOAD_REPO_REQUEST:
            if (resultCode == Activity.RESULT_OK) {
                mCameraUploadRepoChooserData = data;
                if (mCameraUploadRepoChooserData == null) {
                    return;
                }
                // stop camera upload service
                startCameraUploadService(false);
                String repoName = mCameraUploadRepoChooserData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
                String repoId = mCameraUploadRepoChooserData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
                String dir = mCameraUploadRepoChooserData.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
                Account account = (Account)mCameraUploadRepoChooserData.getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
                settingsMgr.saveCameraUploadRepoInfo(repoId, repoName, dir, account);
                this.repoName = repoName;
                cameraUploadRepo.setSummary(repoName);
                cameraUploadRepo.setDefaultValue(repoName);
                settingsMgr.saveCameraUploadRepoName(repoName);
                // start camera upload service
                startCameraUploadService(true);
            } else if (resultCode == Activity.RESULT_CANCELED && repoName == null) { // repoName is null when first initialized
                cameraUploadSwitch.setChecked(false);
                allowMobileConnections.setEnabled(false);
                cameraUploadRepo.setEnabled(false);
            }
           break;

        default:
            break;
        }

    }

    private void startCameraUploadService(Boolean isStart) {
        if (!isStart) {
            // stop camera upload service
            mActivity.stopService(cameraUploadIntent);
        } else {
            if (repoName != null) {
                // show remote library name
                cameraUploadRepo.setSummary(repoName);
            } else {
                // Pop-up window to let user choose remote library
                Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
                intent.putExtra(EXTRA_CAMERA_UPLOAD, true);
                this.startActivityForResult(intent, SettingsManager.CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
                return;
            }

            //start service
            mActivity.startService(cameraUploadIntent);
        }
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
                cameraUploadRepo.setSummary(R.string.settings_hint);
                settingsMgr.saveCameraUploadRepoName(null);
                cameraUploadSwitch.setChecked(false);
                cameraUploadRepo.setEnabled(false);
                startCameraUploadService(false);
                showToast(R.string.settings_camera_upload_library_not_found);
            } else if (type.equals(CameraUploadService.BROADCAST_CAMERA_UPLOAD_SERVICE_STARTED)) {
                // showToast(R.string.settings_startUpService);
            } else if (type.equals(CameraUploadService.BROADCAST_CAMERA_UPLOAD_SERVICE_STOPPED)) {
                // showToast(R.string.settings_stopUpService);
            }
        }
    };
}
