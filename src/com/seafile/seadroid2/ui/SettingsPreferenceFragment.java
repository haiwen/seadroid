package com.seafile.seadroid2.ui;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.DialogInterface;
import android.content.DialogInterface.OnClickListener;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
import android.content.pm.PackageManager.NameNotFoundException;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceFragment;
import android.preference.PreferenceManager;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;
import android.widget.Toast;

import com.seafile.seadroid2.AccountsActivity;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.sync.CameraUploadService;
import com.seafile.seadroid2.transfer.TransferService;

@SuppressLint("NewApi")
public class SettingsPreferenceFragment
    extends PreferenceFragment
    implements OnPreferenceChangeListener, OnPreferenceClickListener {

    private static final String DEBUG_TAG = "SettingsPreferenceFragment";

    public static final String PKG = "com.seafile.seadroid2";
    public static final String EXTRA_CAMERA_UPLOAD = PKG + ".camera.upload";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_ID = PKG + ".camera.repoid";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_NAME = PKG + ".camera.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL = PKG + ".camera.account.email";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER = PKG + ".camera.account.server";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN = PKG + ".camera.account.token";
    public static final String SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME = PKG + ".camera.settings.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_SETTINGS_START = PKG + ".camera.settings.startService";
    public static final int CHOOSE_CAMERA_UPLOAD_REPO_REQUEST = 1;
    public static final int GESTURE_LOCK_REQUEST = 6;
    public static final int SETTINGS_FEEDBACK_REPORT_LIKE = 10;
    public static final int SETTINGS_FEEDBACK_REPORT_DISLIKE = 11;
    public static final int SETTINGS_FEEDBACK_REPORT_BUG = 12;
    public static final int SETTINGS_FEEDBACK_NEED_HELP = 13;
    public static final int SETTINGS_FEEDBACK_REPORT_OTHERS = 14;
    private CheckBoxPreference gestureLockSwitch;
    private CheckBoxPreference cameraUploadSwitch;
    private CheckBoxPreference allowMobileConnections;
    private Preference cameraUploadRepo;
    private Preference versionName;
    private Preference feedback;
    private boolean setupSuccess;
    private boolean gestureLockBefore;
    private SharedPreferences sharedPref;
    private SharedPreferences.Editor editor;
    private SettingsActivity mActivity;
    private Intent cameraUploadIntent;
    private boolean isUploadStart;
    private Intent mCameraUploadRepoChooserData;
    private String repoName;
    
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(DEBUG_TAG, "onCreate");
        addPreferencesFromResource(R.xml.settings);

        mActivity = (SettingsActivity) getActivity();
        sharedPref = mActivity.getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);
        editor = sharedPref.edit();

        gestureLockSwitch = (CheckBoxPreference) findPreference(BrowserActivity.GESTURE_LOCK_SWITCH_KEY);
        cameraUploadSwitch = (CheckBoxPreference) findPreference(BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY);
        allowMobileConnections = (CheckBoxPreference) findPreference(BrowserActivity.ALLOW_MOBILE_CONNECTIONS_SWITCH_KEY);
        cameraUploadRepo = (Preference) findPreference(BrowserActivity.CAMERA_UPLOAD_REPO_KEY);
        versionName = findPreference(BrowserActivity.SETTINGS_ABOUT_VERSION_KEY);
        try {
            versionName.setSummary(mActivity.getPackageManager().getPackageInfo(mActivity.getPackageName(), 0).versionName);
        } catch (NameNotFoundException e) {
            e.printStackTrace();
        }
        feedback = findPreference(BrowserActivity.SETTINGS_ABOUT_FEEDBACK_KEY);

        gestureLockSwitch.setOnPreferenceChangeListener(this);
        gestureLockSwitch.setOnPreferenceClickListener(this);
        if (sharedPref.getBoolean(BrowserActivity.GESTURE_LOCK_SWITCH_KEY, false)) {
            gestureLockSwitch.setChecked(true);
        }

        cameraUploadSwitch.setOnPreferenceClickListener(this);
        allowMobileConnections.setOnPreferenceClickListener(this);
        feedback.setOnPreferenceClickListener(this);
        cameraUploadRepo.setOnPreferenceClickListener(this);
        cameraUploadIntent = new Intent(mActivity, CameraUploadService.class);
        repoName = getCameraUploadRepoName();
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

        LocalBroadcastManager
                .getInstance(getActivity().getApplicationContext())
                .registerReceiver(transferReceiver,
                        new IntentFilter(TransferService.BROADCAST_ACTION));
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        LocalBroadcastManager
                .getInstance(getActivity().getApplicationContext())
                .unregisterReceiver(transferReceiver);
        transferReceiver = null;
    }

    private void saveCameraUploadRepoName(String repoName) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, repoName);
        editor.commit();
    }

    private String getCameraUploadRepoName() {
        return sharedPref.getString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, null);
    }
    @Override
    public boolean onPreferenceClick(Preference preference) {
        SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(mActivity);
        if (preference.getKey().equals(BrowserActivity.GESTURE_LOCK_SWITCH_KEY)) {
            gestureLockBefore = settings.getBoolean(BrowserActivity.GESTURE_LOCK_SWITCH_KEY, false);

            if (gestureLockBefore == false) {
                Intent newIntent = new Intent(getActivity(), GestureLockSetupActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivityForResult(newIntent, GESTURE_LOCK_REQUEST);

            } else {
                SharedPreferences.Editor editor = settings.edit();
                editor.putBoolean(BrowserActivity.GESTURE_LOCK_SWITCH_KEY, false);
                editor.putString(BrowserActivity.LOCK_KEY, null);
                editor.commit();
                gestureLockSwitch.setChecked(false);
            }
        } else if (preference.getKey().equals(BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY)) {
            isUploadStart = settings.getBoolean(BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY, false);
            if (!isUploadStart) {
                cameraUploadRepo.setEnabled(false);
                allowMobileConnections.setEnabled(false);
                startCameraUploadService(false);
            } else {
                allowMobileConnections.setEnabled(true);
                cameraUploadRepo.setEnabled(true);
                startCameraUploadService(true);
            }
        } else if (preference.getKey().equals(BrowserActivity.ALLOW_MOBILE_CONNECTIONS_SWITCH_KEY)) {
            // no task here
        } else if (preference.getKey().equals(BrowserActivity.CAMERA_UPLOAD_REPO_KEY)) {
            // Pop-up window to let user choose remote library
            Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
            intent.putExtra(EXTRA_CAMERA_UPLOAD, true);
            this.startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
        } else if (preference.getKey().equals(BrowserActivity.SETTINGS_ABOUT_FEEDBACK_KEY)) {
            SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(mActivity);
            builder.setTitle(mActivity.getResources().getString(R.string.settings_about_feedback_title));
            builder.setItems(R.array.settings_feedback_entries, new OnClickListener() {
                
                @Override
                public void onClick(DialogInterface dialog, int which) {
                    Intent intent = new Intent();
                    intent.setClass(mActivity, FeedbackActivity.class);
                    switch (which) {
                    case 0:
                        // users like some features
                        intent.setFlags(SettingsPreferenceFragment.SETTINGS_FEEDBACK_REPORT_LIKE);
                        break;
                    case 1:
                        // users dislike some features
                        intent.setFlags(SettingsPreferenceFragment.SETTINGS_FEEDBACK_REPORT_DISLIKE);
                        break;
                    case 2:
                        // users report bugs
                        intent.setFlags(SettingsPreferenceFragment.SETTINGS_FEEDBACK_REPORT_BUG);
                        break;
                    case 3:
                        // users need help
                        intent.setFlags(SettingsPreferenceFragment.SETTINGS_FEEDBACK_NEED_HELP);
                        break;
                    case 4:
                        // users report something else
                        intent.setFlags(SettingsPreferenceFragment.SETTINGS_FEEDBACK_REPORT_OTHERS);
                        break;
                        
                    default:
                        break;
                    }
                    startActivity(intent);
                }
            });
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
        case GESTURE_LOCK_REQUEST:
            if (resultCode == Activity.RESULT_OK) {
                setupSuccess = data.getBooleanExtra("setupSuccess", true);
                SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(getActivity());
                SharedPreferences.Editor editor = settings.edit();

                if (setupSuccess == true) {
                    showToast(R.string.setup_gesture_lock_success);
                    editor.putBoolean(BrowserActivity.GESTURE_LOCK_SWITCH_KEY, true);
                    gestureLockSwitch.setChecked(true);
                } else {
                    editor.putBoolean(BrowserActivity.GESTURE_LOCK_SWITCH_KEY, false);
                    gestureLockSwitch.setChecked(false);
                }

                editor.commit();
            }

            break;

        case CHOOSE_CAMERA_UPLOAD_REPO_REQUEST:
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
                saveCameraUploadRepoInfo(repoId, repoName, dir, account);
                this.repoName = repoName;
                cameraUploadRepo.setSummary(repoName);
                cameraUploadRepo.setDefaultValue(repoName);
                saveCameraUploadRepoName(repoName);
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
                this.startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
                return;
            }

            //start service
            mActivity.startService(cameraUploadIntent);
        }
    }

    private void saveCameraUploadRepoInfo(String repoId, String repoName, String dstDir,
            Account account) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_ID, repoId);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, repoName);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL,
                account.getEmail());
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER,
                account.getServer());
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN,
                account.getToken());
        editor.commit();
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
                saveCameraUploadRepoName(null);
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
