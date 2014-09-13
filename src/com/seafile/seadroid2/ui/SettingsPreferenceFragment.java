package com.seafile.seadroid2.ui;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.SharedPreferences;
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
import com.seafile.seadroid2.util.Utils;

@SuppressLint("NewApi")
public class SettingsPreferenceFragment extends PreferenceFragment implements OnPreferenceChangeListener,   
OnPreferenceClickListener {

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
    public static final int CHOOSE_CAMERA_UPLOAD_REPO_ONLY = 2;
    private static final int Gesture_Lock_REQUEST = 6;
    private CheckBoxPreference gestureLockSwitch;
    private CheckBoxPreference cameraUploadSwitch;
    private boolean setupSuccess = false;
    private boolean gestureLockBefore;
    private SharedPreferences sharedPref;
    private SharedPreferences.Editor editor;
    private SettingsActivity mActivity;
    private Intent cameraUploadIntent;
    private Preference cameraUploadRepo;
    private boolean isUploadStart = false;
    private Intent dstData;
    private String repoName;

    @SuppressLint("NewApi")
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(DEBUG_TAG, "onCreate");
        addPreferencesFromResource(R.xml.settings);
        
        gestureLockSwitch = (CheckBoxPreference) findPreference(BrowserActivity.GESTURE_LOCK_SWITCH_KEY); 
        cameraUploadSwitch = (CheckBoxPreference) findPreference(BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY); 
        cameraUploadRepo = (Preference) findPreference(BrowserActivity.CAMERA_UPLOAD_REPO_KEY); 
        gestureLockSwitch.setOnPreferenceChangeListener(this);
        gestureLockSwitch.setOnPreferenceClickListener(this);
        
        cameraUploadSwitch.setOnPreferenceClickListener(this);
        cameraUploadRepo.setOnPreferenceClickListener(this);
        mActivity = (SettingsActivity) getActivity();
        cameraUploadIntent = new Intent(mActivity, CameraUploadService.class);
        sharedPref = mActivity.getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);
        editor = sharedPref.edit();
        repoName = getCameraUploadRepoName();
        if (repoName != null) {
            cameraUploadRepo.setSummary(repoName);
            cameraUploadRepo.setDefaultValue(repoName);
        }
        
        if (!cameraUploadSwitch.isChecked()) {
            cameraUploadRepo.setEnabled(false);
        }else {
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
        if (preference.getKey().equals(BrowserActivity.GESTURE_LOCK_SWITCH_KEY)) {
            SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(getActivity());
            gestureLockBefore = settings.getBoolean(BrowserActivity.GESTURE_LOCK_SWITCH_KEY, false);
            
            if (gestureLockBefore == false) {
                Intent newIntent = new Intent(getActivity(), GestureLockSetupActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivityForResult(newIntent, Gesture_Lock_REQUEST);
                
            } else {
                SharedPreferences.Editor editor = settings.edit();
                editor.putBoolean(BrowserActivity.GESTURE_LOCK_SWITCH_KEY, false);
                editor.putString(BrowserActivity.LOCK_KEY, null);
                editor.commit();
                gestureLockSwitch.setChecked(false);
            }
        }else if (preference.getKey().equals(BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY)) {
            SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(mActivity);
            isUploadStart = settings.getBoolean(BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY, false);
            startCameraUploadService(isUploadStart);
            if (!isUploadStart) {
                cameraUploadRepo.setEnabled(false);
            }else {
                if (Utils.isWiFiOn()) {
                    cameraUploadRepo.setEnabled(true);
                }
            }
        }else if (preference.getKey().equals(BrowserActivity.CAMERA_UPLOAD_REPO_KEY)) {
            mActivity.stopService(cameraUploadIntent);
            // Pop-up window to let user choose remote library
            Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
            intent.putExtra(EXTRA_CAMERA_UPLOAD, true);
            this.startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
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
        case Gesture_Lock_REQUEST:
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
                dstData = data;
                if (dstData == null) {
                    return;
                }
                String dstRepoId, dstRepoName, dstDir;
                Account account;
                dstRepoName = dstData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
                dstRepoId = dstData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
                dstDir = dstData.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
                account = (Account)dstData.getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
                saveCameraUploadRepoInfo(dstRepoId, dstRepoName, dstDir, account);
                repoName = dstRepoName;
                cameraUploadRepo.setSummary(repoName);
                cameraUploadRepo.setDefaultValue(repoName);
                saveCameraUploadRepoName(dstRepoName);
                startCameraUploadService(true);
            } else if (resultCode == Activity.RESULT_CANCELED) {
                startCameraUploadService(false);
                cameraUploadSwitch.setChecked(false);
                cameraUploadRepo.setEnabled(false);
            }
           break; 
           
        default:
            break;
        }
        
    }
    
    private void startCameraUploadService(Boolean isChecked) {
        if (!isChecked) {
            
            // stop camera upload service
            mActivity.stopService(cameraUploadIntent);
            showToast(R.string.settings_stopUpService);
        }else {
            
            if (repoName != null) {
                // show remote library name
                cameraUploadRepo.setSummary(repoName);

            }else {
                // Pop-up window to let user choose remote library
                Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
                intent.putExtra(EXTRA_CAMERA_UPLOAD, true);
                this.startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
                return;
            }
            
            if (Utils.isWiFiOn()) {
                //start service
                mActivity.startService(cameraUploadIntent);
                showToast(R.string.settings_startUpService);
            }else {
                mActivity.stopService(cameraUploadIntent);
                cameraUploadSwitch.setChecked(false);
                cameraUploadRepo.setEnabled(false);
                showToast(R.string.settings_wifi_down);
            }
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
            }
        }
    };
}
