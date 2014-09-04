package com.seafile.seadroid2.ui;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceFragment;
import android.preference.PreferenceManager;
import android.util.Log;
import android.widget.Toast;

import com.seafile.seadroid2.AccountsActivity;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.sync.CameraUploadService;
import com.seafile.seadroid2.util.Utils;

@SuppressLint("NewApi")
public class SettingsPreferenceFragment extends PreferenceFragment implements OnPreferenceChangeListener,   
OnPreferenceClickListener {

private static final String DEBUG_TAG = "SettingsPreferenceFragment";
    
    public static final String PKG = "com.seafile.seadroid2";
    public static final String EXTRA_CAMERA_UPLOAD = PKG + ".camera.upload";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_ID = PKG + ".spf.camera.repoid";
    public static final String SHARED_PREF_CAMERA_UPLOAD_REPO_NAME = PKG + ".spf.camera.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL = PKG + ".spf.camera.account.email";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER = PKG + ".spf.camera.account.server";
    public static final String SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN = PKG + ".spf.camera.account.token";
    public static final String SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME = PKG + ".spf.camera.settings.repoName";
    public static final String SHARED_PREF_CAMERA_UPLOAD_SETTINGS_START = PKG + ".spf.camera.settings.startService";
    private static final int CHOOSE_CAMERA_UPLOAD_REPO_REQUEST = 1;
    private SharedPreferences sharedPref;
    SharedPreferences.Editor editor;
    private Intent cameraUploadIntent;
    private Intent dstData;
    private boolean is_upload_start = false;
    private String repoName;
    private SettingsActivity mActivity;
    private Preference cameraUploadRepo;
    private CheckBoxPreference gestureLockSwitch;
    private CheckBoxPreference cameraUploadSwitch;
    private boolean gesture_lock_before;
    private boolean setupSuccess = false;
    private static final int Gesture_Lock_REQUEST = 6;

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
        repoName = getRepoName();
        if (repoName != null) {
            cameraUploadRepo.setSummary(repoName);
            cameraUploadRepo.setDefaultValue(repoName);
        }
    }
    
    private void saveRepoName(String repoName) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, repoName);
        editor.commit();
    }
    
    private String getRepoName(){
        return sharedPref.getString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, null);
    }
    @Override
    public boolean onPreferenceClick(Preference preference) {
        if (preference.getKey().equals(BrowserActivity.GESTURE_LOCK_SWITCH_KEY)) {
            SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(getActivity());
            gesture_lock_before = settings.getBoolean(BrowserActivity.GESTURE_LOCK_SWITCH_KEY, false);
            
            if (gesture_lock_before == false) {
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
            is_upload_start = settings.getBoolean(BrowserActivity.CAMERA_UPLOAD_SWITCH_KEY, false);
            startUploadService(is_upload_start);
        }else if (preference.getKey().equals(BrowserActivity.CAMERA_UPLOAD_REPO_KEY)) {
            // Pop-up window to let user choose remote library
            Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
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
                saveRepoInfo(dstRepoId, dstRepoName, dstDir, account);
                repoName = dstRepoName;
                cameraUploadRepo.setSummary(repoName);
                cameraUploadRepo.setDefaultValue(repoName);
                saveRepoName(dstRepoName);
                startUploadService(true);
            }
           break; 
           
        default:
            break;
        }
        
    }
    
    private void startUploadService(Boolean isChecked) {
        if (!isChecked) {
            
            // stop camera upload service
            mActivity.stopService(cameraUploadIntent);
            showToast(R.string.stopUpService);
        }else {
            
            if (repoName != null) {
                // show remote library name
                cameraUploadRepo.setSummary(repoName);

            }else {
                // Pop-up window to let user choose remote library
                Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
                this.startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
                return;
            }
            
            if (Utils.isWiFiOn()) {
                //start service
                mActivity.startService(cameraUploadIntent);
                showToast(R.string.startUpService);
            }else {
                mActivity.stopService(cameraUploadIntent);
                showToast(R.string.wifi_down);
            }
        }
    }
    
    private void saveRepoInfo(String repoId, String repoName, String dstDir,
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
}
