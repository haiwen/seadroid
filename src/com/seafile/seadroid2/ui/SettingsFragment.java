package com.seafile.seadroid2.ui;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.Button;
import android.widget.CompoundButton;
import android.widget.CompoundButton.OnCheckedChangeListener;
import android.widget.TextView;
import android.widget.Toast;
import android.widget.ToggleButton;

import com.actionbarsherlock.app.SherlockFragment;
import com.seafile.seadroid2.AccountsActivity;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.sync.CameraUploadService;
import com.seafile.seadroid2.util.Utils;

public class SettingsFragment extends SherlockFragment{
    private static final String DEBUG_TAG = "SettingsFragment";
    
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
    SharedPreferences sharedPref;
    SharedPreferences.Editor editor;
    Intent cameraUploadIntent;
    private Intent dstData;
    private String repo_name;
    private Boolean isStartUpload = false;
    private BrowserActivity mActivity;
    private NavContext navContext;
    private ToggleButton start_upload_tb;
    private TextView repo_name_tv;
    private Button choose_repo_btn;
    
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Log.d(DEBUG_TAG, "SettingsFragment onCreate");
    }

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        Log.d(DEBUG_TAG, "SettingsFragment Attached");
        mActivity = (BrowserActivity) activity;
        navContext = mActivity.getNavContext();
        cameraUploadIntent = new Intent(mActivity, CameraUploadService.class);
        sharedPref = mActivity.getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);
        editor = sharedPref.edit();
    }

    public NavContext getNavContext() {
        return navContext;
    }
    
    @Override
    public void onDetach() {
        mActivity = null;
        super.onDetach();
        Log.d(DEBUG_TAG, "SettingsFragment onDetach");
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "SettingsFragment onCreateView");
        getPrefSettings();
        Log.d(DEBUG_TAG, "repo_name: " + repo_name);
        View view = inflater.inflate(R.layout.settings, container, false);
        start_upload_tb = (ToggleButton) view.findViewById(R.id.start_upload_cb);
        repo_name_tv = (TextView) view.findViewById(R.id.repo_name_tv);
        choose_repo_btn = (Button) view.findViewById(R.id.chooseRepo);
        if (isStartUpload) {
            start_upload_tb.toggle();
        } 
            
        start_upload_tb
                .setOnCheckedChangeListener(new OnCheckedChangeListener() {

                    @Override
                    public void onCheckedChanged(CompoundButton buttonView,
                            boolean isChecked) {
                        saveUploadServPre(isChecked);
                        choose_repo_btn.setClickable(isChecked);
                        if (!isChecked) {
                            choose_repo_btn.setText("not applicable");
                        }else {
                            choose_repo_btn.setText(R.string.settings_btn);
                        }
                        startUploadService(isChecked);
                        Log.d(DEBUG_TAG, "start_upload_cb: " + isChecked);
                    }
                });
        
        choose_repo_btn.setOnClickListener(new OnClickListener() {
            
            @Override
            public void onClick(View v) {
                mActivity.stopService(cameraUploadIntent);
                Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
                SettingsFragment.this.startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
                
            }
        });
        choose_repo_btn.setClickable(isStartUpload);
        if (!isStartUpload) {
            choose_repo_btn.setText("not applicable");
        }else {
            choose_repo_btn.setText(R.string.settings_btn);
        }
        if (repo_name != null) {
            repo_name_tv.setText(repo_name);
        }
        return view;
    }
    
    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putBoolean("isStartUpload", isStartUpload);
    }

    @Override
    public void onViewStateRestored(Bundle savedInstanceState) {
        super.onViewStateRestored(savedInstanceState);
        if (savedInstanceState!= null) {
            start_upload_tb.setSelected(savedInstanceState.getBoolean("isStartUpload"));
        }
    }
    
    public void showToast(CharSequence msg) {
        Toast toast = Toast.makeText(mActivity, msg, Toast.LENGTH_LONG);
        toast.show();
    }

    public void showToast(int id) {
        showToast(getString(id));
    }
    private void startUploadService(Boolean isChecked) {
        if (!isChecked) {
            
            // stop camera upload service
            mActivity.stopService(cameraUploadIntent);
            showToast(R.string.stopUpService);
        }else {
            
            if (repo_name != null) {
                // show remote repo name
                repo_name_tv.setText(repo_name);

            }else {
                // Pop-up window to let user choose remote repo
                Intent intent = new Intent(mActivity, SeafilePathChooserActivity.class);
                this.startActivityForResult(intent, CHOOSE_CAMERA_UPLOAD_REPO_REQUEST);
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
    
    private void saveRepoInfo(String repoId, String repoName, String dstDir, Account account) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_ID, repoId);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_REPO_NAME, repoName);
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, account.getEmail());
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, account.getServer());
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_TOKEN, account.getToken());
        editor.commit();
    }
    
    private void saveUploadServPre(Boolean isStartUpload) {
        editor.putBoolean(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_START, isStartUpload);
        editor.commit();
    }
    
    private void saveRepoName(String repoName) {
        editor.putString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, repoName);
        editor.commit();
    }
    
    private void getPrefSettings(){
        repo_name = sharedPref.getString(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_REPONAME, null);
        isStartUpload = sharedPref.getBoolean(SHARED_PREF_CAMERA_UPLOAD_SETTINGS_START, false);
    }
   
    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode != CHOOSE_CAMERA_UPLOAD_REPO_REQUEST) {
            return;
        }
        if (resultCode != Activity.RESULT_OK) {
            return;
        }
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
        saveRepoName(dstRepoName);
        repo_name_tv.setText(dstRepoName);
        if (Utils.isWiFiOn()) {
            mActivity.startService(cameraUploadIntent);
            showToast(R.string.startUpService);
        }else {
            mActivity.stopService(cameraUploadIntent);
            showToast(R.string.wifi_down);
        }
    }
    
}