package com.seafile.seadroid2.ui;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.BrowserActivity;

import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.Handler;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceFragment;
import android.util.Log;

public class SettingsPreferenceFragment extends PreferenceFragment implements OnPreferenceChangeListener,   
OnPreferenceClickListener {

    private CheckBoxPreference pinLockSwitch;
    
    private boolean pin_lock_before;
    private boolean setupSuccess = false;
    private String gesture_lock_key;
    private static final int Gesture_Lock_REQUEST = 6;
    private static Handler handler=new Handler();
    private boolean setupActivityFinish = false;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.settings);
        
        pinLockSwitch = (CheckBoxPreference) findPreference(BrowserActivity.PIN_LOCK_SWITCH_KEY); 
        pinLockSwitch.setOnPreferenceChangeListener(this);
        pinLockSwitch.setOnPreferenceClickListener(this);
        
    }
    
    @Override
    public boolean onPreferenceClick(Preference preference) {
        // TODO Auto-generated method stub
        /*if (preference.getKey().equals(BrowserActivity.PIN_LOCK_SWITCH_KEY)) {
            SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(getActivity());
            pin_lock_before = settings.getBoolean(BrowserActivity.PIN_LOCK_SWITCH_KEY, false);
            
            if (pin_lock_before == false) {
                Intent newIntent = new Intent(getActivity(), GestureLockSetupActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivityForResult(newIntent, Gesture_Lock_REQUEST);
                SharedPreferences.Editor editor = settings.edit();
                
                if (setupSuccess == true) {
                    editor.putBoolean(BrowserActivity.PIN_LOCK_SWITCH_KEY, true);
                } else {
                    editor.putBoolean(BrowserActivity.PIN_LOCK_SWITCH_KEY, false);
                }
    
                editor.commit();
            } else {
                SharedPreferences.Editor editor = settings.edit();
                editor.putBoolean(BrowserActivity.PIN_LOCK_SWITCH_KEY, false);
                editor.putString(BrowserActivity.LOCK_KEY, null);
                editor.commit();
            }
        }*/
        return true;
    }

    @Override
    public boolean onPreferenceChange(Preference preference, Object newValue) {
        /*if (preference.getKey().equals(BrowserActivity.PIN_LOCK_SWITCH_KEY)) {
            if (((pin_lock_before == false)&&(setupSuccess == true))||(pin_lock_before == true)) {
                return true;
            } else {
                return false;
            }
        }
        return true;*/
        if (preference.getKey().equals(BrowserActivity.PIN_LOCK_SWITCH_KEY)) {
            SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(getActivity());
            pin_lock_before = settings.getBoolean(BrowserActivity.PIN_LOCK_SWITCH_KEY, false);
            
            if (pin_lock_before == false) {
                
                SharedPreferences.Editor editor = settings.edit();
                Intent newIntent = new Intent(getActivity(), GestureLockSetupActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivityForResult(newIntent, Gesture_Lock_REQUEST);
                
                if (setupSuccess == true) {
                    editor.putBoolean(BrowserActivity.PIN_LOCK_SWITCH_KEY, true);
                    editor.commit();
                    return true;
                } else {
                    editor.putBoolean(BrowserActivity.PIN_LOCK_SWITCH_KEY, false);
                    editor.commit();
                    return false;
                }
    
                
            } else {
                SharedPreferences.Editor editor = settings.edit();
                editor.putBoolean(BrowserActivity.PIN_LOCK_SWITCH_KEY, false);
                editor.putString(BrowserActivity.LOCK_KEY, null);
                editor.commit();
                return true;
            }
        }
        return true;
    }

    @Override
    public void onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {
        case Gesture_Lock_REQUEST:
            if (resultCode == getActivity().RESULT_OK) {
                setupSuccess = data.getBooleanExtra("setupSuccess", true);
                setupActivityFinish = true;
            }
        }
    }
}
