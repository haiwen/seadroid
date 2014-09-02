package com.seafile.seadroid2.ui;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.BrowserActivity;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.CheckBoxPreference;
import android.preference.Preference;
import android.preference.PreferenceManager;
import android.preference.Preference.OnPreferenceChangeListener;
import android.preference.Preference.OnPreferenceClickListener;
import android.preference.PreferenceFragment;
import android.widget.Toast;

public class SettingsPreferenceFragment extends PreferenceFragment implements OnPreferenceChangeListener,   
OnPreferenceClickListener {

    private CheckBoxPreference gestureLockSwitch;
    
    private boolean gesture_lock_before;
    private boolean setupSuccess = false;
    private static final int Gesture_Lock_REQUEST = 6;

    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        addPreferencesFromResource(R.xml.settings);
        
        gestureLockSwitch = (CheckBoxPreference) findPreference(BrowserActivity.GESTURE_LOCK_SWITCH_KEY); 
        gestureLockSwitch.setOnPreferenceChangeListener(this);
        gestureLockSwitch.setOnPreferenceClickListener(this);
        
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
            if (resultCode == getActivity().RESULT_OK) {
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
        }
    }
}
