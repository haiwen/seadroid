package com.seafile.seadroid2.ui;

import java.util.List;

import android.app.Activity;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.KeyEvent;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.ui.GestureLockPatternView.Cell;
import com.seafile.seadroid2.ui.GestureLockPatternView.DisplayMode;


public class GestureLockActivity extends Activity implements
        GestureLockPatternView.OnPatternListener {
    private static final String TAG = "GestureLockActivity";

    private List<Cell> lockPattern;
    private GestureLockPatternView lockPatternView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this);
        String patternString = settings.getString(SettingsManager.LOCK_KEY, null);
        if (patternString == null) {
            finish();
            return;
        }
        lockPattern = GestureLockPatternView.stringToPattern(patternString);
        setContentView(R.layout.activity_lock);
        lockPatternView = (GestureLockPatternView) findViewById(R.id.lock_pattern);
        lockPatternView.setOnPatternListener(this);

    }

    @Override
    public boolean onKeyDown(int keyCode, KeyEvent event) {

        // disable back key
        if (keyCode == KeyEvent.KEYCODE_BACK) {

            return true;
        }

        return super.onKeyDown(keyCode, event);
    }


    @Override
    public void onPatternStart() {
        Log.d(TAG, "onPatternStart");
    }

    @Override
    public void onPatternCleared() {
        Log.d(TAG, "onPatternCleared");
    }

    @Override
    public void onPatternCellAdded(List<Cell> pattern) {
        Log.d(TAG, "onPatternCellAdded");
        Log.e(TAG, GestureLockPatternView.patternToString(pattern));
        // Toast.makeText(this, LockPatternView.patternToString(pattern),
        // Toast.LENGTH_LONG).show();
    }

    @Override
    public void onPatternDetected(List<Cell> pattern) {
        Log.d(TAG, "onPatternDetected");

        if (pattern.equals(lockPattern)) {
            finish();
        } else {
            lockPatternView.setDisplayMode(DisplayMode.Wrong);
            Toast.makeText(this, R.string.lockpattern_error, Toast.LENGTH_LONG)
                    .show();
        }

    }

}
