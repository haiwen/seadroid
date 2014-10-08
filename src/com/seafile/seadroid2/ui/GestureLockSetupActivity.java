package com.seafile.seadroid2.ui;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.preference.PreferenceManager;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.ui.GestureLockPatternView.Cell;
import com.seafile.seadroid2.ui.GestureLockPatternView.DisplayMode;

public class GestureLockSetupActivity extends Activity implements
        GestureLockPatternView.OnPatternListener, OnClickListener {

    private static final String TAG = "GestureLockSetupActivity";
    private GestureLockPatternView lockPatternView;
    private Button leftButton;
    private Button rightButton;

    private static final int STEP_1 = 1; // begin
    private static final int STEP_2 = 2; // first setup lock
    private static final int STEP_3 = 3; // click go on button
    private static final int STEP_4 = 4; // second setup lock

    private int step;

    private List<Cell> choosePattern;

    private boolean confirm = false;
    private Intent backIntent = new Intent();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_lock_setup);
        lockPatternView = (GestureLockPatternView) findViewById(R.id.lock_pattern);
        lockPatternView.setOnPatternListener(this);
        leftButton = (Button) findViewById(R.id.left_btn);
        rightButton = (Button) findViewById(R.id.right_btn);
        

        step = STEP_1;
        updateView();
    }

    private void updateView() {
        switch (step) {
        case STEP_1:
            leftButton.setText(R.string.cancel);
            rightButton.setText(R.string.go_on);
            rightButton.setEnabled(false);
            choosePattern = null;
            confirm = false;
            lockPatternView.clearPattern();
            lockPatternView.enableInput();
            break;
        case STEP_2:
            leftButton.setText(R.string.try_again);
            rightButton.setText(R.string.go_on);
            rightButton.setEnabled(true);
            lockPatternView.disableInput();
            break;
        case STEP_3:
            leftButton.setText(R.string.cancel);
            rightButton.setText(R.string.confirm);
            rightButton.setEnabled(false);
            lockPatternView.clearPattern();
            lockPatternView.enableInput();
            break;
        case STEP_4:
            leftButton.setText(R.string.cancel);
            if (confirm) {
                rightButton.setText(R.string.confirm);
                rightButton.setEnabled(true);
                lockPatternView.disableInput();
            } else {
                rightButton.setText(R.string.confirm);
                lockPatternView.setDisplayMode(DisplayMode.Wrong);
                lockPatternView.enableInput();
                rightButton.setEnabled(false);
            }

            break;

        default:
            break;
        }
    }

    @Override
    public void onClick(View v) {

        switch (v.getId()) {
        case R.id.left_btn:
            if (step == STEP_1 || step == STEP_3 || step == STEP_4) {
                SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this);
                settings.edit().putString(SettingsManager.LOCK_KEY,null).commit();
                
                backIntent.putExtra("setupSuccess", false);
                this.setResult(RESULT_OK, backIntent);
                finish();
            } else if (step == STEP_2) {
                step = STEP_1;
                updateView();
            }
            break;

        case R.id.right_btn:
            if (step == STEP_2) {
                step = STEP_3;
                updateView();
            } else if (step == STEP_4) {

                SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this);
                settings.edit().putString(SettingsManager.LOCK_KEY, GestureLockPatternView.patternToString(choosePattern)).commit();

                backIntent.putExtra("setupSuccess", true);
                this.setResult(RESULT_OK, backIntent);
                finish();
            }

            break;

        default:
            break;
        }

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
    }

    @Override
    public void onPatternDetected(List<Cell> pattern) {
        Log.d(TAG, "onPatternDetected");

        if (pattern.size() < GestureLockPatternView.MIN_LOCK_PATTERN_SIZE) {
            Toast.makeText(this,
                    R.string.lockpattern_recording_incorrect_too_short,
                    Toast.LENGTH_LONG).show();
            lockPatternView.setDisplayMode(DisplayMode.Wrong);
            return;
        }

        if (choosePattern == null) {
            choosePattern = new ArrayList<Cell>(pattern);
            Log.d(TAG, "choosePattern = "+Arrays.toString(choosePattern.toArray()));
         
            step = STEP_2;
            updateView();
            return;
        }   
        
        Log.d(TAG, "choosePattern = "+Arrays.toString(choosePattern.toArray()));
        Log.d(TAG, "pattern = "+Arrays.toString(pattern.toArray()));
        
        if (choosePattern.equals(pattern)) {
            Log.d(TAG, "pattern = "+Arrays.toString(pattern.toArray()));
           
            confirm = true;
        } else {
            confirm = false;
        }
      
        step = STEP_4;
        updateView();

    }

}
