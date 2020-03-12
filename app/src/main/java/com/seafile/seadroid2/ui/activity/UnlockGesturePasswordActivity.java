package com.seafile.seadroid2.ui.activity;

import android.content.Intent;
import android.graphics.Color;
import android.os.Bundle;
import android.os.CountDownTimer;
import android.os.Handler;
import android.support.v7.widget.Toolbar;
import android.view.MenuItem;
import android.view.Window;
import android.view.WindowManager;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.TextView;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
import com.seafile.seadroid2.gesturelock.LockPatternView;
import com.seafile.seadroid2.gesturelock.LockPatternView.Cell;

import java.util.List;


public class UnlockGesturePasswordActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private LockPatternView mLockPatternView;
    private int mFailedPatternAttemptsSinceLastTimeout = 0;
    private CountDownTimer mCountdownTimer = null;
    private Handler mHandler = new Handler();
    private TextView mHeadTextView;
    private Animation mShakeAnim;

    private Toast mToast;

    SettingsManager settingsMgr;
    
    @Override
    public void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_NO_TITLE);
        super.onCreate(savedInstanceState);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN,
                WindowManager.LayoutParams.FLAG_FULLSCREEN);
        setContentView(R.layout.gesturepassword_unlock);
        mLockPatternView = (LockPatternView) this
                .findViewById(R.id.gesturepwd_unlock_lockview);
        mLockPatternView.setOnPatternListener(mChooseNewLockPatternListener);
        mLockPatternView.setTactileFeedbackEnabled(true);
        mHeadTextView = (TextView) findViewById(R.id.gesturepwd_unlock_text);
        mShakeAnim = AnimationUtils.loadAnimation(this, R.anim.shake_x);
        final Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(false);
        getSupportActionBar().setTitle(R.string.gesture_lock);
        settingsMgr = SettingsManager.instance();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (mCountdownTimer != null)
            mCountdownTimer.cancel();
    }

    @Override
    public void onBackPressed() {
        // stop default action (finishing the current activity) to be executed.
        // super.onBackPressed();
        Intent i = new Intent(this, BrowserActivity.class);
        i.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(i);
        finish();
    }

    private Runnable mClearPatternRunnable = new Runnable() {
        public void run() {
            mLockPatternView.clearPattern();
        }
    };

    protected LockPatternView.OnPatternListener mChooseNewLockPatternListener = new LockPatternView.OnPatternListener() {

        public void onPatternStart() {
            mLockPatternView.removeCallbacks(mClearPatternRunnable);
            patternInProgress();
        }

        public void onPatternCleared() {
            mLockPatternView.removeCallbacks(mClearPatternRunnable);
        }

        public void onPatternDetected(List<LockPatternView.Cell> pattern) {
            if (pattern == null)
                return;
            LockPatternUtils mLockPatternUtils = new LockPatternUtils(getApplicationContext());
            if (mLockPatternUtils.checkPattern(pattern)) {
                mLockPatternView
                        .setDisplayMode(LockPatternView.DisplayMode.Correct);
                settingsMgr.setupGestureLock();
                finish();
            } else {
                mLockPatternView
                        .setDisplayMode(LockPatternView.DisplayMode.Wrong);
                if (pattern.size() >= LockPatternUtils.MIN_PATTERN_REGISTER_FAIL) {
                    mFailedPatternAttemptsSinceLastTimeout++;
                    int retry = LockPatternUtils.FAILED_ATTEMPTS_BEFORE_TIMEOUT
                            - mFailedPatternAttemptsSinceLastTimeout;
                    if (retry >= 0) {
                        if (retry == 0)
                        showShortToast(UnlockGesturePasswordActivity.this, getResources().getString(R.string.lockscreen_access_pattern_failure));
                        mHeadTextView.setText(getResources().getQuantityString(R.plurals.lockscreen_access_pattern_failure_left_try_times, retry, retry));
                        mHeadTextView.setTextColor(Color.RED);
                        mHeadTextView.startAnimation(mShakeAnim);
                    }

                } else {
                    showShortToast(UnlockGesturePasswordActivity.this, getResources().getString(R.string.lockscreen_access_pattern_failure_not_long_enough));
                }

                if (mFailedPatternAttemptsSinceLastTimeout >= LockPatternUtils.FAILED_ATTEMPTS_BEFORE_TIMEOUT) {
                    mHandler.postDelayed(attemptLockout, 2000);
                } else {
                    mLockPatternView.postDelayed(mClearPatternRunnable, 2000);
                }
            }
        }

        public void onPatternCellAdded(List<Cell> pattern) {

        }

        private void patternInProgress() {
        }
    };
    Runnable attemptLockout = new Runnable() {

        @Override
        public void run() {
            mLockPatternView.clearPattern();
            mLockPatternView.setEnabled(false);
            mCountdownTimer = new CountDownTimer(
                    LockPatternUtils.FAILED_ATTEMPT_TIMEOUT_MS + 1, 1000) {

                @Override
                public void onTick(long millisUntilFinished) {
                    int secondsRemaining = (int) (millisUntilFinished / 1000) - 1;
                    if (secondsRemaining > 0) {
                        mHeadTextView.setText(getResources().getQuantityString(R.plurals.lockscreen_access_pattern_failure_left_try_seconds, secondsRemaining, secondsRemaining));
                    } else {
                        mHeadTextView.setText(R.string.lockscreen_access_pattern_hint);
                        mHeadTextView.setTextColor(Color.WHITE);
                    }

                }

                @Override
                public void onFinish() {
                    mLockPatternView.setEnabled(true);
                    mFailedPatternAttemptsSinceLastTimeout = 0;
                }
            }.start();
        }
    };

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        if (item.getItemId() == android.R.id.home) {
            finish();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return false;
    }
}
