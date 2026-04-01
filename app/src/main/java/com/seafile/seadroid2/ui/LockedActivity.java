package com.seafile.seadroid2.ui;

import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.biometric.BiometricPrompt;
import androidx.core.content.ContextCompat;
import androidx.core.splashscreen.SplashScreen;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.AppLockManager;

/**
 * Full-screen lock gate that shows the system biometric/credential prompt.
 * Displays a splash screen behind the prompt so the user doesn't see app content.
 *
 * <p>Inspired by <a href="https://github.com/haiwen/seadroid/pull/1113">PR #1113</a>
 * by uellenberg. Improvements over that PR:
 * <ul>
 *   <li>Supports lock timeout (re-lock after configurable background period)</li>
 *   <li>Does not block background service registration in MainActivity</li>
 *   <li>Uses stable biometric library dependency</li>
 * </ul>
 */
public class LockedActivity extends AppCompatActivity {

    public static final String EXTRA_TARGET_INTENT = "target_intent";

    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Show a splash screen as backdrop so the user never sees app content
        SplashScreen splashScreen = SplashScreen.installSplashScreen(this);
        splashScreen.setKeepOnScreenCondition(() -> true);

        showBiometricPrompt();
    }

    private void showBiometricPrompt() {
        BiometricPrompt biometricPrompt = new BiometricPrompt(this,
                ContextCompat.getMainExecutor(this),
                new BiometricPrompt.AuthenticationCallback() {
                    @Override
                    public void onAuthenticationSucceeded(
                            @NonNull BiometricPrompt.AuthenticationResult result) {
                        super.onAuthenticationSucceeded(result);
                        onUnlocked();
                    }

                    @Override
                    public void onAuthenticationError(
                            int errorCode, @NonNull CharSequence errString) {
                        super.onAuthenticationError(errorCode, errString);
                        // User cancelled or system error — exit the app
                        finishAffinity();
                    }
                });

        BiometricPrompt.PromptInfo promptInfo = new BiometricPrompt.PromptInfo.Builder()
                .setTitle(getString(R.string.biometric_prompt_title))
                .setSubtitle(getString(R.string.biometric_prompt_subtitle))
                .setAllowedAuthenticators(AppLockManager.ALLOWED_AUTHENTICATORS)
                .build();

        biometricPrompt.authenticate(promptInfo);
    }

    private void onUnlocked() {
        AppLockManager.unlock();

        // Navigate to the activity that was originally being opened
        Intent targetIntent = getIntent().getParcelableExtra(EXTRA_TARGET_INTENT);
        if (targetIntent == null) {
            targetIntent = new Intent(this, SplashActivity.class);
        }
        targetIntent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
        startActivity(targetIntent);
        finish();
    }

    @Override
    public void onBackPressed() {
        // Don't allow back to bypass the lock
        finishAffinity();
    }
}
