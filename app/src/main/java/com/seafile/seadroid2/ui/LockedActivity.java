package com.seafile.seadroid2.ui;

import android.content.Intent;
import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.appcompat.app.AppCompatActivity;
import androidx.biometric.BiometricManager;
import androidx.biometric.BiometricPrompt;
import androidx.core.content.ContextCompat;
import androidx.core.splashscreen.SplashScreen;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;

import java.util.concurrent.Executor;

public class LockedActivity extends AppCompatActivity {
    public static int BIOMETRIC_REQS = BiometricManager.Authenticators.BIOMETRIC_STRONG
            | BiometricManager.Authenticators.DEVICE_CREDENTIAL;


    @Override
    protected void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Show a "fake" splash screen for better UX.
        SplashScreen splashScreen = SplashScreen.installSplashScreen(this);
        splashScreen.setKeepOnScreenCondition(() -> true);

        biometricPrompt();
    }

    private void biometricPrompt() {
        Executor executor = ContextCompat.getMainExecutor(this);
        BiometricPrompt biometricPrompt = new BiometricPrompt(this, executor,
                new BiometricPrompt.AuthenticationCallback() {
                    @Override
                    public void onAuthenticationSucceeded(@NonNull BiometricPrompt.AuthenticationResult result) {
                        super.onAuthenticationSucceeded(result);

                        unlock();
                    }

                    @Override
                    public void onAuthenticationError(int errorCode, @NonNull CharSequence errString) {
                        super.onAuthenticationError(errorCode, errString);

                        // The user exited, or something's wrong with the system.
                        // The only thing we can do at this point is exit.
                        finish();
                    }
                });

        BiometricPrompt.PromptInfo promptInfo = new BiometricPrompt.PromptInfo.Builder()
                .setTitle(getString(R.string.biometric_prompt_title))
                .setAllowedAuthenticators(BIOMETRIC_REQS)
                .build();

        biometricPrompt.authenticate(promptInfo);
    }

    private void unlock() {
        SeadroidApplication.unlock();

        // Use the supplied next activity, if applicable.
        Intent targetIntent = getIntent().getParcelableExtra("TARGET_INTENT");
        if (targetIntent == null) {
            targetIntent = new Intent(LockedActivity.this, SplashActivity.class);
        }

        // Prevent back button / returning some other way.
        targetIntent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NEW_TASK);

        startActivity(targetIntent);
        finish();
    }
}
