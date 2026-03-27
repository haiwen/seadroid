package com.seafile.seadroid2.framework.util;

import androidx.biometric.BiometricManager;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.preferences.Settings;

/**
 * Manages the app-wide lock state for biometric/device credential authentication.
 *
 * <p>Lock behavior:
 * <ul>
 *   <li>When enabled in settings, the app locks on cold start and when
 *       backgrounded beyond the configured timeout.</li>
 *   <li>Authentication is delegated to the system via {@code BiometricPrompt}
 *       with {@code DEVICE_CREDENTIAL} fallback, so fingerprint, face, PIN, and
 *       pattern are all supported with zero custom UI.</li>
 *   <li>Background services (backup, sync) are not blocked — only UI access
 *       requires authentication.</li>
 * </ul>
 *
 * <p>Inspired by <a href="https://github.com/haiwen/seadroid/pull/1113">PR #1113</a>
 * by uellenberg.
 */
public final class AppLockManager {

    /** Authenticator types accepted by the lock screen. */
    public static final int ALLOWED_AUTHENTICATORS =
            BiometricManager.Authenticators.BIOMETRIC_STRONG
            | BiometricManager.Authenticators.BIOMETRIC_WEAK
            | BiometricManager.Authenticators.DEVICE_CREDENTIAL;

    private static volatile boolean sLocked = false;
    private static volatile long sLastBackgroundTimestamp = 0;

    private AppLockManager() {}

    /** Returns true if the UI should be gated behind the lock screen. */
    public static boolean isLocked() {
        return sLocked;
    }

    /** Called when the user successfully authenticates. */
    public static void unlock() {
        sLocked = false;
        sLastBackgroundTimestamp = 0;
    }

    /**
     * Lock the app if biometric lock is enabled and the device supports it.
     * Called on cold start from {@code SeadroidApplication.onCreate()}.
     */
    public static void lockIfEnabled() {
        if (isLockFeatureEnabled() && canAuthenticate()) {
            sLocked = true;
        }
    }

    /**
     * Record the moment the app goes to background.
     * Called from {@link com.seafile.seadroid2.AppProcessLifeObserver#onStop}.
     */
    public static void onAppBackgrounded() {
        if (isLockFeatureEnabled()) {
            sLastBackgroundTimestamp = System.currentTimeMillis();
        }
    }

    /**
     * Check whether the lock timeout has elapsed since the app was backgrounded.
     * If so, re-lock. Called from {@link com.seafile.seadroid2.AppProcessLifeObserver#onStart}.
     */
    public static void onAppForegrounded() {
        if (!isLockFeatureEnabled() || !canAuthenticate()) {
            return;
        }
        if (sLastBackgroundTimestamp == 0) {
            // First foreground after cold start — already handled by lockIfEnabled()
            return;
        }
        long elapsed = System.currentTimeMillis() - sLastBackgroundTimestamp;
        long timeoutMs = getLockTimeoutMs();
        if (timeoutMs == 0 || elapsed >= timeoutMs) {
            sLocked = true;
        }
    }

    /** Returns true if the user has enabled biometric lock in settings. */
    public static boolean isLockFeatureEnabled() {
        try {
            return Settings.BIOMETRIC_LOCK_SWITCH.queryValue();
        } catch (Exception e) {
            return false;
        }
    }

    /** Returns true if the device supports at least one accepted authenticator. */
    public static boolean canAuthenticate() {
        int result = BiometricManager.from(SeadroidApplication.getAppContext())
                .canAuthenticate(ALLOWED_AUTHENTICATORS);
        return result == BiometricManager.BIOMETRIC_SUCCESS;
    }

    /**
     * Returns the configured lock timeout in milliseconds.
     * 0 = lock immediately when backgrounded.
     */
    private static long getLockTimeoutMs() {
        try {
            String value = Settings.LOCK_TIMEOUT.queryValue();
            return Long.parseLong(value);
        } catch (Exception e) {
            return 0;
        }
    }
}
