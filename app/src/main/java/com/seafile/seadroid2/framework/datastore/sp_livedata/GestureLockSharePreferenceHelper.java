package com.seafile.seadroid2.framework.datastore.sp_livedata;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.gesturelock.LockPatternUtils;
import com.seafile.seadroid2.preferences.Settings;

public class GestureLockSharePreferenceHelper {

    public static final long LOCK_EXPIRATION_MSECS = 5 * 60 * 1000;

    public static void disable() {

        //
        Settings.SETTINGS_GESTURE.putValue(false);

        //
        GestureLockSharePreferenceHelper.updateLockTimeStamp(0L);
    }

    /**
     * For convenience, if the user has given the correct gesture lock, he
     * would not be asked for gesture lock for a short period of time.
     */
    public static boolean isLockRequired() {

        Boolean isEnable = Settings.SETTINGS_GESTURE.queryValue();
        if (!isEnable) {
            return false;
        }

        LockPatternUtils mLockPatternUtils = new LockPatternUtils(SeadroidApplication.getAppContext());
        if (!mLockPatternUtils.savedPatternExists()) {
            return false;
        }

        long lock_timestamp = Settings.SETTINGS_GESTURE_LOCK_TIMESTAMP.queryValue();
        if (lock_timestamp == 0) {
            return false;
        }

        long now = System.currentTimeMillis();
        if (now < lock_timestamp + LOCK_EXPIRATION_MSECS) {
            return false;
        }

        return true;
    }

    public static void updateLockTimeStamp() {
        long now = System.currentTimeMillis();
        updateLockTimeStamp(now);
    }

    public static void updateLockTimeStamp(long time) {

        Settings.SETTINGS_GESTURE_LOCK_TIMESTAMP.putValue(time);
    }
}
