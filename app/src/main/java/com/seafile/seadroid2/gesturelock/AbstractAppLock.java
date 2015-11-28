/*
 * App passcode library for Android, master branch
 * Dual licensed under MIT, and GPL.
 * See https://github.com/wordpress-mobile/Android-PasscodeLock
 */
package com.seafile.seadroid2.gesturelock;

import android.app.Application;

/**
 * Abstract AppLock
 */
public abstract class AbstractAppLock implements Application.ActivityLifecycleCallbacks {

    public abstract void enable();
    public abstract void disable();
}
