package com.seafile.seadroid2.framework.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import static org.robolectric.Shadows.shadowOf;

import android.app.ActivityManager;
import android.app.Application;
import android.content.Context;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.Process;

import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.RuntimeEnvironment;
import org.robolectric.annotation.Config;
import org.robolectric.shadows.ShadowActivityManager;
import org.robolectric.shadows.ShadowConnectivityManager;
import org.robolectric.shadows.ShadowNetworkInfo;

import java.util.Collections;

@RunWith(RobolectricTestRunner.class)
@Config(sdk = 28, manifest = Config.NONE, application = Application.class)
public class BackgroundExecutionPolicyTest {
    private Context context;
    private ShadowActivityManager activities;
    private ShadowConnectivityManager network;

    @Before
    public void setUp() {
        context = RuntimeEnvironment.getApplication();
        activities = shadowOf(context.getSystemService(ActivityManager.class));
        network = shadowOf(context.getSystemService(ConnectivityManager.class));
        setImportance(ActivityManager.RunningAppProcessInfo.IMPORTANCE_SERVICE);
        activities.setBackgroundRestricted(false);
        network.setRestrictBackgroundStatus(ConnectivityManager.RESTRICT_BACKGROUND_STATUS_DISABLED);
        setNetwork(ConnectivityManager.TYPE_MOBILE);
    }

    @Test
    public void restrictedBackgroundProcessDefersBackup() {
        activities.setBackgroundRestricted(true);
        assertTrue(BackgroundExecutionPolicy.shouldDefer(context));
    }

    @Test
    public void dataSaverOnMeteredNetworkDefersBackup() {
        network.setRestrictBackgroundStatus(ConnectivityManager.RESTRICT_BACKGROUND_STATUS_ENABLED);
        assertTrue(BackgroundExecutionPolicy.shouldDefer(context));
    }

    @Test
    public void dataSaverDoesNotBlockUnmeteredNetwork() {
        network.setRestrictBackgroundStatus(ConnectivityManager.RESTRICT_BACKGROUND_STATUS_ENABLED);
        setNetwork(ConnectivityManager.TYPE_WIFI);
        assertFalse(BackgroundExecutionPolicy.shouldDefer(context));
    }

    @Test
    public void dataSaverExemptionAllowsBackup() {
        network.setRestrictBackgroundStatus(ConnectivityManager.RESTRICT_BACKGROUND_STATUS_WHITELISTED);
        assertFalse(BackgroundExecutionPolicy.shouldDefer(context));
    }

    @Test
    public void returningToForegroundAllowsBackupAgain() {
        activities.setBackgroundRestricted(true);
        network.setRestrictBackgroundStatus(ConnectivityManager.RESTRICT_BACKGROUND_STATUS_ENABLED);
        assertTrue(BackgroundExecutionPolicy.shouldDefer(context));
        setImportance(ActivityManager.RunningAppProcessInfo.IMPORTANCE_FOREGROUND);
        assertFalse(BackgroundExecutionPolicy.shouldDefer(context));
    }

    private void setImportance(int importance) {
        ActivityManager.RunningAppProcessInfo process = new ActivityManager.RunningAppProcessInfo();
        process.pid = Process.myPid();
        process.importance = importance;
        activities.setProcesses(Collections.singletonList(process));
    }

    private void setNetwork(int type) {
        network.setActiveNetworkInfo(ShadowNetworkInfo.newInstance(
                NetworkInfo.DetailedState.CONNECTED, type, 0, true, true));
    }
}
