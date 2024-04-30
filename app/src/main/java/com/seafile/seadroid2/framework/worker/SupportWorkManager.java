package com.seafile.seadroid2.framework.worker;

import androidx.work.WorkManager;

import com.seafile.seadroid2.SeadroidApplication;

public class SupportWorkManager {

    public static WorkManager getWorkManager() {
        return WorkManager.getInstance(SeadroidApplication.getAppContext());
    }
}
