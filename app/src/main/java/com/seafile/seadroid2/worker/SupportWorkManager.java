package com.seafile.seadroid2.worker;

import androidx.work.WorkManager;

import com.seafile.seadroid2.SeadroidApplication;

public class SupportWorkManager {
    public static void initWorkManager() {
//        Configuration configuration = new Configuration.Builder()
//                .setMaxSchedulerLimit(20)
//                .build();
//        WorkManager.initialize(SeadroidApplication.getAppContext(), configuration);
    }

    public static WorkManager getWorkManager() {
        return WorkManager.getInstance(SeadroidApplication.getAppContext());
    }
}
