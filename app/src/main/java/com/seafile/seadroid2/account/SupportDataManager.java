package com.seafile.seadroid2.account;

import com.seafile.seadroid2.data.DataManager;

public class SupportDataManager {
    private DataManager dataManager;

    private static volatile SupportDataManager singleton = null;

    public static SupportDataManager getInstance() {
        if (singleton == null) {
            synchronized (SupportDataManager.class) {
                if (singleton == null) {
                    singleton = new SupportDataManager();
                }
            }
        }
        return singleton;
    }

    public SupportDataManager() {
        dataManager = new DataManager(SupportAccountManager.getInstance().getCurrentAccount());
    }

    public DataManager getDataManager() {
        return dataManager;
    }
}
