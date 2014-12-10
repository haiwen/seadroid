package com.seafile.seadroid2.ui.dialog;

import java.io.IOException;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.util.Utils;

class ClearCacheTask extends TaskDialog.Task {
    Account account;
    String path;
    SettingsManager settingsMgr;
    DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();

    public ClearCacheTask(Account account, String path, SettingsManager settingsManager) {
        this.account = account;
        this.path = path;
        this.settingsMgr = settingsManager;
    }

    @Override
    protected void runTask() {
        try {
            // clear cached files
            Utils.clearCache(path);

            // clear cached data from database
            settingsMgr.delCachesByActSignature(account);
        } catch (IOException e) {
            e.printStackTrace();
            // delete cache failed
            // TODO notify user
        }
    }
}

public class ClearCacheTaskDialog extends TaskDialog {
    private String path;
    private Account account;
    SettingsManager settingsMgr;

    public void init(Account account, String path) {
        this.account = account;
        this.path = path;
    }

    private SettingsManager getSettingsManager() {
        if (settingsMgr == null) {
            settingsMgr = SettingsManager.instance();
        }
        return settingsMgr;
    }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_delete_file, null);
        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        setTitle(getString(R.string.settings_clear_cache_title));
    }

    @Override
    protected ClearCacheTask prepareTask() {
        ClearCacheTask task = new ClearCacheTask(account, path, getSettingsManager());
        return task;
    }
}
