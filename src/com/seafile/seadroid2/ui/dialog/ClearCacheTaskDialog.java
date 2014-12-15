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
    private String cacheDir;
    private String tempDir;
    SettingsManager settingsMgr;
    DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();

    public ClearCacheTask(Account account, String filesDir, String cacheDir, String tempDir, SettingsManager settingsManager) {
        this.account = account;
        this.path = filesDir;
        this.cacheDir = cacheDir;
        this.tempDir = tempDir;
        this.settingsMgr = settingsManager;
    }

    @Override
    protected void runTask() {
        try {
            // clear cached files
            Utils.clearCache(path);

            // clear cached repo data
            Utils.clearCache(cacheDir);

            // clear temp files
            Utils.clearCache(tempDir);

            // clear cached data from database
            settingsMgr.delCachesByActSignature(account);

            // clear WebView data

            // clear editor cache

        } catch (IOException e) {
            e.printStackTrace();
            // delete cache failed
            // TODO notify user
        }
    }
}

public class ClearCacheTaskDialog extends TaskDialog {
    private String filesDir;
    private String cacheDir;
    private String tempDir;
    private Account account;
    SettingsManager settingsMgr;

    public void init(Account account, String filesDir, String cacheDir, String tempDir) {
        this.account = account;
        this.filesDir = filesDir;
        this.cacheDir = cacheDir;
        this.tempDir = tempDir;
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
        ClearCacheTask task = new ClearCacheTask(account, filesDir, cacheDir, tempDir, getSettingsManager());
        return task;
    }
}
