package com.seafile.seadroid2.ui.dialog;

import java.io.IOException;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

class ClearCacheTask extends TaskDialog.Task {
    private Account account;
    private String filesDir;
    private String cacheDir;
    private String tempDir;
    private String thumbDir;
    SettingsManager settingsMgr;

    public ClearCacheTask(Account account, String filesDir, String cacheDir, String tempDir, String thumbDir, SettingsManager settingsManager) {
        this.account = account;
        this.filesDir = filesDir;
        this.cacheDir = cacheDir;
        this.tempDir = tempDir;
        this.thumbDir = thumbDir;
        this.settingsMgr = settingsManager;
    }

    @Override
    protected void runTask() {
        try {
            // clear cached files
            Utils.clearCache(filesDir);

            // clear cached repo data
            Utils.clearCache(cacheDir);

            // clear temp files
            Utils.clearCache(tempDir);

            // clear thumb files
            Utils.clearCache(thumbDir);

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
    private String thumbDir;
    private Account account;
    SettingsManager settingsMgr;

    public void init(Account account, String filesDir, String cacheDir, String tempDir, String thumbDir) {
        this.account = account;
        this.filesDir = filesDir;
        this.cacheDir = cacheDir;
        this.tempDir = tempDir;
        this.thumbDir = thumbDir;
    }

    private SettingsManager getSettingsManager() {
        if (settingsMgr == null) {
            settingsMgr = SettingsManager.instance();
        }
        return settingsMgr;
    }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_delete_cache, null);
        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        setTitle(getString(R.string.settings_clear_cache_title));
    }

    @Override
    protected ClearCacheTask prepareTask() {
        ClearCacheTask task = new ClearCacheTask(account, filesDir, cacheDir, tempDir, thumbDir, getSettingsManager());
        return task;
    }
}
