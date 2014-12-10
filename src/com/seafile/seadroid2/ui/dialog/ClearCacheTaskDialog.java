package com.seafile.seadroid2.ui.dialog;

import java.io.IOException;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.util.Utils;

class ClearCacheTask extends TaskDialog.Task {
    String path;
    SettingsManager settingsMgr;

    public ClearCacheTask(String path, SettingsManager settingsManager) {
        this.path = path;
        this.settingsMgr = settingsManager;
    }

    @Override
    protected void runTask() {
        try {
            Utils.clearCache(path);
        } catch (IOException e) {
            e.printStackTrace();
            // delete cache failed
            // TODO notify user
        }
    }
}

public class ClearCacheTaskDialog extends TaskDialog {
    private String path;
    SettingsManager settingsMgr;

    public void init(String path) {
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
        ClearCacheTask task = new ClearCacheTask(path, getSettingsManager());
        return task;
    }
}
