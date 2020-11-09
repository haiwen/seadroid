package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.bumptech.glide.Glide;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.data.StorageManager;

class ClearCacheTask extends TaskDialog.Task {

    @Override
    protected void runTask() {
        StorageManager storageManager = StorageManager.getInstance();
        storageManager.clearCache();

        // clear cached data from database
        DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
        dbHelper.delCaches();

        //clear Glide cache
        Glide.get(SeadroidApplication.getAppContext()).clearDiskCache();
    }
}

public class ClearCacheTaskDialog extends TaskDialog {
    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_delete_cache, null);
        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        dialog.setTitle(getString(R.string.settings_clear_cache_title));
    }

    @Override
    protected ClearCacheTask prepareTask() {
        ClearCacheTask task = new ClearCacheTask();
        return task;
    }
}
