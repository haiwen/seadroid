package com.seafile.seadroid2.ui.activity;

import android.content.*;
import android.net.Uri;
import android.os.IBinder;
import android.webkit.MimeTypeMap;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.util.ToastUtils;
import com.seafile.seadroid2.ui.dialog.OpenAsDialog;

import java.io.File;

/**
 * Base class for opening a file
 */
public abstract class OpenFileBaseActivity extends SherlockFragmentActivity {

    protected TransferService mTransferService;
    protected DataManager mDataManager;
    protected Account mAccount;

    protected ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            mTransferService = binder.getService();
            onTransferSericeConnected();
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {}
    };

    protected abstract void onTransferSericeConnected();

    public void showFile(File file) {
        String name = file.getName();
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();

        if (suffix.length() == 0) {
            ToastUtils.show(this, R.string.unknown_file_type);
            return;
        }

        if (suffix.endsWith("md") || suffix.endsWith("markdown")) {
            startMarkdownActivity(file.getPath());
            return;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        Intent open = new Intent(Intent.ACTION_VIEW);
        open.setDataAndType((Uri.fromFile(file)), mime);

        try {
            startActivity(open);
            return;
        } catch (ActivityNotFoundException e) {
            new OpenAsDialog(file).show(getSupportFragmentManager(), "OpenAsDialog");
            // activity_not_found
            return;
        }

    }

    protected void showRepo(String repoID, String repoName, String path, String dirID) {
        Intent intent = new Intent(this, BrowserActivity.class);
        intent.putExtra("repoID", repoID);
        intent.putExtra("repoName", repoName);
        intent.putExtra("path", path);
        intent.putExtra("dirID", dirID);
        startActivity(intent);
    }

    protected void startMarkdownActivity(String path) {
        Intent intent = new Intent(this, MarkdownActivity.class);
        intent.putExtra("path", path);
        startActivity(intent);
    }

    protected void bindTransferService() {
        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        if (mTransferService != null) {
            unbindService(mConnection);
            mTransferService = null;
        }
    }
}
