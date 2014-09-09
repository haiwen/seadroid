package com.seafile.seadroid2;

import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.database.Cursor;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.provider.MediaStore.Images;
import android.util.Log;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.ui.SeafilePathChooserActivity;
import com.seafile.seadroid2.util.Utils;

public class ShareToSeafileActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "ShareToSeafileActivity";

    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "password_dialog_fragment_tag";
    private static final int CHOOSE_COPY_MOVE_DEST_REQUEST = 1;

    private TransferService mTxService;
    private ServiceConnection mConnection;
    private String localPath;
    private Intent dstData;
    private Boolean isFinishActivity = false;
	protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Intent intent = getIntent();

        Bundle extras = intent.getExtras();
        if (extras != null) {
            Uri uri = (Uri)extras.get(Intent.EXTRA_STREAM);
            localPath = getSharedFilePath(uri);
        }
        if (localPath == null) {
            showToast(R.string.not_supported_share);
            finish();
            return;
        }

        Log.d(DEBUG_TAG, "share " + localPath);
        Intent chooserIntent = new Intent(this, SeafilePathChooserActivity.class);
        startActivityForResult(chooserIntent, CHOOSE_COPY_MOVE_DEST_REQUEST);
    }

    private String getSharedFilePath(Uri uri) {
        if (uri == null) {
            return null;
        }

        if (uri.getScheme().equals("file")) {
            return uri.getPath();
        } else {
            ContentResolver contentResolver = getContentResolver();
            Cursor cursor = contentResolver.query(uri, null, null, null, null);
            if (cursor == null || !cursor.moveToFirst()) {
                return null;
            }
            String filePath = cursor.getString(cursor.getColumnIndex(Images.Media.DATA));
            return filePath;
        }
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");
        if (mTxService != null) {
            unbindService(mConnection);
            mTxService = null;
        }

        super.onDestroy();
    }

    private void addUploadTask(Account account, String repoName, String repoID, String targetDir, String localFilePath) {
        bindTransferService(account, repoName, repoID, targetDir, localFilePath);
    }

    private void bindTransferService(final Account account, final String repoName, final String repoID,
                                        final String targetDir, final String localPath) {
        // start transfer service
        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);

        mConnection = new ServiceConnection() {
            @Override
            public void onServiceConnected(ComponentName className, IBinder service) {
                TransferBinder binder = (TransferBinder) service;
                mTxService = binder.getService();
                mTxService.addUploadTask(account, repoID, repoName, targetDir,
                                         localPath, false);
                showToast(R.string.upload_started);
                finish();
            }

            @Override
            public void onServiceDisconnected(ComponentName arg0) {
                mTxService = null;
            }
        };
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode != CHOOSE_COPY_MOVE_DEST_REQUEST) {
            return;
        }
        if (resultCode == RESULT_OK) {
            if (!Utils.isNetworkOn()) {
                showToast(R.string.network_down);
                return;
            }
            dstData = data;
            Log.i(DEBUG_TAG, "CHOOSE_COPY_MOVE_DEST_REQUEST returns");
        }
        isFinishActivity =true;
    }

    @Override
    protected void onPostResume() {
        super.onPostResume();
        if (dstData != null) {

            String dstRepoId, dstRepoName, dstDir;
            Account account;
            dstRepoName = dstData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_NAME);
            dstRepoId = dstData.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
            dstDir = dstData.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
            account = (Account)dstData.getParcelableExtra(SeafilePathChooserActivity.DATA_ACCOUNT);
        	addUploadTask(account, dstRepoName, dstRepoId, dstDir, localPath);
        	Log.d(DEBUG_TAG, "dstRepoName: " + dstRepoName);
        	Log.d(DEBUG_TAG, "dstDir: " + dstDir);
		}
        
        if(isFinishActivity) {
		    Log.d(DEBUG_TAG, "finish!");
		    finish();
        }
    }

    public void showToast(CharSequence msg) {
        Context context = getApplicationContext();
        Toast toast = Toast.makeText(context, msg, Toast.LENGTH_SHORT);
        toast.show();
    }

    public void showToast(int id) {
        showToast(getString(id));
    }
}
