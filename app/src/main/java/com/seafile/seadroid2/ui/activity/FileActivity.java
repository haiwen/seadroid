package com.seafile.seadroid2.ui.activity;

import java.io.File;
import java.net.HttpURLConnection;

import android.content.ActivityNotFoundException;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;
import android.view.View;
import android.webkit.MimeTypeMap;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.R.id;
import com.seafile.seadroid2.R.layout;
import com.seafile.seadroid2.R.string;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.ui.dialog.OpenAsDialog;
import com.seafile.seadroid2.ui.dialog.PasswordDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;

/**
 * Display a file
 */
public class FileActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "FileActivity";

    private TextView mFileNameText;
    private ImageView mFileIcon;
    private Button mButtonCancel;

    private TextView mProgressText;
    private ProgressBar mProgressBar;

    private String mRepoName, mRepoID, mFilePath;
    private DataManager mDataManager;
    private Account mAccount;

    private int mTaskID = -1;
    private TransferService mTransferService;
    private TransferReceiver mTransferReceiver;
    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            Log.d(DEBUG_TAG, "TransferService connected");

            TransferBinder binder = (TransferBinder) service;
            mTransferService = binder.getService();
            onTransferSericeConnected();
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
        }
    };

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Intent intent = getIntent();

        mAccount  = (Account)intent.getParcelableExtra("account");
        mRepoName = intent.getStringExtra("repoName");
        mRepoID = intent.getStringExtra("repoID");
        mFilePath = intent.getStringExtra("filePath");
        mTaskID = intent.getIntExtra("taskID", 0);

        mDataManager = new DataManager(mAccount);

        setContentView(R.layout.file_activity);
        initWidgets();
        bindTransferService();
    }

    @Override
    protected void onStop() {
        Log.d(DEBUG_TAG, "onStop");
        super.onStop();

        if (mTransferReceiver != null) {
            LocalBroadcastManager.getInstance(this).unregisterReceiver(mTransferReceiver);
        }
    }

    @Override
    protected void onDestroy() {
        if (mTransferService != null) {
            unbindService(mConnection);
            mTransferService = null;
        }

        super.onDestroy();
    }

    private void initWidgets() {
        mFileNameText = (TextView)findViewById(R.id.file_name);
        mFileIcon = (ImageView)findViewById(R.id.file_icon);
        mButtonCancel = (Button)findViewById(R.id.op_cancel);
        mProgressBar = (ProgressBar)findViewById(R.id.progress_bar);
        mProgressText = (TextView)findViewById(R.id.progress_text);

        String fileName = Utils.fileNameFromPath(mFilePath);
        mFileNameText.setText(fileName);

        // icon
        mFileIcon.setImageResource(Utils.getFileIcon(fileName));

        mButtonCancel.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                if (mTaskID > 0) {
                    mTransferService.cancelDownloadTask(mTaskID);
                }
                finish();
            }
        });
    }

    private void startMarkdownActivity(String path) {
        Intent intent = new Intent(this, MarkdownActivity.class);
        intent.putExtra("path", path);
        startActivity(intent);
    }

    private void showFile() {
        File file = mDataManager.getLocalRepoFile(mRepoName, mRepoID, mFilePath);
        String name = file.getName();
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();

        if (suffix.length() == 0) {
            showToast(R.string.unknown_file_type);
            return;
        }

        // Open markdown files in MarkdownActivity
        if (suffix.endsWith("md") || suffix.endsWith("markdown")) {
            startMarkdownActivity(file.getPath());
            finish();
            overridePendingTransition(0, 0);
            return;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        Intent open = new Intent(Intent.ACTION_VIEW);
        open.setDataAndType((Uri.fromFile(file)), mime);

        try {
            startActivity(open);
            finish();
            return;
        } catch (ActivityNotFoundException e) {
            new OpenAsDialog(file) {
                @Override
                public void onDismiss(DialogInterface dialog) {
                    finish();
                }
            }.show(getSupportFragmentManager(), "OpenAsDialog");
            return;
        }

/*      String chooser_title = getString(R.string.open_with);
        Intent chooser = Intent.createChooser(open, chooser_title);

        if (open.resolveActivity(getPackageManager()) != null) {
            startActivity(chooser);
            finish();
            overridePendingTransition(0, 0);
            return;
        } else {
            showToast(R.string.activity_not_found);
            return;
        }*/

    }

    private void onTransferSericeConnected() {
        // Register broadcast receiver
        IntentFilter filter = new IntentFilter(TransferService.BROADCAST_ACTION);
        mTransferReceiver = new TransferReceiver();
        LocalBroadcastManager.getInstance(this).registerReceiver(mTransferReceiver, filter);

        mProgressBar.setVisibility(View.VISIBLE);
        mProgressBar.setIndeterminate(true);
        mProgressText.setVisibility(View.VISIBLE);
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
    }

    private void bindTransferService() {
        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");
    }

    private void onFileDownloadProgress(DownloadTaskInfo info) {
        long fileSize = info.fileSize;
        long finished = info.finished;

        mProgressBar.setIndeterminate(false);
        int percent;
        if (fileSize == 0) {
            percent = 100;
        } else {
            percent = (int)(finished * 100 / fileSize);
        }
        mProgressBar.setProgress(percent);

        String txt = Utils.readableFileSize(finished) + " / " + Utils.readableFileSize(fileSize);

        mProgressText.setText(txt);
    }

    private void onFileDownloaded(DownloadTaskInfo info) {
        mProgressBar.setVisibility(View.GONE);
        mProgressText.setVisibility(View.GONE);
        mButtonCancel.setVisibility(View.GONE);

        showFile();
    }

    private void onFileDownloadFailed(DownloadTaskInfo info) {
        mProgressBar.setVisibility(View.GONE);
        mProgressText.setVisibility(View.GONE);
        mButtonCancel.setVisibility(View.GONE);

        SeafException err = info.err;
        String fileName = Utils.fileNameFromPath(info.pathInRepo);
        if (err.getCode() == HttpURLConnection.HTTP_NOT_FOUND) {
            // file deleted
            showToast("The file \"" + fileName + "\" has been deleted");
        } else if (err.getCode() == SeafConnection.HTTP_STATUS_REPO_PASSWORD_REQUIRED) {
            handlePassword();
        } else {
            showToast("Failed to download file \"" + fileName);
        }
    }

    private void handlePassword() {
        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.setRepo(mRepoName, mRepoID, mAccount);
        passwordDialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                mTaskID = mTransferService.addDownloadTask(mAccount,
                                                           mRepoName,
                                                           mRepoID,
                                                           mFilePath);
            }

            @Override
            public void onTaskCancelled() {
                finish();
            }
        });
        passwordDialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    public void showToast(CharSequence msg) {
        Context context = getApplicationContext();
        Toast toast = Toast.makeText(context, msg, Toast.LENGTH_SHORT);
        toast.show();
    }

    public void showToast(int id) {
        showToast(getString(id));
    }

    private class TransferReceiver extends BroadcastReceiver {
        private TransferReceiver() {}

        public void onReceive(Context context, Intent intent) {
            if (mTaskID < 0) {
                return;
            }

            String type = intent.getStringExtra("type");
            if (type == null) {
                return;
            }

            int taskID = intent.getIntExtra("taskID", 0);
            if (taskID != mTaskID) {
                return;
            }

            DownloadTaskInfo info = mTransferService.getDownloadTaskInfo(taskID);
            if (info == null) {
                Log.w(DEBUG_TAG, "download info is null");
            }
            if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_PROGRESS)) {
                onFileDownloadProgress(info);
            } else if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_SUCCESS)) {
                onFileDownloaded(info);
            } else if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_FAILED)) {
                onFileDownloadFailed(info);
            }
        }

    } // TransferReceiver
}
