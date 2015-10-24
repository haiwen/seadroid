package com.seafile.seadroid2.ui.activity;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.Handler;
import android.os.IBinder;
import android.util.Log;
import android.view.View;
import android.widget.*;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.notification.DownloadNotificationProvider;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.TaskState;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.ui.dialog.PasswordDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.net.HttpURLConnection;

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
    private boolean timerStarted;
    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            Log.d(DEBUG_TAG, "TransferService connected");

            TransferBinder binder = (TransferBinder) service;
            mTransferService = binder.getService();
            onTransferServiceConnected();
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
        }
    };

    private final Handler mTimer = new Handler();

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Intent intent = getIntent();

        mAccount  = intent.getParcelableExtra("account");
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
    protected void onStart() {
        Log.d(DEBUG_TAG, "onStart");
        super.onStart();
        if (mTransferService != null) {
            startTimer();
        }
    }

    @Override
    protected void onStop() {
        Log.d(DEBUG_TAG, "onStop");
        super.onStop();
        stopTimer();
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
        super.onDestroy();
        if (mTransferService != null) {
            unbindService(mConnection);
            mTransferService = null;
        }

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
                    mTransferService.cancelNotification();
                }
                finish();
            }
        });
    }

    private void onTransferServiceConnected() {
        mProgressBar.setVisibility(View.VISIBLE);
        mProgressBar.setIndeterminate(true);
        mProgressText.setVisibility(View.VISIBLE);

        startTimer();
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        Log.d(DEBUG_TAG, "onSaveInstanceState");
        //super.onSaveInstanceState(outState);
    }

    private void bindTransferService() {
        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        // Intent bIntent = new Intent(this, TransferService.class);
        bindService(txIntent, mConnection, Context.BIND_AUTO_CREATE);
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

        if (!mTransferService.hasDownloadNotifProvider()) {
            DownloadNotificationProvider provider = new DownloadNotificationProvider(
                    mTransferService.getDownloadTaskManager(),
                    mTransferService);
            mTransferService.saveDownloadNotifProvider(provider);
        } else {
            // if the notificationManager mapping the repoID exist, update its data set
            // mTransferService.getDownloadNotifProvider().updateTotalSize(fileSize);
        }
    }

    private void onFileDownloaded() {
        mProgressBar.setVisibility(View.GONE);
        mProgressText.setVisibility(View.GONE);
        mButtonCancel.setVisibility(View.GONE);

        File file = mDataManager.getLocalRepoFile(mRepoName, mRepoID, mFilePath);
        if (file != null && timerStarted) {
            Intent result = new Intent();
            result.putExtra("path", file.getAbsolutePath());
            setResult(RESULT_OK, result);
        }
        else {
            setResult(RESULT_CANCELED);
        }
        stopTimer();
        finish();
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
        stopTimer();
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


    public void startTimer() {
        if (timerStarted) {
            return;
        }
        timerStarted = true;
        Log.d(DEBUG_TAG, "timer started");
        mTimer.postDelayed(new Runnable() {

            @Override
            public void run() {
                if (mTransferService == null)
                    return;

                DownloadTaskInfo downloadTaskInfo = mTransferService.getDownloadTaskInfo(mTaskID);
                if (downloadTaskInfo.state == TaskState.INIT
                        || downloadTaskInfo.state == TaskState.TRANSFERRING)
                    onFileDownloadProgress(downloadTaskInfo);
                else if (downloadTaskInfo.state == TaskState.FAILED)
                    onFileDownloadFailed(downloadTaskInfo);
                else if (downloadTaskInfo.state == TaskState.FINISHED)
                    onFileDownloaded();
                else if (downloadTaskInfo.state == TaskState.CANCELLED)
                    // do nothing when cancelled

                Log.d(DEBUG_TAG, "timer post refresh signal " + System.currentTimeMillis());
                mTimer.postDelayed(this, 1 * 1000);
            }
        }, 1 * 1000);
    }

    public void stopTimer() {
        if (!timerStarted) {
            return;
        }
        timerStarted = false;
        Log.d(DEBUG_TAG, "timer stopped");
        mTimer.removeCallbacksAndMessages(null);
    }
}
