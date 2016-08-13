package com.seafile.seadroid2.ui.dialog;

import java.net.HttpURLConnection;

import android.support.v7.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.os.Handler;
import android.support.v4.app.DialogFragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.util.Utils;

/**
 * Check and download the latest version of a file and open it
 */
public class FetchFileDialog extends DialogFragment {
    public static final String DEBUG_TAG = "FetchFileDialog";

    private String repoName;
    private String repoID;
    private String path;
    private long fileSize;

    private ImageView fileIcon;
    private TextView fileNameText, fileSizeText;
    private ProgressBar progressBar;

    private int taskID = -1;
    private boolean cancelled = false;

    private FetchFileListener mListener;

    private final Handler mTimer = new Handler();

    public interface FetchFileListener {
        void onDismiss();
        void onSuccess();
        void onFailure(SeafException e);
    }

    private BrowserActivity getBrowserActivity() {
        return (BrowserActivity)getActivity();
    }

    public void init(String repoName, String repoID, String path, long fileSize, FetchFileListener listener) {
        this.repoName = repoName;
        this.repoID = repoID;
        this.path = path;
        this.fileSize = fileSize;
        this.mListener = listener;
    }

    // Get the latest version of the file
    private void startDownloadFile() {
        BrowserActivity mActivity = getBrowserActivity();

        taskID = mActivity.getTransferService().addDownloadTask(mActivity.getAccount(),
                                                                repoName, repoID, path, fileSize);
    }

    @Override
    public void onStart() {
        super.onStart();
        startTimer();
    }

    @Override
    public void onStop() {
        stopTimer();
        super.onStop();
    }

    public void startTimer() {
        Log.d(DEBUG_TAG, "timer started");
        mTimer.postDelayed(new Runnable() {

            @Override
            public void run() {
                DownloadTaskInfo downloadTaskInfo = getBrowserActivity().getTransferService().getDownloadTaskInfo(taskID);
                handleDownloadTaskInfo(downloadTaskInfo);
                Log.d(DEBUG_TAG, "timer post refresh signal " + System.currentTimeMillis());
                mTimer.postDelayed(this, 1 * 1000);
            }
        }, 1 * 1000);
    }

    public void stopTimer() {
        Log.d(DEBUG_TAG, "timer stopped");
        mTimer.removeCallbacksAndMessages(null);
    }

    public int getTaskID() {
        return taskID;
    }

    public void handleDownloadTaskInfo(DownloadTaskInfo info) {
        if (cancelled) {
            return;
        }
        switch (info.state) {
        case INIT:
            break;
        case TRANSFERRING:
            updateProgress(info.fileSize, info.finished);
            break;
        case CANCELLED:
            break;
        case FAILED:
            onTaskFailed(info.err);
            break;
        case FINISHED:
            onTaskFinished();
            break;
        }
    }

    private TaskDialog.TaskDialogListener getPasswordDialogListener() {
        return new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                startDownloadFile();
            }

            @Override
            public void onTaskCancelled() {
                getDialog().dismiss();
            }
        };
    }

    private void handlePassword() {
        getBrowserActivity().showPasswordDialog(repoName, repoID,
                                                getPasswordDialogListener());
    }

    private void onTaskFailed(SeafException err) {
        String fileName = Utils.fileNameFromPath(path);
        if (err.getCode() == HttpURLConnection.HTTP_NOT_FOUND) {
            getDialog().dismiss();
            final String message = String.format(getActivity().getString(R.string.file_not_found), fileName);
            Toast.makeText(getBrowserActivity(), message, Toast.LENGTH_SHORT).show();
        } else if (err.getCode() == SeafConnection.HTTP_STATUS_REPO_PASSWORD_REQUIRED) {
            handlePassword();
        } else {
            getDialog().dismiss();
            final String message = String.format(getActivity().getString(R.string.op_exception_failed_to_download_file), fileName);
            Toast.makeText(getBrowserActivity(), message, Toast.LENGTH_SHORT).show();
            if (mListener != null) {
                mListener.onFailure(err);
            }
        }
    }

    protected void onTaskFinished() {
        getDialog().dismiss();
        if (mListener != null) {
            mListener.onSuccess();
        }
    }

    private void cancelTask() {
        if (taskID < 0) {
            return;
        }

        TransferService txService = getBrowserActivity().getTransferService();
        if (txService == null) {
            return;
        }

        txService.cancelDownloadTask(taskID);
    }

    private void updateProgress(long fileSize, long finished) {
        progressBar.setIndeterminate(false);
        int percent;
        if (fileSize == 0) {
            percent = 100;
        } else {
            percent = (int)(finished * 100 / fileSize);
        }
        progressBar.setProgress(percent);

        String txt = Utils.readableFileSize(finished) + " / " + Utils.readableFileSize(fileSize);

        fileSizeText.setText(txt);
    }

    /**
     * Handle screen rotation
     */
    @Override
    public void onSaveInstanceState(Bundle outState) {
        outState.putString("repoName", repoName);
        outState.putString("repoID", repoID);
        outState.putString("path", path);
        outState.putInt("taskID", taskID);
        outState.putInt("progress", progressBar.getProgress());

        super.onSaveInstanceState(outState);
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        View view = inflater.inflate(R.layout.dialog_open_file, null);

        fileIcon = (ImageView)view.findViewById(R.id.file_icon);
        fileNameText = (TextView)view.findViewById(R.id.file_name);
        fileSizeText = (TextView)view.findViewById(R.id.file_size);
        progressBar = (ProgressBar)view.findViewById(R.id.progress_bar);

        if (savedInstanceState != null) {
            repoName = savedInstanceState.getString("repoName");
            repoID = savedInstanceState.getString("repoID");
            path = savedInstanceState.getString("path");
            taskID = savedInstanceState.getInt("taskID");
            int progress = savedInstanceState.getInt("progress");
            if (progress > 0) {
                progressBar.setIndeterminate(false);
                progressBar.setProgress(progress);
            }
        }

        String fileName = Utils.fileNameFromPath(path);
        fileIcon.setImageResource(Utils.getFileIcon(fileName));
        fileNameText.setText(fileName);

        builder.setView(view);
        builder.setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                cancelled = true;
                cancelTask();
            }
        });

        Dialog dialog = builder.create();

        if (taskID == -1) {
            startDownloadFile();
        }

        dialog.setOnDismissListener(new DialogInterface.OnDismissListener() {
            @Override
            public void onDismiss(DialogInterface d) {
                if (mListener != null) {
                    mListener.onDismiss();
                }
            }
        });

        return dialog;
    }
}