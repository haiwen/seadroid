package com.seafile.seadroid2.ui;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferManager.DownloadTaskInfo;
import com.seafile.seadroid2.Utils;

/**
 * Check and download the latest version of a file and open it
 */
public class FetchFileDialog extends DialogFragment {
    private String repoName;
    private String repoID;
    private String path;

    private ImageView fileIcon;
    private TextView fileNameText, fileSizeText;
    private ProgressBar progressBar;

    private int taskID = -1;
    private boolean cancelled = false;

    private FetchFileListener mListener;

    public interface FetchFileListener {
        void onDismiss();
        void onSuccess();
        void onFailure(SeafException e);
    }

    private BrowserActivity getBrowserActivity() {
        return (BrowserActivity)getActivity();
    }

    public void init(String repoName, String repoID, String path, FetchFileListener listener) {
        this.repoName = repoName;
        this.repoID = repoID;
        this.path = path;

        this.mListener = listener;
    }

    // Get the lastest version of the file
    private void startDownloadFile() {
        BrowserActivity mActivity = getBrowserActivity();

        taskID = mActivity.getTransferService().addDownloadTask(mActivity.getAccount(),
                                                                repoName, repoID, path);
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
        if (err.getCode() == 404) {
            getDialog().dismiss();
            getBrowserActivity().showToast("The file \"" + fileName + "\" has been deleted");
        } else if (err.getCode() == 440) {
            handlePassword();
        } else {
            getDialog().dismiss();
            getBrowserActivity().showToast("Failed to download file \"" + fileName);
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