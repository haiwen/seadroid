package com.seafile.seadroid2.ui;

import java.io.File;

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
import com.seafile.seadroid2.TransferManager.DownloadTaskInfo;
import com.seafile.seadroid2.Utils;

/**
 * Check and download the latest version of a file and open it
 */
public class OpenFileDialog extends DialogFragment {
    private String repoName;
    private String repoID;
    private String path;

    private ImageView fileIcon;
    private TextView fileNameText;
    private ProgressBar progressBar;

    private int taskID;

    private OnDismissListener dismissListener;

    public interface OnDismissListener {
        void onDismiss();
    }

    private BrowserActivity getBrowserActivity() {
        return (BrowserActivity)getActivity();
    }

    public void init(String repoName, String repoID, String path, OnDismissListener listener) {
        this.repoName = repoName;
        this.repoID = repoID;
        this.path = path;

        this.dismissListener = listener;
    }

    // Get the lastest version of the file
    private void loadFile() {
        BrowserActivity mActivity = getBrowserActivity();

        taskID = mActivity.getTransferService().addDownloadTask(mActivity.getAccount(),
                                                                repoName, repoID, path);
    }

    public int getTaskID() {
        return taskID;
    }

    public void handleDownloadTaskInfo(DownloadTaskInfo info) {
        switch (info.state) {
        case INIT:
            break;
        case TRANSFERRING:
            updateProgress(info.fileSize, info.finished);
            break;
        case CANCELLED:
            break;
        case FAILED:
            break;
        case FINISHED:
            onTaskFinished();
            break;
        }
    }

    private void onTaskFinished() {
        getDialog().dismiss();

        File localFile = getBrowserActivity().getDataManager().getLocalRepoFile(repoName, repoID, path);

        getBrowserActivity().showFile(localFile);
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
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        View view = inflater.inflate(R.layout.dialog_open_file, null);

        fileIcon = (ImageView)view.findViewById(R.id.file_icon);
        fileNameText = (TextView)view.findViewById(R.id.file_name);
        progressBar = (ProgressBar)view.findViewById(R.id.progress_bar);

        String fileName = Utils.fileNameFromPath(path);
        fileIcon.setImageResource(Utils.getFileIcon(fileName));
        fileNameText.setText(fileName);

        builder.setView(view);
        builder.setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                // TODO: cancel the download task
            }
        });

        Dialog dialog = builder.create();

        dialog.setOnShowListener(new DialogInterface.OnShowListener() {
            @Override
            public void onShow(DialogInterface d) {
                loadFile();
            }
        });

        dialog.setOnDismissListener(new DialogInterface.OnDismissListener() {
            @Override
            public void onDismiss(DialogInterface d) {
                if (dismissListener != null) {
                    dismissListener.onDismiss();
                }
            }
        });

        return dialog;
    }
}