package com.seafile.seadroid2.ui;

import java.io.File;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.DialogInterface;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.DialogFragment;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DataManager.ProgressMonitor;

/**
 * Check and download the latest version of a file and open it
 */
public class OpenFileDialog extends DialogFragment {
    private String repoName;
    private String repoID;
    private String dirPath;
    private String fileName;

    private ImageView fileIcon;
    private TextView fileNameText;
    private ProgressBar progessBar;

    private long fileSize;
    private long finished;
    private LoadFileTask fileTask;

    private BrowserActivity getBrowserActivity() {
        return (BrowserActivity)getActivity();
    }

    public void setFileInfo(String repoName, String repoID, String dirPath, String fileName) {
        this.repoName = repoName;
        this.repoID = repoID;
        this.dirPath = dirPath;
        this.fileName = fileName;

        // The size of the file would be known in the first progress update
        this.fileSize = -1;
    }

    // Get the lastest version of the file
    private void loadFile() {
        String fullPath = Utils.pathJoin(dirPath, fileName);
        fileTask = new LoadFileTask(repoName, repoID, fullPath);
        ConcurrentAsyncTask.execute(fileTask);
    }

    public void onProgressUpdate(long value) {
        if (fileSize == -1) {
            fileSize = value;
            progessBar.setProgress(0);
        } else {
            finished = value;
            int percent;
            if (fileSize == 0) {
                percent = 100;
            } else {
                percent = (int)(finished * 100 / fileSize);
            }
            progessBar.setProgress(percent);
        }
    }

    @Override
    public Dialog onCreateDialog(Bundle savedInstanceState) {

        AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
        LayoutInflater inflater = getActivity().getLayoutInflater();
        View view = inflater.inflate(R.layout.dialog_open_file, null);

        fileIcon = (ImageView)view.findViewById(R.id.file_icon);
        fileNameText = (TextView)view.findViewById(R.id.file_name);
        progessBar = (ProgressBar)view.findViewById(R.id.progress_bar);

        fileIcon.setImageResource(Utils.getFileIcon(fileName));
        fileNameText.setText(fileName);

        builder.setView(view);
        builder.setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                if (fileTask != null) {
                    fileTask.cancel(true);
                }
            }
        });

        Dialog dialog = builder.create();

        dialog.setOnShowListener(new DialogInterface.OnShowListener() {
            @Override
            public void onShow(DialogInterface d) {
                loadFile();
            }
        });

        return dialog;
    }

    private class LoadFileTask extends AsyncTask<Void, Long, File> {
        private String repoName;
        private String repoID;
        private String path;
        private SeafException err;

        public LoadFileTask(String repoName, String repoID, String path) {
            this.repoName = repoName;
            this.repoID = repoID;
            this.path = path;
        }

        @Override
        protected File doInBackground(Void...params) {
            ProgressMonitor monitor = new ProgressMonitor() {
                @Override
                public void onProgressNotify(long value) {
                    onProgressUpdate(value);
                }

                @Override
                public boolean isCancelled() {
                    return LoadFileTask.this.isCancelled();
                }
            };

            try {
                return getBrowserActivity().getDataManager().getFile(repoName, repoID, path, monitor);
            } catch (SeafException e) {
                e.printStackTrace();
                err = e;
                return null;
            }
        }

        @Override
        public void onProgressUpdate(Long...values) {
            OpenFileDialog.this.onProgressUpdate(values[0]);
        }

        @Override
        public void onPostExecute(File file) {
            BrowserActivity mActivity = getBrowserActivity();
            if (mActivity == null) {
                return;
            }

            if (err != null) {
                mActivity.showToast("error: " + err);
            } else {
                getDialog().dismiss();
                mActivity.showFile(file);
            }
        }
    }
}