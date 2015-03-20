package com.seafile.seadroid2.transfer;

import android.graphics.Color;
import android.view.View;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

/**
 * upload task info
 */
public class UploadTaskInfo extends TransferTaskInfo {

    public final String parentDir;
    public final long uploadedSize, totalSize;
    public final boolean isUpdate, isCopyToLocal;

    /**
     * Constructor of UploadTaskInfo
     * 
     * @param account Current login Account instance
     * @param taskID TransferTask id
     * @param state TransferTask state, value is one of <strong>INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED</strong> of {@link TaskState}
     * @param repoID Repository id
     * @param repoName Repository name
     * @param parentDir Parent directory of the file
     * @param localPath Local path
     * @param isUpdate Force to update files if true
     * @param isCopyToLocal Copy files to SD card if true
     * @param uploadedSize File uploaded size
     * @param totalSize File total size
     * @param err Exception instance of {@link SeafException}
     */
    public UploadTaskInfo(Account account,
                          int taskID,
                          TaskState state,
                          String repoID,
                          String repoName,
                          String parentDir,
                          String localPath,
                          boolean isUpdate,
                          boolean isCopyToLocal,
                          long uploadedSize,
                          long totalSize,
                          SeafException err) {

        super(account, taskID, state, repoID, repoName, localPath, err);

        this.parentDir = parentDir;
        this.uploadedSize = uploadedSize;
        this.totalSize = totalSize;
        this.isUpdate = isUpdate;
        this.isCopyToLocal = isCopyToLocal;
    }

    @Override
    protected void updateTaskView(Viewholder viewHolder) {
        int iconID = Utils.getFileIcon(localFilePath);
        String fullpath = repoName + parentDir;
        // the three fileds is not dynamic
        viewHolder.icon.setImageResource(iconID);
        viewHolder.targetPath.setText(fullpath);
        viewHolder.fileName.setText(Utils.fileNameFromPath(localFilePath));

        String stateStr = "";
        int stateColor = R.color.light_black;
        long totalSize = 0l;
        long transferedSize = 0l;
        transferedSize = uploadedSize;
        String sizeStr = Utils.readableFileSize(totalSize).toString();

        switch (state) {
            case INIT:
                stateStr = SeadroidApplication.getAppContext().getString(R.string.upload_waiting);
                viewHolder.fileSize.setVisibility(View.INVISIBLE);
                viewHolder.progressBar.setVisibility(View.INVISIBLE);
                break;
            case TRANSFERRING:
                int percent;
                if (totalSize == 0)
                    percent = 0;
                else
                    percent = (int) (transferedSize * 100 / totalSize);

                viewHolder.progressBar.setProgress(percent);
                sizeStr = String.format("%s / %s",
                        Utils.readableFileSize(transferedSize),
                        Utils.readableFileSize(totalSize));
                viewHolder.fileSize.setVisibility(View.VISIBLE);
                viewHolder.progressBar.setVisibility(View.VISIBLE);
                break;
            case FINISHED:
                stateStr = SeadroidApplication.getAppContext().getString(R.string.upload_finished);
                stateColor = Color.BLACK;
                viewHolder.fileSize.setVisibility(View.VISIBLE);
                viewHolder.progressBar.setVisibility(View.INVISIBLE);
                break;
            case CANCELLED:
                stateStr = SeadroidApplication.getAppContext().getString(R.string.upload_cancelled);
                stateColor = Color.RED;
                viewHolder.fileSize.setVisibility(View.INVISIBLE);
                viewHolder.progressBar.setVisibility(View.INVISIBLE);
                break;
            case FAILED:
                stateStr = SeadroidApplication.getAppContext().getString(R.string.upload_failed);
                stateColor = Color.RED;
                viewHolder.fileSize.setVisibility(View.INVISIBLE);
                viewHolder.progressBar.setVisibility(View.INVISIBLE);
                break;
        }
        viewHolder.fileSize.setText(sizeStr);
        viewHolder.state.setText(stateStr);
        viewHolder.state.setTextColor(stateColor);
    }

}
