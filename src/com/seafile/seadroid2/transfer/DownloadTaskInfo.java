package com.seafile.seadroid2.transfer;

import android.graphics.Color;
import android.view.View;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

/**
 *  download task info
 */
public class DownloadTaskInfo extends TransferTaskInfo {

    public final String pathInRepo;
    public final long fileSize, finished;

    /**
     * Constructor of DownloadTaskInfo
     * 
     * @param account Current login Account instance
     * @param taskID TransferTask id
     * @param state TransferTask state, value is one of <strong>INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED</strong> of {@link TaskState}
     * @param repoID Repository id
     * @param repoName Repository name
     * @param pathInRepo File path in Repository
     * @param localPath Local path
     * @param fileSize File total size
     * @param finished File downloaded size
     * @param err Exception instance of {@link SeafException}
     */
    public DownloadTaskInfo(Account account, int taskID, TaskState state,
            String repoID, String repoName, String pathInRepo,
            String localPath, long fileSize, long finished, SeafException err) {
        super(account, taskID, state, repoID, repoName, localPath, err);

        this.pathInRepo = pathInRepo;
        this.fileSize = fileSize;
        this.finished = finished;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null || (obj.getClass() != this.getClass()))
            return false;

        DownloadTaskInfo a = (DownloadTaskInfo) obj;
        if (!super.equals(a))
            return false;

        if (a.pathInRepo == null)
            return false;

        return a.pathInRepo.equals(this.pathInRepo) && a.fileSize == this.fileSize && a.finished == this.finished;
    }

    @Override
    protected void updateTaskView(Viewholder viewHolder) {
        int iconID = Utils.getFileIcon(pathInRepo);
        // the three fileds is not dynamic
        viewHolder.icon.setImageResource(iconID);
        viewHolder.targetPath.setText(Utils.pathJoin(repoName, Utils.getParentPath(pathInRepo)));
        viewHolder.fileName.setText(Utils.fileNameFromPath(pathInRepo));

        String stateStr = "";
        int stateColor = R.color.light_black;
        long totalSize = 0l;
        long transferedSize = 0l;
        totalSize = fileSize;
        transferedSize = finished;
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