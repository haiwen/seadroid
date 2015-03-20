package com.seafile.seadroid2.transfer;

import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.util.Utils;

/**
 * Base class
 * <p/>
 * reference for override equals and hashcode, http://www.javaranch.com/journal/2002/10/equalhash.html
 */
public abstract class TransferTaskInfo implements TransferItem {
    public final Account account;
    public final int taskID;
    public final TaskState state;
    public final String repoID;
    public final String repoName;
    public final String localFilePath;
    public final SeafException err;

    /**
     * Constructor
     *
     * @param account   Current login Account instance
     * @param taskID    TransferTask id
     * @param state     TransferTask state, value is one of INIT, TRANSFERRING, FINISHED, CANCELLED, FAILED of {@link TaskState}
     * @param repoID    Repository id
     * @param repoName  Repository name
     * @param localPath Local path
     * @param err       Exception instance of {@link SeafException}
     */
    public TransferTaskInfo(Account account, int taskID, TaskState state, String repoID,
                            String repoName, String localPath,
                            SeafException err) {
        this.account = account;
        this.taskID = taskID;
        this.state = state;
        this.repoID = repoID;
        this.repoName = repoName;
        this.localFilePath = localPath;
        this.err = err;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == this)
            return true;
        if ((obj == null) || (obj.getClass() != this.getClass()))
            return false;
        TransferTaskInfo tti = (TransferTaskInfo) obj;
        return (account.getSignature() == tti.account.getSignature() || (account.getSignature() != null && account.getSignature().equals(tti.account.getSignature())))
                && (repoID == tti.repoID || (repoID != null && repoID.equals(tti.repoID)))
                && (localFilePath == tti.localFilePath || (localFilePath != null && localFilePath.equals(tti.localFilePath)));
    }

    @Override
    public String toString() {
        return "email " + account.getEmail() + " server " + account.getServer() + " taskID " + taskID + " repoID " + repoID +
                " repoName " + repoName + " localFilePath " + localFilePath;
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 31 * hash + (account.getSignature() == null ? 0 : account.getSignature().hashCode());
        hash = 31 * hash + (repoID == null ? 0 : repoID.hashCode());
        hash = 31 * hash + (localFilePath == null ? 0 : localFilePath.hashCode());
        return hash;
    }

    public int getViewType() {
        return 1;
    }

    @Override
    public View getView(LayoutInflater inflater, View convertView) {
        View view = convertView;
        Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(SeadroidApplication.getAppContext()).inflate(R.layout.transfer_list_item, null);
            ImageView icon = (ImageView) view.findViewById(R.id.transfer_file_icon);
            TextView state = (TextView) view.findViewById(R.id.transfer_file_state);
            TextView targetPath = (TextView) view.findViewById(R.id.transfer_target_path);
            TextView fileName = (TextView) view.findViewById(R.id.transfer_file_name);
            TextView fileSize = (TextView) view.findViewById(R.id.transfer_file_size);
            ProgressBar progressBar = (ProgressBar) view.findViewById(R.id.transfer_file_progress_bar);
            viewHolder = new Viewholder(icon, state, targetPath, fileName, fileSize, progressBar);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        updateTaskView(viewHolder);

        return view;
    }

    protected abstract void updateTaskView(Viewholder viewHolder);

    class Viewholder {
        ImageView icon;
        TextView targetPath, fileName, fileSize, state;
        ProgressBar progressBar;

        public Viewholder(ImageView icon, TextView state, TextView targetPath,
                          TextView fileName, TextView fileSize, ProgressBar progressBar) {
            this.icon = icon;
            this.state = state;
            this.targetPath = targetPath;
            this.fileName = fileName;
            this.fileSize = fileSize;
            this.progressBar = progressBar;
        }
    }
}