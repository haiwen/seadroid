package com.seafile.seadroid2.ui.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.TransferTaskInfo;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.activity.TransferActivity;
import com.seafile.seadroid2.util.Utils;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/*
 * Adapter class for both uploading and downloading tasks
 */
public class TransferTaskAdapter extends BaseAdapter {

    private static final String DEBUG_TAG = "TransferTaskAdapter";

    private List<? extends TransferTaskInfo> mTransferTaskInfos;
    private Context mContext;
    /**  0 mark as Download Task, 1 mark as Upload Task, the same convention with {@link TransferActivity #currentPosition} */
    private int mTransferTaskType = -1;
    public static final int DOWNLOAD_LIST_TAB = 0;
    public static final int UPLOAD_LIST_TAB = 1;

    /**
     * Constructor of {@link TransferTaskAdapter}
     * <p>
     * set {@link TransferTaskAdapter #mDownloadTaskInfos} to null if the task is a uploading task</br>
     * set {@link TransferTaskAdapter #mUploadTaskInfos} to null if the task is a downloading task </br>
     * set {@link TransferTaskAdapter #mTransferTaskType} 0 to mark as Download Task, 1 mark to mark as Upload Task</br>
     * 
     * @param context
     * @param transferTaskInfos
     */
    public TransferTaskAdapter(Context context,
                               List<? extends TransferTaskInfo> transferTaskInfos) {
        this.mTransferTaskInfos = transferTaskInfos;
        this.mContext = context;
    }

    public void setCurrentTab(int whichTab) {
        this.mTransferTaskType = whichTab;
    }

    /*
     * sort transfer list by task state, INIT goes to above, FINISHED goes to bottom.
     */
    private class TaskInfoComparator implements Comparator<TransferTaskInfo> {
        private int taskStateToInteger(TransferTaskInfo info) {
            switch (info.state) {
            case TRANSFERRING:
                return 0;
            case INIT:
                return 1;
            case CANCELLED:
                return 2;
            case FAILED:
                return 3;
            case FINISHED:
                return 4;
            }

            return 0;
        }

        @Override
        public int compare(TransferTaskInfo infoA, TransferTaskInfo infoB) {
            // sort task list, transferring < init < cancelled < failed <  finished
            return taskStateToInteger(infoA) - taskStateToInteger(infoB);
        }
    }

    public void setTransferTaskInfos(List<? extends TransferTaskInfo> infos) {
        mTransferTaskInfos = infos;
        Collections.sort(mTransferTaskInfos, new TaskInfoComparator());
    }

    @Override
    public int getCount() {
        return mTransferTaskInfos.size();
    }

    @Override
    public boolean isEmpty() {
        return mTransferTaskInfos.isEmpty();
    }

    @Override
    public TransferTaskInfo getItem(int position) {
        return mTransferTaskInfos.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    private void updateTaskView(TransferTaskInfo info, Viewholder viewHolder) {
        String stateStr = "";
        int stateColor = R.color.light_black;
        long totalSize = 0l;
        long transferedSize = 0l;
        if (mTransferTaskType == 0) {
            DownloadTaskInfo dti = (DownloadTaskInfo) info;
            totalSize = dti.fileSize;
            transferedSize = dti.finished;
        } else if (mTransferTaskType == 1) {
            UploadTaskInfo uti = (UploadTaskInfo) info;
            totalSize = uti.totalSize;
            transferedSize = uti.uploadedSize;
        }
        String sizeStr = Utils.readableFileSize(totalSize).toString();

        switch (info.state) {
        case INIT:
            stateStr = mContext.getString(R.string.upload_waiting);
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
            stateStr = mContext.getString(R.string.upload_finished);
            stateColor = Color.BLACK;
            viewHolder.fileSize.setVisibility(View.VISIBLE);
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        case CANCELLED:
            stateStr = mContext.getString(R.string.upload_cancelled);
            stateColor = Color.RED;
            viewHolder.fileSize.setVisibility(View.INVISIBLE);
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        case FAILED:
            stateStr = mContext.getString(R.string.upload_failed);
            stateColor = Color.RED;
            viewHolder.fileSize.setVisibility(View.INVISIBLE);
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        }
        viewHolder.fileSize.setText(sizeStr);
        viewHolder.state.setText(stateStr);
        viewHolder.state.setTextColor(stateColor);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View view = convertView;
        Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mContext).inflate(R.layout.transfer_list_item, null);
            ImageView icon = (ImageView)view.findViewById(R.id.transfer_file_icon);
            TextView state = (TextView)view.findViewById(R.id.transfer_file_state);
            TextView targetPath = (TextView)view.findViewById(R.id.transfer_target_path);
            TextView fileName = (TextView)view.findViewById(R.id.transfer_file_name);
            TextView fileSize = (TextView)view.findViewById(R.id.transfer_file_size);
            ProgressBar progressBar = (ProgressBar)view.findViewById(R.id.transfer_file_progress_bar);
            viewHolder = new Viewholder(icon, state, targetPath, fileName, fileSize, progressBar);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }
        
        int iconID;
        if (mTransferTaskType == 0) {
            DownloadTaskInfo taskInfo = (DownloadTaskInfo) mTransferTaskInfos.get(position);
            iconID = Utils.getFileIcon(taskInfo.pathInRepo);
            // the three fileds is not dynamic
            viewHolder.icon.setImageResource(iconID);
            viewHolder.targetPath.setText(Utils.pathJoin(taskInfo.repoName, Utils.getParentPath(taskInfo.pathInRepo)));
            viewHolder.fileName.setText(Utils.fileNameFromPath(taskInfo.pathInRepo));
            updateTaskView(taskInfo, viewHolder);
        } else if (mTransferTaskType == 1) {
            UploadTaskInfo taskInfo = (UploadTaskInfo) mTransferTaskInfos.get(position);
            iconID = Utils.getFileIcon(taskInfo.localFilePath);
            String fullpath = taskInfo.repoName + taskInfo.parentDir;
            // the three fileds is not dynamic
            viewHolder.icon.setImageResource(iconID);
            viewHolder.targetPath.setText(fullpath);
            viewHolder.fileName.setText(Utils.fileNameFromPath(taskInfo.localFilePath));
            updateTaskView(taskInfo, viewHolder);
        }

        return view;
    }

    private class Viewholder {
        ImageView icon;
        TextView targetPath, fileName, fileSize, state;
        ProgressBar progressBar;

        public Viewholder(ImageView icon, TextView state, TextView targetPath,
                          TextView fileName, TextView fileSize, ProgressBar progressBar) {
            super();
            this.icon = icon;
            this.state = state;
            this.targetPath = targetPath;
            this.fileName = fileName;
            this.fileSize = fileSize;
            this.progressBar = progressBar;
        }
    }
}