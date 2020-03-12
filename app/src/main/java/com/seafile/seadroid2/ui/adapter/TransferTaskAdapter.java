package com.seafile.seadroid2.ui.adapter;

import android.graphics.Color;
import android.util.Log;
import android.util.SparseBooleanArray;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.TransferTaskInfo;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.activity.TransferActivity;
import com.seafile.seadroid2.util.Utils;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Adapter class for both uploading and downloading tasks
 */
public class TransferTaskAdapter extends BaseAdapter {

    private static final String DEBUG_TAG = "TransferTaskAdapter";

    private SparseBooleanArray mSelectedItemsIds;
    private List<Integer> mSelectedItemsPositions = Lists.newArrayList();
    private List<? extends TransferTaskInfo> mTransferTaskInfos;
    /** flag to mark if action mode was activated, used to update the state of multi selection buttons */
    private boolean actionModeStarted;
    private TransferActivity mActivity;
    private TaskType mTransferTaskType;
    public enum TaskType {DOWNLOAD_TASK, UPLOAD_TASK}

    /**
     * Constructor of {@link TransferTaskAdapter}
     * <p>
     * set {@link TransferTaskAdapter #mDownloadTaskInfos} to null if the task is a uploading task</br>
     * set {@link TransferTaskAdapter #mUploadTaskInfos} to null if the task is a downloading task </br>
     * set {@link TransferTaskAdapter #mTransferTaskType} 0 to mark as Download Task, 1 mark to mark as Upload Task</br>
     *
     * @param activity
     * @param transferTaskInfos
     */
    public TransferTaskAdapter(TransferActivity activity,
                               List<? extends TransferTaskInfo> transferTaskInfos) {
        this.mTransferTaskInfos = transferTaskInfos;
        this.mActivity = activity;
        this.mSelectedItemsIds = new SparseBooleanArray();
    }

    public void setCurrentTab(TaskType type) {
        this.mTransferTaskType = type;
    }

    /**
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
    public boolean hasStableIds() {
        // make adapter with stable ids by return true.
        // Also in {@link #getItemId} must either override hashCode() or has some kind of id field to be returned
        return true;
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
        if (mTransferTaskType.equals(TaskType.DOWNLOAD_TASK)) {
            DownloadTaskInfo dti = (DownloadTaskInfo) info;
            totalSize = dti.fileSize;
            transferedSize = dti.finished;
        } else if (mTransferTaskType.equals(TaskType.UPLOAD_TASK)) {
            UploadTaskInfo uti = (UploadTaskInfo) info;
            totalSize = uti.totalSize;
            transferedSize = uti.uploadedSize;
        }
        String sizeStr = Utils.readableFileSize(totalSize).toString();

        switch (info.state) {
        case INIT:
            if (mTransferTaskType.equals(TaskType.DOWNLOAD_TASK))
                stateStr = mActivity.getString(R.string.download_waiting);
            else if (mTransferTaskType.equals(TaskType.UPLOAD_TASK))
                stateStr = mActivity.getString(R.string.upload_waiting);
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
            if (mTransferTaskType.equals(TaskType.DOWNLOAD_TASK))
                stateStr = mActivity.getString(R.string.download_finished);
            else if (mTransferTaskType.equals(TaskType.UPLOAD_TASK))
                stateStr = mActivity.getString(R.string.upload_finished);
            stateColor = Color.BLACK;
            viewHolder.fileSize.setVisibility(View.VISIBLE);
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        case CANCELLED:
            if (mTransferTaskType.equals(TaskType.DOWNLOAD_TASK))
                stateStr = mActivity.getString(R.string.download_cancelled);
            else if (mTransferTaskType.equals(TaskType.UPLOAD_TASK))
                stateStr = mActivity.getString(R.string.upload_cancelled);
            stateColor = Color.RED;
            viewHolder.fileSize.setVisibility(View.INVISIBLE);
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        case FAILED:
            if (mTransferTaskType.equals(TaskType.DOWNLOAD_TASK))
                stateStr = mActivity.getString(R.string.download_failed);
            else if (mTransferTaskType.equals(TaskType.UPLOAD_TASK))
                stateStr = mActivity.getString(R.string.upload_failed);
            stateColor = Color.RED;
            viewHolder.fileSize.setVisibility(View.INVISIBLE);
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        }
        viewHolder.fileSize.setText(sizeStr);
        viewHolder.state.setText(stateStr);
        viewHolder.state.setTextColor(stateColor);
    }

    public int getCheckedItemCount() {
        return mSelectedItemsIds.size();
    }

    public List<Integer> getSelectedIds() {
        return mSelectedItemsPositions;
    }

    public void toggleSelection(int position) {
        if (mSelectedItemsIds.get(position)) {
            // unselected
            mSelectedItemsIds.delete(position);
            mSelectedItemsPositions.remove(Integer.valueOf(position));
        } else {
            mSelectedItemsIds.put(position, true);
            mSelectedItemsPositions.add(position);
        }

        mActivity.onItemSelected();
        notifyDataSetChanged();
    }

    public void actionModeOn() {
        actionModeStarted = true;
        notifyDataSetChanged();
    }

    public void actionModeOff() {
        actionModeStarted = false;
        notifyDataSetChanged();
    }

    public void deselectAllItems() {
        mSelectedItemsIds.clear();
        mSelectedItemsPositions.clear();
        notifyDataSetChanged();
    }

    public void selectAllItems() {
        mSelectedItemsIds.clear();
        mSelectedItemsPositions.clear();
        for (int i = 0; i < mTransferTaskInfos.size(); i++) {
            mSelectedItemsIds.put(i, true);
            mSelectedItemsPositions.add(i);
        }
        notifyDataSetChanged();
    }

    @Override
    public View getView(final int position, View convertView, ViewGroup parent) {
        View view = convertView;
        final Viewholder viewHolder;
        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.transfer_list_item, null);
            ImageView icon = (ImageView)view.findViewById(R.id.transfer_file_icon);
            ImageView multiSelectBtn = (ImageView)view.findViewById(R.id.transfer_file_multi_select_btn);
            TextView state = (TextView)view.findViewById(R.id.transfer_file_state);
            TextView targetPath = (TextView)view.findViewById(R.id.transfer_target_path);
            TextView fileName = (TextView)view.findViewById(R.id.transfer_file_name);
            TextView fileSize = (TextView)view.findViewById(R.id.transfer_file_size);
            ProgressBar progressBar = (ProgressBar)view.findViewById(R.id.transfer_file_progress_bar);
            viewHolder = new Viewholder(icon, multiSelectBtn, state, targetPath, fileName, fileSize, progressBar);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        int iconID;
        if (mTransferTaskType.equals(TaskType.DOWNLOAD_TASK)) {
            final DownloadTaskInfo taskInfo = (DownloadTaskInfo) mTransferTaskInfos.get(position);
            iconID = Utils.getFileIcon(taskInfo.pathInRepo);
            // the three fields are not dynamic
            viewHolder.icon.setImageResource(iconID);
            viewHolder.targetPath.setText(Utils.pathJoin(taskInfo.repoName, Utils.getParentPath(taskInfo.pathInRepo)));
            viewHolder.fileName.setText(Utils.fileNameFromPath(taskInfo.pathInRepo));
            // Log.d(DEBUG_TAG, "multi select btn checked " + mSelectedItemsIds.get(position));
            if (mSelectedItemsIds.get(position)) {
                viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_checked);
            } else if (actionModeStarted)
                viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_unchecked);
            else
                viewHolder.multiSelectBtn.setImageResource(R.drawable.btn_multiselect);

            viewHolder.multiSelectBtn.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (!mSelectedItemsIds.get(position)) {
                        viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_checked);
                        mSelectedItemsIds.put(position, true);
                        mSelectedItemsPositions.add(position);
                    } else {
                        viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_unchecked);
                        mSelectedItemsIds.delete(position);
                        mSelectedItemsPositions.remove(Integer.valueOf(position));
                    }

                    mActivity.onItemSelected();
                }
            });
            updateTaskView(taskInfo, viewHolder);
        } else if (mTransferTaskType.equals(TaskType.UPLOAD_TASK)) {
            UploadTaskInfo taskInfo = (UploadTaskInfo) mTransferTaskInfos.get(position);
            iconID = Utils.getFileIcon(taskInfo.localFilePath);
            String fullpath = Utils.pathJoin(taskInfo.repoName, taskInfo.parentDir);
            // the three fileds is not dynamic
            viewHolder.icon.setImageResource(iconID);
            viewHolder.targetPath.setText(fullpath);
            viewHolder.fileName.setText(Utils.fileNameFromPath(taskInfo.localFilePath));
            // Log.d(DEBUG_TAG, "multi select btn checked " + mSelectedItemsIds.get(position));
            if (mSelectedItemsIds.get(position)) {
                viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_checked);
            } else if (actionModeStarted)
                viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_unchecked);
            else
                viewHolder.multiSelectBtn.setImageResource(R.drawable.btn_multiselect);

            viewHolder.multiSelectBtn.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    if (!mSelectedItemsIds.get(position)) {
                        viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_checked);
                        mSelectedItemsIds.put(position, true);
                        mSelectedItemsPositions.add(position);
                    } else {
                        viewHolder.multiSelectBtn.setImageResource(R.drawable.checkbox_unchecked);
                        mSelectedItemsIds.delete(position);
                        mSelectedItemsPositions.remove(Integer.valueOf(position));
                    }

                    mActivity.onItemSelected();
                }
            });
            updateTaskView(taskInfo, viewHolder);
        }

        return view;
    }

    private class Viewholder {
        ImageView icon, multiSelectBtn;
        TextView targetPath, fileName, fileSize, state;
        ProgressBar progressBar;

        public Viewholder(ImageView icon, ImageView multiSelectBtn, TextView state, TextView targetPath,
                          TextView fileName, TextView fileSize, ProgressBar progressBar) {
            super();
            this.icon = icon;
            this.multiSelectBtn = multiSelectBtn;
            this.state = state;
            this.targetPath = targetPath;
            this.fileName = fileName;
            this.fileSize = fileSize;
            this.progressBar = progressBar;
        }
    }
}