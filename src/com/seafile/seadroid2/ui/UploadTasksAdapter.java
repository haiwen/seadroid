package com.seafile.seadroid2.ui;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Collections;
import java.util.Comparator;

import android.content.Context;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.graphics.Color;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.TransferManager.UploadTaskInfo;
import com.seafile.seadroid2.Utils;

public class UploadTasksAdapter extends BaseAdapter {
    private List<UploadTaskInfo> mTaskInfos;
    private Map<Integer, Viewholder> mTaskViewMap;
    private Context mContext;

    @SuppressWarnings("unused")
    private static final String DEBUG_TAG = "UploadTasksAdapter";

    public UploadTasksAdapter(Context context, List<UploadTaskInfo> infos) {
        setTaskInfos(infos);
        mContext = context;
        mTaskViewMap = new HashMap<Integer, Viewholder>();
    }

    private class TaskInfoComparator implements Comparator<UploadTaskInfo> {
        private int taskStateToInteger(UploadTaskInfo info) {
            switch (info.state) {
            case INIT:
                return 0;
            case TRANSFERRING:
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
        public int compare(UploadTaskInfo infoA, UploadTaskInfo infoB) {
            // sort task list, init < transferring  < cancelled < failed <  finished
            return taskStateToInteger(infoA) - taskStateToInteger(infoB);
        }
    }

    public void setTaskInfos(List<UploadTaskInfo> infos) {
        mTaskInfos = infos;
        Collections.sort(mTaskInfos, new TaskInfoComparator());
    }

    @Override
    public int getCount() {
        return mTaskInfos.size();
    }

    @Override
    public boolean isEmpty() {
        return mTaskInfos.isEmpty();
    }

    @Override
    public UploadTaskInfo getItem(int position) {
        return mTaskInfos.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    private void updateTaskView(UploadTaskInfo info, Viewholder viewHolder) {
        String stateStr = "";
        int stateColor = R.color.light_black;
        long total = info.totalSize;
        long uploaded = info.uploadedSize;
        String sizeStr = Utils.readableFileSize(total).toString();

        switch (info.state) {
        case INIT:
            stateStr = "Waiting";
            break;
        case TRANSFERRING:
            int percent = (int)(uploaded * 100 / total);
            viewHolder.progressBar.setProgress(percent);
            sizeStr = String.format("%s / %s",
                                    Utils.readableFileSize(uploaded),
                                    Utils.readableFileSize(total));
            viewHolder.progressBar.setVisibility(View.VISIBLE);
            break;
        case FINISHED:
            stateStr = "Finished";
            stateColor = Color.BLACK;
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        case CANCELLED:
            stateStr = "Cancelled";
            stateColor = Color.RED;
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        case FAILED:
            stateStr = "Failed";
            stateColor = Color.RED;
            viewHolder.progressBar.setVisibility(View.INVISIBLE);
            break;
        }

        viewHolder.fileSize.setText(sizeStr);
        viewHolder.state.setText(stateStr);
        viewHolder.state.setTextColor(stateColor);
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        UploadTaskInfo taskInfo = mTaskInfos.get(position);
        View view = convertView;
        Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mContext).inflate(R.layout.upload_list_item, null);
            ImageView icon = (ImageView)view.findViewById(R.id.upload_file_icon);
            TextView state = (TextView)view.findViewById(R.id.upload_file_state);
            TextView targetPath = (TextView)view.findViewById(R.id.upload_target_path);
            TextView fileName = (TextView)view.findViewById(R.id.upload_file_name);
            TextView fileSize = (TextView)view.findViewById(R.id.upload_file_size);
            ProgressBar progressBar = (ProgressBar)view.findViewById(R.id.upload_file_progress_bar);
            viewHolder = new Viewholder(icon, state, targetPath, fileName, fileSize, progressBar);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder)convertView.getTag();
        }

        int iconID = Utils.getFileIcon(taskInfo.localFilePath);
        String fullpath = taskInfo.repoName + taskInfo.parentDir;

        // the three fileds is not dynamic
        viewHolder.icon.setImageResource(iconID);
        viewHolder.targetPath.setText(fullpath);
        viewHolder.fileName.setText(Utils.fileNameFromPath(taskInfo.localFilePath));

        updateTaskView(taskInfo, viewHolder);
        refreshTaskViewMap(taskInfo.taskID, viewHolder);

        return view;
    }

    public void onTaskProgressUpdate(UploadTaskInfo info) {
        handleUploadTaskUpdate(info);
    }

    public void onTaskFailed (UploadTaskInfo info) {
        handleUploadTaskUpdate(info);
    }

    public void onTaskFinished (UploadTaskInfo info) {
        handleUploadTaskUpdate(info);
    }

    public void onTaskCancelled (UploadTaskInfo info) {
        handleUploadTaskUpdate(info);
    }

    private void refreshTaskViewMap(int taskID , Viewholder viewHolder) {
        Iterator<Map.Entry<Integer, Viewholder>> iter;
        iter = mTaskViewMap.entrySet().iterator();
        while (iter.hasNext()) {
            Map.Entry<Integer, Viewholder> entry = iter.next();
            if (viewHolder == entry.getValue()) {
                iter.remove();
            }
        }

        mTaskViewMap.put(taskID, viewHolder);
    }

    private void handleUploadTaskUpdate(UploadTaskInfo info) {
        int taskID;
        Viewholder viewHolder = null;
        for (Map.Entry<Integer, Viewholder> entry: mTaskViewMap.entrySet()) {
            taskID = entry.getKey();
            if (taskID == info.taskID) {
                viewHolder = entry.getValue();
                break;
            }
        }

        if (viewHolder == null) {
            return;
        }

        updateTaskView(info, viewHolder);
        updateTaskInfo(info);
    }

    private void updateTaskInfo(UploadTaskInfo newInfo) {
        int i, n = mTaskInfos.size();
        for (i = 0; i < n; i++) {
            UploadTaskInfo info = mTaskInfos.get(i);
            if (info.taskID == newInfo.taskID) {
                mTaskInfos.set(i, newInfo);
                return;
            }
        }
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



