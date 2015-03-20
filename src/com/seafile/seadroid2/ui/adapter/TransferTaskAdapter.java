package com.seafile.seadroid2.ui.adapter;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.transfer.*;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Adapter class for both uploading and downloading tasks
 */
public class TransferTaskAdapter extends BaseAdapter {
    private static final String DEBUG_TAG = "TransferTaskAdapter";

    private LayoutInflater mInflater;

    /**
     * Constructor of {@link TransferTaskAdapter}
     * <p>
     *
     * @param context
     */
    public TransferTaskAdapter(Context context) {
        this.mInflater = LayoutInflater.from(context);
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

    private List<TransferItem> items = Lists.newArrayList();

    public void setTransferTaskInfos(List<? extends TransferTaskInfo> infos) {
        List<? extends TransferTaskInfo>  mTransferTaskInfos = infos;
        Collections.sort(mTransferTaskInfos, new TaskInfoComparator());

        int transferringHeaderCount = 0;
        int waitingHeaderCount = 0;
        int cancelledHeaderCount = 0;
        int failedHeaderCount = 0;
        int finishedHeaderCount = 0;

        int transferringTotal = 0;
        int waitingTotal = 0;
        int cancelledTotal = 0;
        int failedTotal = 0;
        int finishedTotal = 0;

        for (TransferTaskInfo tti : mTransferTaskInfos) {
            if (tti.state.equals(TaskState.TRANSFERRING))
                transferringTotal++;
            else if (tti.state.equals(TaskState.INIT))
                waitingTotal++;
            else if (tti.state.equals(TaskState.CANCELLED))
                cancelledTotal++;
            else if (tti.state.equals(TaskState.FAILED))
                failedTotal++;
            else if (tti.state.equals(TaskState.FINISHED))
                finishedTotal++;
        }

        items.clear();

        for (TransferTaskInfo tti : mTransferTaskInfos) {
            if (tti.state.equals(TaskState.TRANSFERRING)
                    && transferringHeaderCount == 0) {
                items.add(new TransferSectionHeader(TaskState.TRANSFERRING, transferringTotal));
                items.add(tti);
                transferringHeaderCount++;
            } else if (tti.state.equals(TaskState.INIT)
                    && waitingHeaderCount == 0) {
                items.add(new TransferSectionHeader(TaskState.INIT, waitingTotal));
                items.add(tti);
                waitingHeaderCount++;
            } else if (tti.state.equals(TaskState.CANCELLED)
                    && cancelledHeaderCount == 0) {
                items.add(new TransferSectionHeader(TaskState.CANCELLED, cancelledTotal));
                items.add(tti);
                cancelledHeaderCount++;
            } else if (tti.state.equals(TaskState.FAILED)
                    && failedHeaderCount == 0) {
                items.add(new TransferSectionHeader(TaskState.FAILED, failedTotal));
                items.add(tti);
                failedHeaderCount++;
            } else if (tti.state.equals(TaskState.FINISHED)
                    && finishedHeaderCount == 0) {
                items.add(new TransferSectionHeader(TaskState.FINISHED, finishedTotal));
                items.add(tti);
                finishedHeaderCount++;
            } else
                items.add(tti);
        }
    }

    @Override
    public int getCount() {
        return items.size();
    }

    @Override
    public boolean isEmpty() {
        return items.isEmpty();
    }

    @Override
    public TransferItem getItem(int position) {
        return items.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        return getItem(position).getView(mInflater, convertView);
    }

    @Override
    public int getViewTypeCount() {
        return TransferViewType.values().length;

    }

    @Override
    public int getItemViewType(int position) {
        return getItem(position).getViewType();
    }
}