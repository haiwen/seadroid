package com.seafile.seadroid2.ui.transfer_list;

import android.content.DialogInterface;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.work.Data;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.model.enums.TransferAction;
import com.seafile.seadroid2.framework.data.model.enums.TransferDataSource;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.download.DownloadWorker;
import com.seafile.seadroid2.framework.worker.SupportWorkManager;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.util.List;

import io.reactivex.functions.Consumer;

public class DownloadListFragment extends TransferListFragment {

    public static DownloadListFragment newInstance() {
        Bundle args = new Bundle();
        DownloadListFragment fragment = new DownloadListFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public TransferAction getTransferAction() {
        return TransferAction.DOWNLOAD;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initWorkerListener();

    }

    private void initWorkerListener() {
        SupportWorkManager.getWorkManager()
                .getWorkInfoByIdLiveData(DownloadWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        doWorkInfoLiveData(TransferDataSource.DOWNLOAD, workInfo);
                    }
                });
    }

    private String lastTransferId = null;

    private void doWorkInfoLiveData(TransferDataSource dataSource, WorkInfo workInfo) {
        if (workInfo == null) {
            return;
        }

        Data outData = workInfo.getOutputData();
        String outEvent = outData.getString(TransferWorker.KEY_DATA_EVENT);
        if (TransferEvent.EVENT_FINISH.equals(outEvent)) {
            refreshData();
            return;
        }


        Data progressData = workInfo.getProgress();
        String progressEvent = progressData.getString(TransferWorker.KEY_DATA_EVENT);

        if (TransferEvent.EVENT_TRANSFERRING.equals(progressEvent)) {

            String transferId = progressData.getString(TransferWorker.DATA_TRANSFER_ID_KEY);
            String fileName = progressData.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);
            int percent = progressData.getInt(TransferWorker.KEY_DATA_PROGRESS, 0);
            long transferredSize = progressData.getLong(TransferWorker.KEY_DATA_TRANSFERRED_SIZE, 0);
            long totalSize = progressData.getLong(TransferWorker.KEY_DATA_TOTAL_SIZE, 0);

            SLogs.d("download: " + fileName + ", percent：" + percent + ", total_size：" + totalSize + ", dataSource: " + dataSource);

            if (TextUtils.equals(transferId, lastTransferId)) {
                notifyProgressById(transferId, transferredSize, percent, progressEvent);
            } else {
                lastTransferId = transferId;

            }
        } else if (TransferEvent.EVENT_TRANSFER_SUCCESS.equals(progressEvent)) {
            String transferId = progressData.getString(TransferWorker.DATA_TRANSFER_ID_KEY);
            String fileName = progressData.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);
            long transferredSize = progressData.getLong(TransferWorker.KEY_DATA_TRANSFERRED_SIZE, 0);
            long totalSize = progressData.getLong(TransferWorker.KEY_DATA_TOTAL_SIZE, 0);

            SLogs.d("download finish: " + fileName + ", total_size：" + totalSize + ", dataSource: " + dataSource);

            notifyProgressById(transferId, transferredSize, 100, progressEvent);

        } else if (TransferEvent.EVENT_TRANSFER_FAILED.equals(progressEvent)) {
            String transferId = progressData.getString(TransferWorker.DATA_TRANSFER_ID_KEY);
            String fileName = progressData.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);
            long transferredSize = progressData.getLong(TransferWorker.KEY_DATA_TRANSFERRED_SIZE, 0);
            long totalSize = progressData.getLong(TransferWorker.KEY_DATA_TOTAL_SIZE, 0);

            SLogs.d("download failed: " + fileName + ", dataSource: " + dataSource);

            notifyProgressById(transferId, transferredSize, 0, progressEvent);
        }

    }

    @Override
    public void deleteSelectedItems(List<FileTransferEntity> list) {
        showDeleteConfirmDialog(list);
    }

    private void showDeleteConfirmDialog(List<FileTransferEntity> list) {
        showConfirmDialog(new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {

                BackgroundJobManagerImpl.getInstance().cancelFilesDownloadJob();

                getViewModel().removeSpecialDownloadListTask(list, new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {

                        BackgroundJobManagerImpl.getInstance().scheduleOneTimeFilesDownloadScanWorker();

                        ToastUtils.showLong(R.string.deleted);

                        dialog.dismiss();

                        refreshData();
                    }
                });
            }
        });
    }

    private void showConfirmDialog(DialogInterface.OnClickListener listener) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(getContext());
        builder.setTitle(R.string.delete);
        builder.setMessage(R.string.delete_records_and_file);
        builder.setPositiveButton(R.string.ok, listener);

        builder.setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss());
        builder.show();
    }

    @Override
    public void restartSelectedItems(List<FileTransferEntity> list) {
        getViewModel().restartDownload(list, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {
                ToastUtils.showLong(R.string.transfer_list_restart_all);
            }
        });

        getViewModel().restartUpload(list, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {

                //
                BackgroundJobManagerImpl.getInstance().startFileDownloadWorker();
            }
        });

    }

    /**
     * cancel all download tasks
     */
    public void cancelAllTasks() {
        BackgroundJobManagerImpl.getInstance().cancelFilesDownloadJob();

        getViewModel().cancelAllDownloadTask(new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {
                ToastUtils.showLong(R.string.cancel_download);

                refreshData();
            }
        });
    }

    /**
     * remove all download tasks
     */
    public void removeAllTasks() {
        showConfirmDialog(new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {

                //cancel worker
                BackgroundJobManagerImpl.getInstance().cancelFilesDownloadJob();

                getViewModel().removeAllDownloadTask(new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        ToastUtils.showLong(R.string.deleted);

                        refreshData();
                    }
                });
            }
        });

    }
}

