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
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadFileManuallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.upload.UploadMediaFileAutomaticallyWorker;

import java.util.List;

import io.reactivex.functions.Consumer;

public class UploadListFragment extends TransferListFragment {

    public static UploadListFragment newInstance() {

        Bundle args = new Bundle();

        UploadListFragment fragment = new UploadListFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public TransferAction getTransferAction() {
        return TransferAction.UPLOAD;
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initWorkerListener();

    }

    private void initWorkerListener() {
        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(UploadFolderFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        doWorkInfoLiveData(TransferDataSource.FOLDER_BACKUP, workInfo);
                    }
                });

        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(UploadFileManuallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        doWorkInfoLiveData(TransferDataSource.FILE_BACKUP, workInfo);
                    }
                });

        BackgroundJobManagerImpl.getInstance().getWorkManager()
                .getWorkInfoByIdLiveData(UploadMediaFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        doWorkInfoLiveData(TransferDataSource.ALBUM_BACKUP, workInfo);
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
        boolean isUploaded = outData.getBoolean(TransferWorker.KEY_DATA_PARAM, false);

        if (TransferEvent.EVENT_FINISH.equals(outEvent) && isUploaded) {
            refreshData();
            return;
        }

        Data progressData = workInfo.getProgress();
        String progressEvent = progressData.getString(TransferWorker.KEY_DATA_EVENT);

        if (TransferEvent.EVENT_CANCEL_WITH_OUT_OF_QUOTA.equals(progressEvent)) {
            refreshData();
        } else if (TransferEvent.EVENT_TRANSFERRING.equals(progressEvent)) {

            String transferId = progressData.getString(TransferWorker.DATA_TRANSFER_ID_KEY);
            String fileName = progressData.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);
            int percent = progressData.getInt(TransferWorker.KEY_DATA_PROGRESS, 0);
            long transferredSize = progressData.getLong(TransferWorker.KEY_DATA_TRANSFERRED_SIZE, 0);
            long totalSize = progressData.getLong(TransferWorker.KEY_DATA_TOTAL_SIZE, 0);

            SLogs.d("upload: " + fileName + ", percent：" + percent + ", total_size：" + totalSize + ", dataSource: " + dataSource);

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

            SLogs.d("upload finish: " + fileName + ", total_size：" + totalSize + ", dataSource: " + dataSource);

            notifyProgressById(transferId, transferredSize, 100, progressEvent);

        } else if (TransferEvent.EVENT_TRANSFER_FAILED.equals(progressEvent)) {
            String transferId = progressData.getString(TransferWorker.DATA_TRANSFER_ID_KEY);
            String fileName = progressData.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);
            long transferredSize = progressData.getLong(TransferWorker.KEY_DATA_TRANSFERRED_SIZE, 0);
            long totalSize = progressData.getLong(TransferWorker.KEY_DATA_TOTAL_SIZE, 0);

            SLogs.d("upload failed: " + fileName + ", total_size：" + totalSize + ", dataSource: " + dataSource);

            notifyProgressById(transferId, transferredSize, 0, progressEvent);
        }

    }

    @Override
    public void deleteSelectedItems(List<FileTransferEntity> list) {
        showConfirmDialog(new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {

                BackgroundJobManagerImpl.getInstance().cancelAllFolderUploadWorker();

                getViewModel().removeSpecialUploadListTask(list, new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        //todo 检查此处逻辑
//                        BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();

                        ToastUtils.showLong(R.string.deleted);

                        dialog.dismiss();

                        refreshData();
                    }
                });
            }
        });
    }


    @Override
    public void restartSelectedItems(List<FileTransferEntity> list) {
        getViewModel().restartUpload(list, new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {

                //todo 检查此处逻辑
                BackgroundJobManagerImpl.getInstance().startFolderChainWorker(true);
                BackgroundJobManagerImpl.getInstance().startMediaChainWorker(true);
            }
        });
    }

    /**
     * cancel all download tasks
     */
    public void cancelAllTasks() {

        BackgroundJobManagerImpl.getInstance().cancelAllFolderUploadWorker();
        BackgroundJobManagerImpl.getInstance().cancelAllMediaWorker();

        getViewModel().cancelAllUploadTask(new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {
                ToastUtils.showLong(R.string.upload_cancelled);

                refreshData();
            }
        });
    }


    /**
     * remove all download tasks
     */
    public void removeAllTasks() {
        //todo
        showConfirmDialog(new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                //
                BackgroundJobManagerImpl.getInstance().cancelAllFolderUploadWorker();

                //
                getViewModel().removeAllUploadTask(new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        ToastUtils.showLong(R.string.upload_cancelled);

                        refreshData();
                    }
                });
            }
        });

    }

    private void showConfirmDialog(DialogInterface.OnClickListener listener) {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(getContext());
        builder.setTitle(R.string.delete);
        builder.setMessage(R.string.delete_records);
        builder.setPositiveButton(R.string.ok, listener);

        builder.setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss());
        builder.show();
    }

}

