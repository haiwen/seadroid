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
import com.seafile.seadroid2.framework.worker.DownloadWorker;
import com.seafile.seadroid2.framework.worker.SupportWorkManager;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.UploadFolderFileAutomaticallyWorker;
import com.seafile.seadroid2.framework.worker.UploadMediaFileAutomaticallyWorker;

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
        SupportWorkManager.getWorkManager()
                .getWorkInfoByIdLiveData(UploadFolderFileAutomaticallyWorker.UID)
                .observe(getViewLifecycleOwner(), new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        doWorkInfoLiveData(TransferDataSource.FOLDER_BACKUP, workInfo);
                    }
                });


        SupportWorkManager.getWorkManager()
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

        //
        if (workInfo.getState().isFinished()) {
            loadData();
        } else if (workInfo.getState() == WorkInfo.State.RUNNING) {

            Data data = workInfo.getProgress();
            String transferId = data.getString(TransferWorker.DATA_TRANSFER_KEY);
            String fileName = data.getString(TransferWorker.DATA_TRANSFER_NAME_KEY);
            int percent = data.getInt(TransferWorker.KEY_DATA_PROGRESS, 0);
            long transferredSize = data.getLong(TransferWorker.KEY_DATA_TRANSFERRED_SIZE, 0);
            long totalSize = data.getLong(TransferWorker.KEY_DATA_TOTAL_SIZE, 0);

            SLogs.d("upload: " + fileName + ", percent：" + percent + ", total_size：" + totalSize + ", dataSource: " + dataSource);


            if (TextUtils.equals(transferId, lastTransferId)) {
                adapter.notifyProgressById(transferId, transferredSize, percent);
            } else {
                lastTransferId = transferId;
                loadData(false);
            }

        }
    }

    @Override
    public void deleteSelectedItems(List<FileTransferEntity> list) {
        showConfirmDialog(new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {

                BackgroundJobManagerImpl.getInstance().cancelFilesUploadWorker();

                getViewModel().removeSpecialUploadListTask(list, new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();

                        ToastUtils.showLong(R.string.deleted);

                        dialog.dismiss();

                        loadData();
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

                //
                BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();
                BackgroundJobManagerImpl.getInstance().startMediaBackupWorker();
            }
        });
    }

    /**
     * cancel all download tasks
     */
    public void cancelAllTasks() {

        BackgroundJobManagerImpl.getInstance().cancelFilesUploadWorker();
        BackgroundJobManagerImpl.getInstance().cancelMediaWorker();

        getViewModel().cancelAllUploadTask(new Consumer<Boolean>() {
            @Override
            public void accept(Boolean aBoolean) throws Exception {
                ToastUtils.showLong(R.string.upload_cancelled);

                loadData();
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
                BackgroundJobManagerImpl.getInstance().cancelFilesUploadWorker();

                //
                getViewModel().removeAllUploadTask(new Consumer<Boolean>() {
                    @Override
                    public void accept(Boolean aBoolean) throws Exception {
                        ToastUtils.showLong(R.string.upload_cancelled);

                        loadData();
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

