package com.seafile.seadroid2.ui.transfer_list;

import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.settings.TabSettings2Fragment;

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

        initWorkerBusObserver();
    }

    private void initWorkerBusObserver() {
        BusHelper.getTransferProgressObserver().observe(getViewLifecycleOwner(), new Observer<Bundle>() {
            @Override
            public void onChanged(Bundle bundle) {
                doBusWork(bundle);
            }
        });
    }

    private void doBusWork(Bundle map) {

        String dataSource = map.getString(TransferWorker.KEY_DATA_SOURCE);
        String statusEvent = map.getString(TransferWorker.KEY_DATA_STATUS);
        String result = map.getString(TransferWorker.KEY_DATA_RESULT);
        String transferId = map.getString(TransferWorker.KEY_TRANSFER_ID);
        int transferCount = map.getInt(TransferWorker.KEY_TRANSFER_COUNT);
        SLogs.d("upload list fragment, event: " + statusEvent + ", dataSource: " + dataSource + ", count: " + transferCount);

        if (TextUtils.equals(TransferDataSource.DOWNLOAD.name(), dataSource)) {
            return;
        }

        TransferModel transferModel = getUploadModel(transferId);
        if (transferModel == null) {
            return;
        }

        if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCANNING)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_SCAN_FINISH)) {

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_IN_TRANSFER)) {

            notifyProgressById(transferModel, statusEvent);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_FAILED)) {

            SLogs.d(UploadListFragment.class,transferModel.toString());
            notifyProgressById(transferModel, statusEvent);

        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_FILE_TRANSFER_SUCCESS)) {

            notifyProgressById(transferModel, statusEvent);
        } else if (TextUtils.equals(statusEvent, TransferEvent.EVENT_TRANSFER_FINISH)) {
            loadData();
        }

    }

//    @Override
//    public void deleteSelectedItems(List<FileTransferEntity> list) {
//        showConfirmDialog(new DialogInterface.OnClickListener() {
//            @Override
//            public void onClick(DialogInterface dialog, int which) {
//
//                BackgroundJobManagerImpl.getInstance().cancelFolderBackupWorker();
//
//                getViewModel().removeSpecialUploadListTask(list, new Consumer<Boolean>() {
//                    @Override
//                    public void accept(Boolean aBoolean) throws Exception {
//                        //todo 检查此处逻辑
////                        BackgroundJobManagerImpl.getInstance().startFolderUploadWorker();
//
//                        //You never know which item a user will select, so we need to remove them one by one, and then resort.
//                        for (FileTransferEntity fileTransferEntity : list) {
//                            removeSpecialEntity(fileTransferEntity.uid);
//                        }
//
//                        getViewModel().getShowLoadingDialogLiveData().setValue(false);
//
//                        ToastUtils.showLong(R.string.deleted);
//                    }
//                });
//
//                dialog.dismiss();
//            }
//        });
//    }

//    /**
//     * remove all download tasks
//     */
//    public void removeAllTasks() {
//        showConfirmDialog(new DialogInterface.OnClickListener() {
//            @Override
//            public void onClick(DialogInterface dialog, int which) {
//                //
//                BackgroundJobManagerImpl.getInstance().cancelFolderBackupWorker();
//
//                //
//                getViewModel().removeAllUploadTask(new Consumer<Boolean>() {
//                    @Override
//                    public void accept(Boolean aBoolean) throws Exception {
//                        ToastUtils.showLong(R.string.upload_cancelled);
//
//                        refreshData();
//                    }
//                });
//            }
//        });
//
//    }

//    private void showConfirmDialog(DialogInterface.OnClickListener listener) {
//        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(requireContext());
//        builder.setTitle(R.string.delete);
//        builder.setMessage(R.string.delete_records);
//        builder.setPositiveButton(R.string.ok, listener);
//
//        builder.setNegativeButton(R.string.cancel, (dialog, which) -> dialog.dismiss());
//        builder.show();
//    }
}

