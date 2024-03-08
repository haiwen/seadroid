package com.seafile.seadroid2.ui.transfer_list;

import android.os.Bundle;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.data.model.enums.TransferAction;

import java.util.List;

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
    public void deleteSelectedItems(List<FileTransferEntity> list) {
        ToastUtils.showShort("TODO：deleteSelectedItems");
    }

    @Override
    public void restartSelectedItems(List<FileTransferEntity> list) {
        ToastUtils.showShort("TODO：restartSelectedItems");
    }

    /**
     * cancel all download tasks
     */
    public void cancelAllTasks() {
        ToastUtils.showShort("TODO：cancelAllTasks");

    }


    /**
     * remove all download tasks
     */
    public void removeAllTasks() {
        ToastUtils.showShort("TODO：removeAllTasks");

    }
}

