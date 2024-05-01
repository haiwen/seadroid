package com.seafile.seadroid2.listener;

import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;

public class FileTransferProgressListener {
    private TransferProgressListener progressListener;
    private FileTransferEntity fileTransferEntity;
    private long temp;

    public FileTransferProgressListener() {

    }

    public FileTransferProgressListener(TransferProgressListener progressListener, FileTransferEntity fileTransferEntity) {
        this.progressListener = progressListener;
        this.fileTransferEntity = fileTransferEntity;
    }

    public void setProgressListener(TransferProgressListener progressListener) {
        this.progressListener = progressListener;
    }

    public void setFileTransferEntity(FileTransferEntity fileTransferEntity) {
        this.fileTransferEntity = fileTransferEntity;
    }

    public void onProgressNotify(long cur, long total) {
        if (progressListener == null) {
            throw new IllegalArgumentException("progressListener is null");
        }

        if (fileTransferEntity == null) {
            throw new IllegalArgumentException("fileTransferEntity is null");
        }


        long nowt = System.currentTimeMillis();
        if (nowt - temp < 1000) {
            return;
        }

        temp = nowt;

        fileTransferEntity.transferred_size = cur;

        int percent = (int) ((float) cur / (float) total * 100);
        progressListener.onProgressNotify(fileTransferEntity, percent, cur, total);
    }

    public interface TransferProgressListener {
        void onProgressNotify(FileTransferEntity fileTransferEntity, int percent, long transferredSize, long totalSize);
    }
}
