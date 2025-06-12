package com.seafile.seadroid2.listener;

import com.blankj.utilcode.util.CloneUtils;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

public class FileTransferProgressListener {
    private TransferProgressListener progressListener;
    private TransferModel transferModel;
    private long temp;

    public FileTransferProgressListener() {

    }

    public FileTransferProgressListener(TransferProgressListener progressListener) {
        this.progressListener = progressListener;
    }

    public void setProgressListener(TransferProgressListener progressListener) {
        this.progressListener = progressListener;
    }

    public void setCurrentTransferModel(TransferModel transferModel) {
        this.transferModel = CloneUtils.deepClone(transferModel, TransferModel.class);
    }

    public void onProgressNotify(long cur, long total) {
        if (progressListener == null) {
            throw new IllegalArgumentException("progressListener is null");
        }

        if (transferModel == null) {
            throw new IllegalArgumentException("uploadModel is null");
        }


        long nowt = System.currentTimeMillis();
        if (nowt - temp < 1000) {
            return;
        }

        temp = nowt;

        transferModel.transferred_size = cur;

        int percent = calc(cur, total);
        progressListener.onProgressNotify(transferModel, percent, cur, total);
    }

    public int onProgress(long cur, long total) {

        long nowt = System.currentTimeMillis();
        if (nowt - temp < 1000) {
            return -1;
        }

        temp = nowt;
        return calc(cur, total);
    }

    public int calc(long cur, long total) {
        int percent = (int) ((float) cur / (float) total * 100);
        return percent;
    }

    public interface TransferProgressListener {
        void onProgressNotify(TransferModel transferModel, int percent, long transferredSize, long totalSize);
    }
}
