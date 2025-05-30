package com.seafile.seadroid2.framework.worker;

import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.worker.queue.TransferQueue;

public class GlobalTransferCacheList {
    public static final TransferQueue ALBUM_BACKUP_QUEUE = new TransferQueue(true);
    public static final TransferQueue FOLDER_BACKUP_QUEUE = new TransferQueue();
    public static final TransferQueue FILE_UPLOAD_QUEUE = new TransferQueue();
    public static final TransferQueue DOWNLOAD_QUEUE = new TransferQueue();

    /**
     * Put local updated files into queue
     */
    public static final TransferQueue CHANGED_FILE_MONITOR_QUEUE = new TransferQueue();

    public static int getUploadPendingCount() {
        return FOLDER_BACKUP_QUEUE.getPendingCount() + ALBUM_BACKUP_QUEUE.getPendingCount() + FILE_UPLOAD_QUEUE.getPendingCount();
    }

    public static int getDownloadPendingCount() {
        return DOWNLOAD_QUEUE.getPendingCount();
    }

    public static void clear() {
        ALBUM_BACKUP_QUEUE.clear();
        FOLDER_BACKUP_QUEUE.clear();
        FILE_UPLOAD_QUEUE.clear();
        DOWNLOAD_QUEUE.clear();
    }

    public static void updateTransferModel(TransferModel transferModel) {
        if (transferModel == null) {
            return;
        }

        if (transferModel.data_source == TransferDataSource.ALBUM_BACKUP) {
            ALBUM_BACKUP_QUEUE.update(transferModel);
        } else if (transferModel.data_source == TransferDataSource.FOLDER_BACKUP) {
            FOLDER_BACKUP_QUEUE.update(transferModel);
        } else if (transferModel.data_source == TransferDataSource.FILE_BACKUP) {
            FILE_UPLOAD_QUEUE.update(transferModel);
        } else if (transferModel.data_source == TransferDataSource.DOWNLOAD) {
            DOWNLOAD_QUEUE.update(transferModel);
        }
    }
}
