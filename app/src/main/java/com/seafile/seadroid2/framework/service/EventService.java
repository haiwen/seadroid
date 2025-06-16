package com.seafile.seadroid2.framework.service;

import android.app.Service;
import android.os.Bundle;

import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.notification.GeneralNotificationHelper;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;

public abstract class EventService extends Service {
    private final String TAG = "EventService";
    public static final int SEGMENT_SIZE = 8192;

    public static final String KEY_DATA_SOURCE = "key_data_source";
    public static final String KEY_DATA_STATUS = "key_data_event";
    public static final String KEY_DATA_RESULT = "key_data_result";

    public static final String KEY_TRANSFER_ID = "key_transfer_id";
    public static final String KEY_TRANSFER_TRANSFERRED_SIZE = "key_transfer_transferred_size";
    public static final String KEY_TRANSFER_TOTAL_SIZE = "key_transfer_total_size";

    public static final String KEY_TRANSFER_COUNT = "key_transfer_count";

    public static final String DATA_DIRENT_LIST_KEY = "data_dirent_list_key";
    public static final String DATA_FORCE_TRANSFER_KEY = "data_transfer_force_key";

    private GeneralNotificationHelper generalNotificationHelper;

    @Override
    public void onCreate() {
        super.onCreate();

        this.generalNotificationHelper = new GeneralNotificationHelper(getApplicationContext());
    }

    public GeneralNotificationHelper getGeneralNotificationHelper() {
        return generalNotificationHelper;
    }

    public void sendWorkerEvent(FeatureDataSource dataSource, String event) {
        send(dataSource, event);
    }

    public void sendWorkerEvent(FeatureDataSource dataSource, String event, Bundle extra) {
        send(dataSource, event, extra);
    }

    public void sendProgressFinishEvent(TransferModel transferModel) {
        Bundle b = new Bundle();
        b.putString(KEY_TRANSFER_ID, transferModel.getId());
        if (transferModel.transfer_status == TransferStatus.SUCCEEDED) {
            send(transferModel.data_source, TransferEvent.EVENT_FILE_TRANSFER_SUCCESS, b);
        } else {
            send(transferModel.data_source, TransferEvent.EVENT_FILE_TRANSFER_FAILED, b);
        }
    }


    public void sendProgressEvent(TransferModel transferModel) {
        Bundle b = new Bundle();
        b.putString(KEY_TRANSFER_ID, transferModel.getId());
        send(transferModel.data_source, TransferEvent.EVENT_FILE_IN_TRANSFER, b);
    }

    private void send(FeatureDataSource dataSource, String event) {
        send(dataSource, event, null);
    }

    private void send(FeatureDataSource dataSource, String event, Bundle extra) {
        Bundle b = new Bundle();
        b.putString(TransferWorker.KEY_DATA_SOURCE, dataSource.name());
        b.putString(TransferWorker.KEY_DATA_STATUS, event);
        if (extra != null) {
            b.putAll(extra);
        }
        send(b);
    }

    private void send(Bundle eventData) {
        BusHelper.getTransferProgressObserver().post(eventData);
    }
}
