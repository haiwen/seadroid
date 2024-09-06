package com.seafile.seadroid2.bus;

import com.jeremyliao.liveeventbus.LiveEventBus;
import com.jeremyliao.liveeventbus.core.Observable;
import com.seafile.seadroid2.enums.TransferOpType;

public class TransferBusHelper {
    private static final String KEY = "BUS_TRANSFER_OP";

    public static Observable<TransferOpType> getTransferObserver() {
        return LiveEventBus.get(KEY, TransferOpType.class);
    }

    public static void startFileMonitor() {
        getTransferObserver().post(TransferOpType.FILE_MONITOR_START);
    }

    public static void resetFileMonitor() {
        getTransferObserver().post(TransferOpType.FILE_MONITOR_RESET);
    }
}
