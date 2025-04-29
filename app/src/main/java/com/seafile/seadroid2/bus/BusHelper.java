package com.seafile.seadroid2.bus;

import android.os.Bundle;

import com.blankj.utilcode.util.NetworkUtils;
import com.jeremyliao.liveeventbus.LiveEventBus;
import com.jeremyliao.liveeventbus.core.Observable;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.enums.TransferOpType;

public class BusHelper {
    private static final String FILE_MONITOR_EVENT = "FILE_MONITOR_EVENT";
    private static final String TRANSFER_PROGRESS_EVENT = "TRANSFER_PROGRESS_EVENT";
    private static final String NAV_CONTEXT_EVENT = "NAV_CONTEXT_EVENT";
    private static final String DATA_REFRESH_EVENT = "DATA_REFRESH_EVENT";
    private static final String NETWORK_STATUS_CHANGE_EVENT = "NETWORK_STATUS_CHANGE_EVENT";

    //transfer progress
    public static Observable<Bundle> getCustomBundleObserver() {
        return LiveEventBus.get(DATA_REFRESH_EVENT);
    }

    //nav context
    public static Observable<NavContext> getNavContextObserver() {
        return LiveEventBus.get(NAV_CONTEXT_EVENT);
    }

    //network
    public static Observable<NetworkUtils.NetworkType> getNetworkStatusObserver() {
        return LiveEventBus.get(NETWORK_STATUS_CHANGE_EVENT);
    }

    //transfer progress
    public static Observable<Bundle> getTransferProgressObserver() {
        return LiveEventBus.get(TRANSFER_PROGRESS_EVENT);
    }

    //file monitor
    public static Observable<TransferOpType> getTransferObserver() {
        return LiveEventBus.get(FILE_MONITOR_EVENT, TransferOpType.class);
    }

    public static void startFileMonitor() {
        getTransferObserver().post(TransferOpType.FILE_MONITOR_START);
    }

    public static void resetFileMonitor() {
        getTransferObserver().post(TransferOpType.FILE_MONITOR_RESET);
    }
}
