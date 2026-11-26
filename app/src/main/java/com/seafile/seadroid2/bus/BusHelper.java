package com.seafile.seadroid2.bus;

import android.os.Bundle;

import com.blankj.utilcode.util.NetworkUtils;
import com.jeremyliao.liveeventbus.LiveEventBus;
import com.jeremyliao.liveeventbus.core.Observable;
import com.seafile.seadroid2.context.NavContext;

public class BusHelper {
    private static final String TRANSFER_PROGRESS_EVENT = "TRANSFER_PROGRESS_EVENT";
    private static final String NAV_CONTEXT_EVENT = "NAV_CONTEXT_EVENT";
    private static final String DATA_REFRESH_EVENT = "DATA_REFRESH_EVENT";
    private static final String NETWORK_STATUS_CHANGE_EVENT = "NETWORK_STATUS_CHANGE_EVENT";

    private static final String COMMON_BUS_EVENT = "COMMON_BUS_EVENT";
    private static final String MEDIA_MOUNT_BUS_EVENT = "MediaMountEvent";

    /**
     * Offline root switch started: stop file monitoring and other operations.
     */
    public static final String ACTION_OFFLINE_ROOT_SWITCH_START = "ACTION_OFFLINE_ROOT_SWITCH_START";
    /**
     * Offline root switch completed: re-initialize monitoring for the new root.
     */
    public static final String ACTION_OFFLINE_ROOT_SWITCH_COMPLETE = "ACTION_OFFLINE_ROOT_SWITCH_COMPLETE";


    public static Observable<String> getMediaMountObserver() {
        return LiveEventBus.get(MEDIA_MOUNT_BUS_EVENT);
    }

    public static Observable<String> getCommonObserver() {
        return LiveEventBus.get(COMMON_BUS_EVENT);
    }

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
}
