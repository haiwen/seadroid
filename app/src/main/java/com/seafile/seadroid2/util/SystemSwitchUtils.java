package com.seafile.seadroid2.util;

import android.content.ContentResolver;
import android.content.Context;
import android.net.ConnectivityManager;


public class SystemSwitchUtils {
    private Context context;
    private ConnectivityManager connManager;
    private static SystemSwitchUtils util;

    public static SystemSwitchUtils getInstance(Context context){
        if (util==null) {
            util=new SystemSwitchUtils(context);
        }
        return util;

    }

    private SystemSwitchUtils(Context context) {
        super();
        this.context = context;
    }


    /**
     * Open Sync
     * @return
     */
    @SuppressWarnings("deprecation")
    public boolean isSyncSwitchOn() {
        if (connManager == null) {
            connManager = (ConnectivityManager) context
                    .getSystemService(Context.CONNECTIVITY_SERVICE);
        }

        return connManager.getBackgroundDataSetting()
                && ContentResolver.getMasterSyncAutomatically();
    }

    /**
     *  synchro switch
     */
    public void syncSwitchUtils() {

        if(!isSyncSwitchOn()){
            ContentResolver.setMasterSyncAutomatically(!isSyncSwitchOn());
        }

    }



}
