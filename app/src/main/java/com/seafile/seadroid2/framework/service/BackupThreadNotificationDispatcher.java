package com.seafile.seadroid2.framework.service;

import android.content.Context;
import com.seafile.seadroid2.enums.FeatureDataSource;

/** Legacy source compatibility only. Tokenless callbacks cannot publish or remove business state. */
@Deprecated
public class BackupThreadNotificationDispatcher implements ITransferNotification {
    public BackupThreadNotificationDispatcher(Context context) { }
    public Integer getNotifyId(FeatureDataSource source) { return null; }
    @Override public void showTransferNotification(FeatureDataSource source, String subtitle) { }
    @Override public void showProgress(FeatureDataSource source, String name, int percent) { }
    @Override public void clearDelay(FeatureDataSource... source) { }
}
