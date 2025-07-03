package com.seafile.seadroid2.framework.helper;

import com.seafile.seadroid2.enums.FeatureDataSource;

public interface ITransferNotification {
    void showNotification(FeatureDataSource source, String subTitle);

    void showProgress(FeatureDataSource source, String fileName, int percent);

    void showCompleted(FeatureDataSource source);

    void showError(FeatureDataSource source);

    void clearLater(FeatureDataSource source);

    void clearAll();

    void clearAll(long delayMillis);
}
