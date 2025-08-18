package com.seafile.seadroid2.framework.service;

import com.seafile.seadroid2.enums.FeatureDataSource;

public interface ITransferNotification {
    void showTransferNotification(FeatureDataSource source, String subTitle);

    void showProgress(FeatureDataSource source, String fileName, int percent);

    void clearDelay(FeatureDataSource... source);
}
