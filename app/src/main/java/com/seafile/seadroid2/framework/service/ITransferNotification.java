package com.seafile.seadroid2.framework.service;

import com.seafile.seadroid2.enums.FeatureDataSource;

/**
 * Legacy stub retained for constructor compatibility only.
 */
@Deprecated
public interface ITransferNotification {
    default void showTransferNotification(FeatureDataSource source, String subTitle) {}
    default void showProgress(FeatureDataSource source, String fileName, int percent) {}
    default void clearDelay(FeatureDataSource... source) {}
}
