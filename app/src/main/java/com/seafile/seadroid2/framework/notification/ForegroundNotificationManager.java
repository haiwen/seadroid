package com.seafile.seadroid2.framework.notification;

import java.util.concurrent.atomic.AtomicBoolean;

public class ForegroundNotificationManager {
    private final int foregroundNotificationId;
    private final AtomicBoolean inUse = new AtomicBoolean(false);
    private volatile int currentHolderId = -1;

    public ForegroundNotificationManager(int foregroundNotificationId) {
        this.foregroundNotificationId = foregroundNotificationId;
    }

    /**
     * 尝试占用前台通知 ID
     *
     * @return 如果成功占用，返回通知 ID，否则返回 fallbackId
     */
    public synchronized int acquireOrFallback(int requesterId) {
        if (inUse.get() && currentHolderId == requesterId) {
            return foregroundNotificationId;
        }

        if (inUse.compareAndSet(false, true)) {
            currentHolderId = requesterId;
            return foregroundNotificationId;
        } else {
            return requesterId;
        }
    }

    /**
     * 显式尝试占用，返回是否成功
     */
    public synchronized boolean tryAcquire(int requesterId) {
        boolean success = inUse.compareAndSet(false, true);
        if (success) {
            currentHolderId = requesterId;
        }
        return success;
    }

    /**
     * 释放使用权，只有当前占用者才能释放
     */
    public synchronized void release(int requesterId) {
        if (inUse.get() && currentHolderId == requesterId) {
            inUse.set(false);
            currentHolderId = -1;
        }
    }

    /**
     * 当前是否已被占用
     */
    public boolean isInUse() {
        return inUse.get();
    }

    /**
     * 获取固定 ID
     */
    public int getForegroundNotificationId() {
        return foregroundNotificationId;
    }

    /**
     * 当前持有者 ID
     */
    public int getCurrentHolderId() {
        return currentHolderId;
    }
}
