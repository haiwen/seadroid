package com.seafile.seadroid2.framework.executor;

import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

public class TaskExecutor {
    private final ThreadPoolExecutor _executor;
    private static final int CORE_POOL_SIZE = Runtime.getRuntime().availableProcessors();
    private static final int MAX_POOL_SIZE = CORE_POOL_SIZE * 2;
    private static final long KEEP_ALIVE_TIME = 30L;

    public ThreadPoolExecutor getExecutor() {
        return _executor;
    }

    private TaskExecutor() {
        _executor = new ThreadPoolExecutor(
                CORE_POOL_SIZE,
                MAX_POOL_SIZE,
                KEEP_ALIVE_TIME,
                TimeUnit.SECONDS,
                new LinkedBlockingQueue<>(20), // 限制队列长度
                new ThreadFactory() {
                    private final AtomicInteger count = new AtomicInteger(1);

                    @Override
                    public Thread newThread(Runnable r) {
                        Thread thread = new Thread(r, "TransferService-Task-" + count.getAndIncrement());
                        // 设置线程优先级避免影响UI
                        thread.setPriority(Thread.MIN_PRIORITY + (Thread.NORM_PRIORITY - Thread.MIN_PRIORITY) / 2);
                        return thread;
                    }
                }
        );
        _executor.allowCoreThreadTimeOut(true);
    }

    public static TaskExecutor getInstance() {
        return SingletonHolder.INSTANCE;
    }

    private static class SingletonHolder {
        private static final TaskExecutor INSTANCE = new TaskExecutor();
    }
}
