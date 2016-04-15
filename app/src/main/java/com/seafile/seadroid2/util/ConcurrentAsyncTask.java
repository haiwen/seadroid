package com.seafile.seadroid2.util;

import android.os.AsyncTask;
import android.os.Build;
import android.support.annotation.NonNull;
import android.util.Log;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.Callable;
import java.util.concurrent.CancellationException;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executor;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Make sure an AsyncTask is executed in parallel across different version of
 * Android
 * @see asynctask executing tasks serially or concurrently {@link http://www.jayway.com/2012/11/28/is-androids-asynctask-executing-tasks-serially-or-concurrently/}
 */
public class ConcurrentAsyncTask {
    private static final String DEBUG_TAG = "ConcurrentAsyncTask";

    /**
     * We extend ThreadPoolExecutor to log exceptions
     */
    private static class SeadroidThreadPoolExecutor extends ThreadPoolExecutor {

        /* Copied over from AsyncTask */
        private static final int CPU_COUNT = Runtime.getRuntime().availableProcessors();
        private static final int CORE_POOL_SIZE = CPU_COUNT + 1;
        private static final int MAXIMUM_POOL_SIZE = CPU_COUNT * 2 + 1;
        private static final int KEEP_ALIVE = 1;
        private static final BlockingQueue<Runnable> sPoolWorkQueue = new LinkedBlockingQueue(128);

        private static final ThreadFactory sThreadFactory = new ThreadFactory() {
            private final AtomicInteger mCount = new AtomicInteger(1);

            public Thread newThread(Runnable r) {
                return new Thread(r, "SeadroidAsyncTask #" + mCount.getAndIncrement());
            }
        };

        public SeadroidThreadPoolExecutor() {
            super(CORE_POOL_SIZE, MAXIMUM_POOL_SIZE, KEEP_ALIVE,
                    TimeUnit.SECONDS, sPoolWorkQueue, sThreadFactory);
        }

        @Override
        protected void afterExecute(Runnable r, Throwable t) {
            super.afterExecute(r, t);

            // some exceptions are stored inside the Future, extract them
            // See ThreadPoolExecutor.afterExecute() javadoc for an explanation
            if (t == null && r instanceof Future<?>) {
                try {
                    Object result = ((Future<?>) r).get();
                } catch (CancellationException ce) {
                    t = ce;
                } catch (ExecutionException ee) {
                    t = ee.getCause();
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt(); // ignore/reset
                }
            }

            if (t != null)
                Log.e(DEBUG_TAG, "Uncaught exception in thread pool", t);
        }
    }

    private static final ThreadPoolExecutor threadPoolExecutor = new SeadroidThreadPoolExecutor();

    public static <T> void execute(AsyncTask<T, ?, ?> task, T...args) {
        if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.HONEYCOMB_MR1) {
            task.execute(args);
        } else {
            task.executeOnExecutor(threadPoolExecutor, args);
        }
    }

    @NonNull
    public static Future<?> submit(Runnable runnable) {
        return threadPoolExecutor.submit(runnable);
    }

    @NonNull
    public static <T> Future<T> submit(Callable<T> task) {
        return threadPoolExecutor.submit(task);
    }
}
