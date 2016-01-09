package com.seafile.seadroid2;

import android.os.AsyncTask;
import android.os.Build;

/**
 * Make sure an AsyncTask is executed in parallel across different version of
 * Android
 * @see asynctask executing tasks serially or concurrently {@link http://www.jayway.com/2012/11/28/is-androids-asynctask-executing-tasks-serially-or-concurrently/}
 */
public class ConcurrentAsyncTask {
    public static <T> void execute(AsyncTask<T, ?, ?> task, T...args) {
        if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.HONEYCOMB_MR1) {
            task.execute(args);
        } else {
            task.executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR, args);
        }
    }

    public static void execute(Runnable runnable) {
        execute(new SimpleAsyncTask(runnable));
    }

    private static class SimpleAsyncTask extends AsyncTask<Void, Void, Void> {
        Runnable runnable;
        public SimpleAsyncTask(Runnable runnable) {
            this.runnable = runnable;
        }

        public Void doInBackground(Void... args) {
            try {
                runnable.run();
            } catch(Exception e) {
                // ignore
            }
            return null;
        }
    }
}
