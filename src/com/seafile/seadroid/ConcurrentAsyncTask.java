package com.seafile.seadroid;

import android.os.AsyncTask;
import android.os.Build;

/**
 * AsyncTask that is guaranteed to run concurrently
 * @see http://www.jayway.com/2012/11/28/is-androids-asynctask-executing-tasks-serially-or-concurrently/
 */
public class ConcurrentAsyncTask {
    public static <T> void execute(AsyncTask<T, ?, ?> task, T...args) {
        if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.HONEYCOMB_MR1) {
            task.execute(args);
        } else {
            task.executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR, args);
        }
    }
}