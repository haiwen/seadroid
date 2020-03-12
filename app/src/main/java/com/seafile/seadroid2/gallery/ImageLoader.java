package com.seafile.seadroid2.gallery;

import android.content.ContentResolver;
import android.graphics.Bitmap;
import android.os.Handler;
import android.util.Log;

import java.util.ArrayList;

import com.google.common.collect.Lists;

/**
 * A dedicated decoding thread used by ImageGallery.
 */
public class ImageLoader {
    @SuppressWarnings("unused")
    private static final String TAG = "ImageLoader";

    // Queue of work to do in the worker thread. The work is done in order.
    private final ArrayList<WorkItem> mQueue = Lists.newArrayList();

    // the worker thread and a done flag so we know when to exit
    private boolean mDone;
    private Thread mDecodeThread;
    private ContentResolver mCr;

    public interface LoadedCallback {
        void run(Bitmap result);
    }

    public void getBitmap(IImage image,
                          LoadedCallback imageLoadedRunnable,
                          int tag) {
        if (mDecodeThread == null) {
            start();
        }
        synchronized (mQueue) {
            WorkItem w = new WorkItem(image, imageLoadedRunnable, tag);
            mQueue.add(w);
            mQueue.notifyAll();
        }
    }

    public boolean cancel(final IImage image) {
        synchronized (mQueue) {
            int index = findItem(image);
            if (index >= 0) {
                mQueue.remove(index);
                return true;
            } else {
                return false;
            }
        }
    }

    // The caller should hold mQueue lock.
    private int findItem(IImage image) {
        for (int i = 0; i < mQueue.size(); i++) {
            if (mQueue.get(i).mImage == image) {
                return i;
            }
        }
        return -1;
    }

    // Clear the queue. Returns an array of tags that were in the queue.
    public int[] clearQueue() {
        synchronized (mQueue) {
            int n = mQueue.size();
            int[] tags = new int[n];
            for (int i = 0; i < n; i++) {
                tags[i] = mQueue.get(i).mTag;
            }
            mQueue.clear();
            return tags;
        }
    }

    private static class WorkItem {
        IImage mImage;
        LoadedCallback mOnLoadedRunnable;
        int mTag;

        WorkItem(IImage image, LoadedCallback onLoadedRunnable, int tag) {
            mImage = image;
            mOnLoadedRunnable = onLoadedRunnable;
            mTag = tag;
        }
    }

    public ImageLoader(ContentResolver cr, Handler handler) {
        mCr = cr;
        start();
    }

    private class WorkerThread implements Runnable {

        // Pick off items on the queue, one by one, and compute their bitmap.
        // Place the resulting bitmap in the cache, then call back by executing
        // the given runnable so things can get updated appropriately.
        public void run() {
            while (true) {
                WorkItem workItem = null;
                synchronized (mQueue) {
                    if (mDone) {
                        break;
                    }
                    if (!mQueue.isEmpty()) {
                        workItem = mQueue.remove(0);
                    } else {
                        try {
                            mQueue.wait();
                        } catch (InterruptedException ex) {
                            // ignore the exception
                        }
                        continue;
                    }
                }

                final Bitmap b = workItem.mImage.miniThumbBitmap();

                if (workItem.mOnLoadedRunnable != null) {
                    workItem.mOnLoadedRunnable.run(b);
                }
            }
        }
    }

    private void start() {
        if (mDecodeThread != null) {
            return;
        }

        mDone = false;
        Thread t = new Thread(new WorkerThread());
        t.setName("image-loader");
        t.setUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
            @Override
            public void uncaughtException(Thread thread, Throwable ex) {
                Log.e(TAG, "Uncaught exception", ex);
            }
        });
        mDecodeThread = t;
        t.start();
    }

    public void stop() {
        synchronized (mQueue) {
            mDone = true;
            mQueue.notifyAll();
        }
        if (mDecodeThread != null) {
            try {
                Thread t = mDecodeThread;
                BitmapManager.instance().cancelThreadDecoding(t, mCr);
                t.join();
                mDecodeThread = null;
            } catch (InterruptedException ex) {
                // so now what?
            }
        }
    }
}