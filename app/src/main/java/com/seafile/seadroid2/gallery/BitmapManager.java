package com.seafile.seadroid2.gallery;

import android.content.ContentResolver;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.provider.MediaStore.Images;
import android.provider.MediaStore.Video;
import android.util.Log;

import java.io.FileDescriptor;
import java.util.WeakHashMap;

/**
 * This class provides several utilities to cancel bitmap decoding.
 *
 * The function decodeFileDescriptor() is used to decode a bitmap. During
 * decoding if another thread wants to cancel it, it calls the function
 * cancelThreadDecoding() specifying the Thread which is in decoding.
 *
 * cancelThreadDecoding() is sticky until allowThreadDecoding() is called.
 */
public class BitmapManager {
    private static final String TAG = "BitmapManager";
    private static enum State {CANCEL, ALLOW}
    private static class ThreadStatus {
        public State mState = State.ALLOW;
        public BitmapFactory.Options mOptions;
        public boolean mThumbRequesting;
        @Override
        public String toString() {
            String s;
            if (mState == State.CANCEL) {
                s = "Cancel";
            } else if (mState == State.ALLOW) {
                s = "Allow";
            } else {
                s = "?";
            }
            s = "thread state = " + s + ", options = " + mOptions;
            return s;
        }
    }

    private final WeakHashMap<Thread, ThreadStatus> mThreadStatus =
            new WeakHashMap<Thread, ThreadStatus>();

    private static BitmapManager sManager = null;

    private BitmapManager() {
    }

    /**
     * Get thread status and create one if specified.
     */
    private synchronized ThreadStatus getOrCreateThreadStatus(Thread t) {
        ThreadStatus status = mThreadStatus.get(t);
        if (status == null) {
            status = new ThreadStatus();
            mThreadStatus.put(t, status);
        }
        return status;
    }

    /**
     * The following three methods are used to keep track of
     * BitmapFaction.Options used for decoding and cancelling.
     */
    private synchronized void setDecodingOptions(Thread t,
            BitmapFactory.Options options) {
        getOrCreateThreadStatus(t).mOptions = options;
    }

    synchronized void removeDecodingOptions(Thread t) {
        ThreadStatus status = mThreadStatus.get(t);
        status.mOptions = null;
    }

    /**
     * The following three methods are used to keep track of which thread
     * is being disabled for bitmap decoding.
     */
    public synchronized boolean canThreadDecoding(Thread t) {
        ThreadStatus status = mThreadStatus.get(t);
        if (status == null) {
            // allow decoding by default
            return true;
        }

        boolean result = (status.mState != State.CANCEL);
        return result;
    }

    public synchronized void allowThreadDecoding(Thread t) {
        getOrCreateThreadStatus(t).mState = State.ALLOW;
    }

    public synchronized void cancelThreadDecoding(Thread t, ContentResolver cr) {
        ThreadStatus status = getOrCreateThreadStatus(t);
        status.mState = State.CANCEL;
        if (status.mOptions != null) {
            status.mOptions.requestCancelDecode();
        }

        // Wake up threads in waiting list
        notifyAll();

        // Since our cancel request can arrive MediaProvider earlier than getThumbnail request,
        // we use mThumbRequesting flag to make sure our request does cancel the request.
        try {
            synchronized (status) {
                while (status.mThumbRequesting) {
                    Images.Thumbnails.cancelThumbnailRequest(cr, -1, t.getId());
                    Video.Thumbnails.cancelThumbnailRequest(cr, -1, t.getId());
                    status.wait(200);
                }
            }
        } catch (InterruptedException ex) {
            // ignore it.
        }
    }

    public Bitmap getThumbnail(ContentResolver cr, long origId, int kind,
            BitmapFactory.Options options, boolean isVideo) {
        Thread t = Thread.currentThread();
        ThreadStatus status = getOrCreateThreadStatus(t);

        if (!canThreadDecoding(t)) {
            Log.d(TAG, "Thread " + t + " is not allowed to decode.");
            return null;
        }

        try {
            synchronized (status) {
                status.mThumbRequesting = true;
            }
            if (isVideo) {
                return Video.Thumbnails.getThumbnail(cr, origId, t.getId(),
                        kind, null);
            } else {
                return Images.Thumbnails.getThumbnail(cr, origId, t.getId(),
                        kind, null);
            }
        } finally {
            synchronized (status) {
                status.mThumbRequesting = false;
                status.notifyAll();
            }
        }
    }

    public static synchronized BitmapManager instance() {
        if (sManager == null) {
            sManager = new BitmapManager();
        }
        return sManager;
    }

    /**
     * The real place to delegate bitmap decoding to BitmapFactory.
     */
    public Bitmap decodeFileDescriptor(FileDescriptor fd,
                                       BitmapFactory.Options options) {
        if (options.mCancel) {
            return null;
        }

        Thread thread = Thread.currentThread();
        if (!canThreadDecoding(thread)) {
            Log.d(TAG, "Thread " + thread + " is not allowed to decode.");
            return null;
        }

        setDecodingOptions(thread, options);
        Bitmap b = BitmapFactory.decodeFileDescriptor(fd, null, options);

        removeDecodingOptions(thread);
        return b;
    }
}