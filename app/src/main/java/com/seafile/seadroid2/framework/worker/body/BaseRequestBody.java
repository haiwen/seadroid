package com.seafile.seadroid2.framework.worker.body;

import androidx.annotation.Nullable;

import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.listener.FileTransferProgressListener;

import okhttp3.MediaType;
import okhttp3.RequestBody;

public abstract class BaseRequestBody extends RequestBody {
    public static final long UPDATE_INTERVAL_MS = 1000L;
    protected static final MediaType MEDIA_TYPE = MediaType.parse("application/octet-stream");

    @Nullable
    protected final FileTransferProgressListener progressListener;
    private volatile boolean stop;

    protected BaseRequestBody(@Nullable FileTransferProgressListener progressListener) {
        this.progressListener = progressListener;
    }

    public void setStop(boolean stop) {
        this.stop = stop;
    }

    @Override
    public final MediaType contentType() {
        return MEDIA_TYPE;
    }

    protected boolean shouldStopUpload() {
        if (Thread.currentThread().isInterrupted()) {
            SLogs.e(new InterruptedException("Upload canceled"));
            return true;
        }
        return stop;
    }

    protected void dispatchProgress(long current, long total) {
        if (progressListener != null) {
            progressListener.onProgressNotify(current, total);
        }
    }

    protected long safeTotal(long expectedTotal, long current) {
        return expectedTotal > 0 ? expectedTotal : current;
    }
}