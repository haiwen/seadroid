package com.seafile.seadroid2.framework.worker.body;

import android.content.Context;
import android.net.Uri;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.framework.service.ParentEventUploader;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.listener.FileTransferProgressListener;

import java.io.IOException;
import java.io.InputStream;

import okio.Buffer;
import okio.BufferedSink;
import okio.Okio;
import okio.Source;

public class UriStreamRequestBody extends BaseRequestBody {
    private final Uri uri;
    private final Context context;
    private final long estimationFileLength;

    public UriStreamRequestBody(Context context, Uri uri, long size, FileTransferProgressListener fileTransferProgressListener) {
        super(fileTransferProgressListener);
        this.context = context;
        this.uri = uri;
        this.estimationFileLength = size;
    }

    /**
     * maybe -1 (chunked)
     *
     * @see com.seafile.seadroid2.framework.service.FileUploadUtils#resolveSize(Context, Uri)
     */
    @Override
    public long contentLength() {
        return estimationFileLength;
    }


    @Override
    public void writeTo(@NonNull BufferedSink sink) throws IOException {
        try (InputStream inputStream = context.getContentResolver().openInputStream(uri)) {
            if (inputStream == null) {
                throw new IOException("Failed to open input stream for URI: " + uri);
            }

            SafeLogs.d(ParentEventUploader.TAG, "UriStreamRequestBody", "Stream Upload");

            try (Source source = Okio.source(inputStream); Buffer buffer = new Buffer()) {
                long lastUpdateTime = System.currentTimeMillis();
                long totalWritten = 0;

                while (true) {
                    if (shouldStopUpload()) {
                        return;
                    }

                    long readCount = source.read(buffer, TransferWorker.SEGMENT_SIZE);
                    if (readCount == -1) break; // End of file

                    sink.write(buffer, readCount);
                    totalWritten += readCount;

                    // Throttle progress updates
                    long now = System.currentTimeMillis();
                    if (now - lastUpdateTime >= UPDATE_INTERVAL_MS) {
                        dispatchProgress(totalWritten, safeTotal(estimationFileLength, totalWritten));
                        lastUpdateTime = now;
                    }
                }

                // Final update for completion to ensure 100% is reported.
                dispatchProgress(totalWritten, safeTotal(estimationFileLength, totalWritten));
            }
        } catch (IOException e) {
            SLogs.e(e);
            throw e;
        }
    }
}
