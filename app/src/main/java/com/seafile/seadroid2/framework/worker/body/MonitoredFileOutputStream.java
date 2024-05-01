package com.seafile.seadroid2.framework.worker.body;

import com.seafile.seadroid2.framework.data.FileBlocks;
import com.seafile.seadroid2.listener.FileTransferProgressListener;
import com.seafile.seadroid2.framework.worker.TransferWorker;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.nio.file.Files;

public class MonitoredFileOutputStream extends OutputStream {

    private static final long PROGRESS_UPDATE_INTERVAL = 500;
    private final FileTransferProgressListener listener;
    private final OutputStream dst;
    private long bytesWritten = 0;
    private long nextUpdate = System.currentTimeMillis() + PROGRESS_UPDATE_INTERVAL;

    private FileBlocks fileBlocks;
    private String blockId;
    private long totalLength;

    public MonitoredFileOutputStream(FileBlocks fileBlocks, String blockId, File file, long totalLength, FileTransferProgressListener listener) throws IOException {
        this.dst = Files.newOutputStream(file.toPath());
        this.listener = listener;
        this.totalLength = totalLength;
        if (fileBlocks != null) {
            this.fileBlocks = fileBlocks;
            this.blockId = blockId;
        }
    }

    @Override
    public void write(byte[] buffer, int off, int len) throws IOException {
        dst.write(buffer, off, len);
        bytesWritten += len;
        checkMonitor();
    }

    @Override
    public void write(byte[] buffer) throws IOException {
        dst.write(buffer);
        bytesWritten += buffer.length;
        checkMonitor();
    }

    @Override
    public void write(int b) throws IOException {
        dst.write(b);
        ++bytesWritten;
        if (bytesWritten % TransferWorker.SEGMENT_SIZE == 0) {
            checkMonitor();
        }
    }

    @Override
    public void close() throws IOException {
        dst.close();
    }

    private void checkMonitor() {
        if (System.currentTimeMillis() > nextUpdate) {
            if (fileBlocks != null) {
                fileBlocks.getBlock(blockId).finished = bytesWritten;

                listener.onProgressNotify(fileBlocks.getFinished(), totalLength);
            } else {

                listener.onProgressNotify(100, 100);
            }
            nextUpdate = System.currentTimeMillis() + PROGRESS_UPDATE_INTERVAL;
        }
    }
}
