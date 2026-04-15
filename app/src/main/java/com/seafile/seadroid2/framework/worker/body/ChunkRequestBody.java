package com.seafile.seadroid2.framework.worker.body;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import okhttp3.MediaType;
import okhttp3.RequestBody;
import okio.BufferedSink;

public class ChunkRequestBody extends RequestBody {
    private final File file;
    private final long offset; // 当前块在文件中的起始偏移量
    private final int chunkSize; // 当前块需要读取的大小
    private final MediaType contentType;

    public ChunkRequestBody(File file, long offset, int chunkSize, String contentType) {
        this.file = file;
        this.offset = offset;
        this.chunkSize = chunkSize;
        this.contentType = MediaType.parse(contentType);
    }

    @Override
    public MediaType contentType() {
        return contentType;
    }

    @Override
    public long contentLength() {
        return chunkSize;
    }

    @Override
    public void writeTo(BufferedSink sink) throws IOException {
        // 使用 RandomAccessFile 可以指定从哪个位置开始读取
        try (RandomAccessFile randomAccessFile = new RandomAccessFile(file, "r")) {
            randomAccessFile.seek(offset); // 核心：移动文件指针到当前块的起始位置

            byte[] buffer = new byte[8192]; // 8KB 缓冲区
            int bytesRead;
            int totalRead = 0;

            // 确保只读取当前块规定大小的数据
            while (totalRead < chunkSize &&
                    (bytesRead = randomAccessFile.read(buffer, 0, Math.min(buffer.length, chunkSize - totalRead))) != -1) {
                sink.write(buffer, 0, bytesRead);
                totalRead += bytesRead;

                // 这里可以回调当前块的上传进度
            }
        }
    }
}
