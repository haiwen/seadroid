package com.seafile.seadroid2.framework.http.download;

import static com.google.common.net.HttpHeaders.CONTENT_LENGTH;

import java.io.IOException;
import java.util.Objects;

import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;

public class BinaryFileDownloader implements AutoCloseable {
    private final OkHttpClient client;
    private final BinaryFileWriter writer;

    public BinaryFileDownloader(OkHttpClient client, BinaryFileWriter writer) {
        this.client = client;
        this.writer = writer;
    }

    public long download(String url) throws IOException {
        Request request = new Request.Builder().url(url).build();
        Response response = client.newCall(request).execute();
        ResponseBody responseBody = response.body();
        if (responseBody == null) {
            throw new IllegalStateException("Response doesn't contain a file");
        }

        String d = Objects.requireNonNull(response.header(CONTENT_LENGTH, "1"));
        long totalSize = Long.parseLong(d);
        return writer.write(responseBody.byteStream(), totalSize);
    }

    @Override
    public void close() throws Exception {
        writer.close();
    }
}
