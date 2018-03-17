package com.seafile.seadroid2.httputils;

import android.util.Log;

import com.seafile.seadroid2.data.ProgressMonitor;

import java.io.File;
import java.io.IOException;
import java.util.concurrent.TimeUnit;

import javax.net.ssl.HttpsURLConnection;

import okhttp3.Interceptor;
import okhttp3.MediaType;
import okhttp3.OkHttpClient;
import okhttp3.RequestBody;
import okhttp3.Response;
import okio.Buffer;
import okio.BufferedSink;
import okio.Okio;
import okio.Source;


public class RequestManager {

    private OkHttpClient client;
    private static final long TIMEOUT_COUNT = 5;

    static {
        HttpsURLConnection.setDefaultSSLSocketFactory(new NoSSLv3Factory());
    }

    private RequestManager() {
        OkHttpClient.Builder builder = new OkHttpClient.Builder();
        builder.addInterceptor(new LoggingInterceptor()); //add okhttp log
        client = builder.hostnameVerifier((hostname, session) -> true)
                .retryOnConnectionFailure(true)
                .connectTimeout(TIMEOUT_COUNT, TimeUnit.MINUTES)
                .readTimeout(TIMEOUT_COUNT, TimeUnit.MINUTES)
                .writeTimeout(TIMEOUT_COUNT, TimeUnit.MINUTES)
                .build();
    }

    public OkHttpClient getClient() {
        return client;
    }


    private static class SingletonFactory {
        private static RequestManager mRequestManager = new RequestManager();
    }

    public static RequestManager getInstance() {
        return SingletonFactory.mRequestManager;
    }


    /**
     * output log interceptor
     */
    private class LoggingInterceptor implements Interceptor {

        @Override
        public Response intercept(Chain chain) throws IOException {
            Response response = chain.proceed(chain.request());
            String content = response.body().string();
            Log.i("LoggingInterceptor", content);
            return response.newBuilder()
                    .body(okhttp3.ResponseBody.create(response.body().contentType(), content))
                    .build();
        }
    }


    /**
     * Create progress RequestBody
     *
     * @param contentType MediaType
     * @param file        update file
     * @param callBack
     * @param <T>
     * @return
     */
    public <T> RequestBody createProgressRequestBody(ProgressMonitor monitor, final File file) {
        return new RequestBody() {

            public long temp = System.currentTimeMillis();

            @Override
            public MediaType contentType() {
                return MediaType.parse("application/octet-stream");
            }

            @Override
            public long contentLength() {
                return file.length();
            }

            @Override
            public void writeTo(BufferedSink sink) throws IOException {
                Source source;
                try {
                    source = Okio.source(file);
                    Buffer buf = new Buffer();
                    // long remaining = contentLength();
                    long current = 0;
                    for (long readCount; (readCount = source.read(buf, 2048)) != -1; ) {
                        sink.write(buf, readCount);
                        current += readCount;
                        long nowt = System.currentTimeMillis();
                        // 1s refresh progress
                        if (nowt - temp >= 1000) {
                            temp = nowt;
                            monitor.onProgressNotify(current, false);
                        }
                    }
                } catch (Exception e) {
                    e.printStackTrace();
                }
            }
        };
    }
}
