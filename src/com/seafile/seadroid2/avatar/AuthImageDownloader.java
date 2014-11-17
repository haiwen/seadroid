package com.seafile.seadroid2.avatar;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;

import javax.net.ssl.HttpsURLConnection;

import android.content.Context;

import com.github.kevinsawicki.http.HttpRequest;
import com.nostra13.universalimageloader.core.assist.FlushedInputStream;
import com.nostra13.universalimageloader.core.download.BaseImageDownloader;

public class AuthImageDownloader extends BaseImageDownloader {
    public static final String TAG = AuthImageDownloader.class.getName();

    public AuthImageDownloader(Context context, int connectTimeout,
            int readTimeout) {
        super(context, connectTimeout, readTimeout);
    }

    @Override
    protected InputStream getStreamFromNetwork(String imageUri, Object extra)
            throws IOException {
        HttpRequest req = HttpRequest.get(imageUri, null, false)
            .readTimeout(readTimeout)
            .connectTimeout(connectTimeout);
        HttpURLConnection conn = req.getConnection();

        if (conn instanceof HttpsURLConnection) {
            // Tell HttpRequest to trust all hosts, and then the user will get a dialog
            // where he needs to confirm the SSL certificate for the account,
            // and the accepted certificate will be stored, so he is not prompted to accept later on.
            // This is handled by SSLTrustManager and CertsManager
            req.trustAllHosts();
            // TODO: use SSLTrustManager to verify user-trusted certs
            req.trustAllCerts();
        }

        return new FlushedInputStream(new BufferedInputStream(
                req.stream()));
    }
}
