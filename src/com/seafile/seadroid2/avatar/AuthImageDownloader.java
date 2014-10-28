package com.seafile.seadroid2.avatar;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.Iterator;
import java.util.Map;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSocketFactory;

import android.content.Context;
import android.util.Log;

import com.nostra13.universalimageloader.core.assist.FlushedInputStream;
import com.nostra13.universalimageloader.core.download.BaseImageDownloader;
import com.seafile.seadroid2.SSLTrustManager;
import com.seafile.seadroid2.account.Account;


public class AuthImageDownloader extends BaseImageDownloader {
    private static final String TAG = AuthImageDownloader.class.getName();

    public AuthImageDownloader(Context context) {
        super(context);
    }

    public AuthImageDownloader(Context context, int connectTimeout,
            int readTimeout) {
        super(context, connectTimeout, readTimeout);
    }

    @Override
    protected InputStream getStreamFromNetwork(String imageUri, Object extra)
            throws IOException {
        URL url = null;
        try {
            url = new URL(imageUri);
        } catch (MalformedURLException e) {
            Log.e(TAG, e.getMessage(), e);
        }

        HttpURLConnection http = null;

        if (Scheme.ofUri(imageUri) == Scheme.HTTPS) {
           Map<Account, SSLSocketFactory> cachedFactories = SSLTrustManager.instance().getCachedFactories();
           Iterator it = cachedFactories.entrySet().iterator();
            while (it.hasNext()) {
                Map.Entry pairs = (Map.Entry) it.next();
                HttpsURLConnection
                        .setDefaultSSLSocketFactory((SSLSocketFactory) pairs
                                .getValue());
                it.remove(); // avoids a ConcurrentModificationException
            }
            /*HttpsURLConnection
                    .setDefaultHostnameVerifier(MyCustomSSLSocketHandler.getCustomHostnameVerifier());*/
            HttpsURLConnection https = (HttpsURLConnection) url.openConnection();
            http = https;
            http.connect();
        } else {
            http = (HttpURLConnection) url.openConnection();
        }
        http.setConnectTimeout(connectTimeout);
        http.setReadTimeout(readTimeout);
        return new FlushedInputStream(new BufferedInputStream(http.getInputStream()));
    }
}