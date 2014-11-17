package com.seafile.seadroid2.avatar;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLSocketFactory;

import android.content.Context;

import com.github.kevinsawicki.http.HttpRequest;
import com.nostra13.universalimageloader.core.assist.FlushedInputStream;
import com.nostra13.universalimageloader.core.download.BaseImageDownloader;
import com.seafile.seadroid2.SSLTrustManager;
import com.seafile.seadroid2.account.Account;

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
            
            HttpsURLConnection sconn = (HttpsURLConnection)conn;
            Map<Account, SSLSocketFactory> cachedFactories = SSLTrustManager.instance().getCachedFactories();
            Iterator<Entry<Account, SSLSocketFactory>> it = cachedFactories.entrySet().iterator();
            while (it.hasNext()) {
                Entry<Account, SSLSocketFactory> pairs = it.next();
                if (imageUri.contains(pairs.getKey().server)) {
                sconn.setSSLSocketFactory((SSLSocketFactory) pairs.getValue());
                it.remove(); // avoids a ConcurrentModificationException
                }
            }
        }

        return new FlushedInputStream(new BufferedInputStream(
                req.stream()));
    }
}
