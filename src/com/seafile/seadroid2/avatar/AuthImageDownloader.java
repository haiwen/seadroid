package com.seafile.seadroid2.avatar;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;

import javax.net.ssl.HostnameVerifier;
import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSession;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import android.content.Context;
import android.util.Log;

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

        URL url = null;
        try {
            url = new URL(imageUri);
        } catch (MalformedURLException e) {
            Log.e(TAG, e.getMessage(), e);
        }
        HttpURLConnection http = null;

        if (Scheme.ofUri(imageUri) == Scheme.HTTPS) {
            trustAllHosts();
            HttpsURLConnection https = (HttpsURLConnection) url
                    .openConnection();
            https.setHostnameVerifier(DO_NOT_VERIFY);
            http = https;
            http.connect();
        } else {
            http = (HttpURLConnection) url.openConnection();
        }

        http.setConnectTimeout(connectTimeout);
        http.setReadTimeout(readTimeout);
        return new FlushedInputStream(new BufferedInputStream(
                http.getInputStream()));
    }

    // always verify the host - dont check for certificate
    final static HostnameVerifier DO_NOT_VERIFY = new HostnameVerifier() {
        @Override
        public boolean verify(String hostname, SSLSession session) {
            return true;
        }

    };

    /**
     * Trust every server - dont check for any certificate
     */
    private static void trustAllHosts() {
        // Create a trust manager that does not validate certificate chains
        TrustManager[] trustAllCerts = new TrustManager[] { new X509TrustManager() {
            @Override
            public void checkClientTrusted(
                    java.security.cert.X509Certificate[] x509Certificates,
                    String s) throws java.security.cert.CertificateException {
            }

            @Override
            public void checkServerTrusted(
                    java.security.cert.X509Certificate[] x509Certificates,
                    String s) throws java.security.cert.CertificateException {
            }

            @Override
            public java.security.cert.X509Certificate[] getAcceptedIssuers() {
                return new java.security.cert.X509Certificate[] {};
            }
        } };

        // Install the all-trusting trust manager
        try {
            SSLContext sc = SSLContext.getInstance("TLS");
            sc.init(null, trustAllCerts, new java.security.SecureRandom());
            HttpsURLConnection
                    .setDefaultSSLSocketFactory(sc.getSocketFactory());
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}