package com.seafile.seadroid2.ssl;

import android.os.Build;

import java.io.IOException;
import java.net.InetAddress;
import java.net.Socket;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import javax.net.ssl.KeyManager;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;
import javax.net.ssl.TrustManager;

/**
 * Custom SSLSocketFactory.
 *
 * Used to manually select TLS protocol versions.
 *
 * based on:
 * https://stackoverflow.com/questions/1037590/which-cipher-suites-to-enable-for-ssl-socket
 */
public class SSLSeafileSocketFactory extends SSLSocketFactory {

    private SSLContext context;
    private String[] allowedCiphers;
    private String[] allowedProtocols;

    public SSLSeafileSocketFactory(KeyManager[] km, TrustManager[] tm, SecureRandom random) throws NoSuchAlgorithmException, KeyManagementException {
        context = SSLContext.getInstance("TLS");
        context.init(km, tm, random);

        allowedProtocols = getProtocolList();
        allowedCiphers = getCipherList();
    }

    public String[] getDefaultCipherSuites()
    {
        return allowedCiphers;
    }

    public String[] getSupportedCipherSuites()
    {
        return allowedCiphers;
    }

    public Socket createSocket(Socket s, String host, int port, boolean autoClose) throws IOException {
        SSLSocketFactory factory = context.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(s, host, port, autoClose);

        ss.setEnabledProtocols(allowedProtocols);
        ss.setEnabledCipherSuites(allowedCiphers);

        return ss;
    }

    public Socket createSocket(InetAddress address, int port, InetAddress localAddress, int localPort) throws IOException {
        SSLSocketFactory factory = context.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(address, port, localAddress, localPort);

        ss.setEnabledProtocols(allowedProtocols);
        ss.setEnabledCipherSuites(allowedCiphers);

        return ss;
    }

    public Socket createSocket(String host, int port, InetAddress localHost, int localPort) throws IOException {
        SSLSocketFactory factory = context.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(host, port, localHost, localPort);

        ss.setEnabledProtocols(allowedProtocols);
        ss.setEnabledCipherSuites(allowedCiphers);

        return ss;
    }

    public Socket createSocket(InetAddress host, int port) throws IOException {
        SSLSocketFactory factory = context.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(host, port);

        ss.setEnabledProtocols(allowedProtocols);
        ss.setEnabledCipherSuites(allowedCiphers);

        return ss;
    }

    public Socket createSocket(String host, int port) throws IOException {
        SSLSocketFactory factory = context.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(host, port);

        ss.setEnabledProtocols(allowedProtocols);
        ss.setEnabledCipherSuites(allowedCiphers);

        return ss;
    }

    protected String[] getProtocolList() {
        // don't offer SSLv2 or SSLv3
        if (Build.VERSION.SDK_INT >= 16) {
            return new String[]{ "TLSv1", "TLSv1.1", "TLSv1.2" };
        } else {
            return new String[]{ "TLSv1" };
        }
    }

    protected String[] getCipherList() {
        // only allow ciphers which are still considered secure.
        // based on:
        // https://briansmith.org/browser-ciphersuites-01.html
        String[] preferredCiphers;

        // Android up to 2.2 use other names
        if (Build.VERSION.SDK_INT <= 8) {
            preferredCiphers = new String[] {
                    "DHE-RSA-AES128-SHA",
                    "DHE-RSA-AES256-SHA",
                    "DHE-DSS-AES128-SHA",
                    "AES128-SHA",
                    "AES256-SHA"
            };
        } else {
            preferredCiphers = new String[] {
                    "TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256",
                    "TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256",
                    "TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA",
                    "TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA",
                    "TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384",
                    "TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384",
                    "TLS_ECDHE_RSA_WITH_AES_256_CBC_SHA",
                    "TLS_ECDHE_ECDSA_WITH_AES_256_CBC_SHA",
                    "TLS_DHE_RSA_WITH_AES_128_CBC_SHA",
                    "TLS_DHE_RSA_WITH_AES_256_CBC_SHA",
                    "TLS_DHE_DSS_WITH_AES_128_CBC_SHA",

                    // backward compatibility. offers no forward security.
                    "TLS_RSA_WITH_AES_128_CBC_SHA",
                    "TLS_RSA_WITH_AES_256_CBC_SHA",

                    // RFC 5746
                    "TLS_EMPTY_RENEGOTIATION_INFO_SCSV"
            };
        }

        // now filter out any ciphers that aren't supported by this device
        SSLSocketFactory factory = context.getSocketFactory();
        String[] availableCiphers = factory.getSupportedCipherSuites();
        ArrayList<String> available = new ArrayList<String>(Arrays.asList(availableCiphers));

        List<String> result = new ArrayList<String>();
        for(int i = 0; i < preferredCiphers.length; i++)
        {
            if(available.contains(preferredCiphers[i]))
                result.add(preferredCiphers[i]);
        }

        return result.toArray(new String[0]);
    }

}
