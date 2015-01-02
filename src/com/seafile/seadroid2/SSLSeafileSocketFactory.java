package com.seafile.seadroid2;

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

    private SSLContext m_ctx;
    private String[] m_ciphers;
    private String[] m_protocols;

    public SSLSeafileSocketFactory(KeyManager[] km, TrustManager[] tm, SecureRandom random) throws NoSuchAlgorithmException, KeyManagementException
    {
        m_ctx = SSLContext.getInstance("TLS");
        m_ctx.init(km, tm, random);

        m_protocols = GetProtocolList();
        m_ciphers = GetCipherList();
    }

    public String[] getDefaultCipherSuites()
    {
        return m_ciphers;
    }

    public String[] getSupportedCipherSuites()
    {
        return m_ciphers;
    }

    public Socket createSocket(Socket s, String host, int port, boolean autoClose) throws IOException
    {
        SSLSocketFactory factory = m_ctx.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(s, host, port, autoClose);

        ss.setEnabledProtocols(m_protocols);
        ss.setEnabledCipherSuites(m_ciphers);

        return ss;
    }

    public Socket createSocket(InetAddress address, int port, InetAddress localAddress, int localPort) throws IOException
    {
        SSLSocketFactory factory = m_ctx.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(address, port, localAddress, localPort);

        ss.setEnabledProtocols(m_protocols);
        ss.setEnabledCipherSuites(m_ciphers);

        return ss;
    }

    public Socket createSocket(String host, int port, InetAddress localHost, int localPort) throws IOException
    {
        SSLSocketFactory factory = m_ctx.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(host, port, localHost, localPort);

        ss.setEnabledProtocols(m_protocols);
        ss.setEnabledCipherSuites(m_ciphers);

        return ss;
    }

    public Socket createSocket(InetAddress host, int port) throws IOException
    {
        SSLSocketFactory factory = m_ctx.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(host, port);

        ss.setEnabledProtocols(m_protocols);
        ss.setEnabledCipherSuites(m_ciphers);

        return ss;
    }

    public Socket createSocket(String host, int port) throws IOException
    {
        SSLSocketFactory factory = m_ctx.getSocketFactory();
        SSLSocket ss = (SSLSocket)factory.createSocket(host, port);

        ss.setEnabledProtocols(m_protocols);
        ss.setEnabledCipherSuites(m_ciphers);

        return ss;
    }

    protected String[] GetProtocolList()
    {
        // don't offer SSLv2 or SSLv3
        if (Build.VERSION.SDK_INT >= 16) {
            return new String[]{ "TLSv1", "TLSv1.1", "TLSv1.2" };
        } else {
            return new String[]{ "TLSv1" };
        }
    }

    protected String[] GetCipherList() {

        SSLSocketFactory factory = m_ctx.getSocketFactory();
        String[] availableCiphers = factory.getSupportedCipherSuites();
        return availableCiphers;

    }


}
