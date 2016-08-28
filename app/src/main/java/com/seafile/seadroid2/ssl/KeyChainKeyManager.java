package com.seafile.seadroid2.ssl;

import android.content.Context;
import android.security.KeyChain;
import android.security.KeyChainException;
import android.util.Log;

import java.net.Socket;
import java.security.Principal;
import java.security.PrivateKey;

import javax.net.ssl.X509KeyManager;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;

/**
 * KeyManager to support client side authentication
 */
public class KeyChainKeyManager implements X509KeyManager {
    public static final String DEBUG_TAG = "KeyChainKeyManager";

    private final String mClientAlias;
    private final X509Certificate[] mCertificateChain;
    private final PrivateKey mPrivateKey;

    public static KeyChainKeyManager fromAlias(Context context, String alias)
            throws CertificateException {
        X509Certificate[] certificateChain;
        try {
            certificateChain = KeyChain.getCertificateChain(context, alias);
        } catch (KeyChainException | InterruptedException e) {
            Log.e(DEBUG_TAG, String.format("alias %s certificate chain error", alias, e.getMessage()));
            throw new CertificateException(e);
        }

        PrivateKey privateKey;
        try {
            privateKey = KeyChain.getPrivateKey(context, alias);
        } catch (KeyChainException | InterruptedException e) {
            Log.e(DEBUG_TAG, String.format("alias %s private key error", alias, e.getMessage()));
            throw new CertificateException(e);
        }

        if (certificateChain == null || privateKey == null) {
            throw new CertificateException("Can't access certificate from keystore");
        }

        return new KeyChainKeyManager(alias, certificateChain, privateKey);
    }

    private KeyChainKeyManager(
            String clientAlias, X509Certificate[] certificateChain,
            PrivateKey privateKey) {
        mClientAlias = clientAlias;
        mCertificateChain = certificateChain;
        mPrivateKey = privateKey;
    }


    @Override
    public String chooseClientAlias(String[] keyTypes, Principal[] issuers, Socket socket) {
        return mClientAlias;
    }

    @Override
    public String chooseServerAlias(String keyType, Principal[] issuers, Socket socket) {
        return null;
    }

    @Override
    public X509Certificate[] getCertificateChain(String alias) {
        return mCertificateChain;
    }

    @Override
    public String[] getClientAliases(String keyType, Principal[] issuers) {
        return new String[0];
    }

    @Override
    public String[] getServerAliases(String keyType, Principal[] issuers) {
        return new String[0];
    }

    @Override
    public PrivateKey getPrivateKey(String alias) {
        return mPrivateKey;
    }
}