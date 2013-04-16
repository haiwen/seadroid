package com.seafile.seadroid2;

import android.util.Log;

import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;

import com.seafile.seadroid2.SeadroidApplication;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.security.KeyStore;
import java.security.KeyStoreException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.CertificateException;
import java.security.cert.X509Certificate;


public class TrustManagerFactory {

    private static final String LOG_TAG = "TrustManagerFactory";

    private static X509TrustManager defaultTrustManager;
    private static X509TrustManager localTrustManager;

    private static X509Certificate[] lastCertChain = null;

    private static File keyStoreFile;
    private static KeyStore keyStore;


    private static class SimpleX509TrustManager implements X509TrustManager {
        
        public void checkClientTrusted(X509Certificate[] chain, String authType)
            throws CertificateException {
        }

        public void checkServerTrusted(X509Certificate[] chain, String authType)
            throws CertificateException {
        }

        public X509Certificate[] getAcceptedIssuers() {
            return null;
        }
    }

    private static class SecureX509TrustManager implements X509TrustManager {

        public void checkClientTrusted(X509Certificate[] chain, String authType)
                throws CertificateException {
            defaultTrustManager.checkClientTrusted(chain, authType);
        }

        public void checkServerTrusted(X509Certificate[] chain, String authType)
                throws CertificateException {
            TrustManagerFactory.setLastCertChain(chain);
            try {
                defaultTrustManager.checkServerTrusted(chain, authType);
            } catch (CertificateException e) {
                localTrustManager.checkServerTrusted(new X509Certificate[] {chain[0]}, authType);
            }
        }

        public X509Certificate[] getAcceptedIssuers() {
            return defaultTrustManager.getAcceptedIssuers();
        }

    }
    
    public static TrustManager[] getTrustManagers() {
        return new TrustManager[] { new SecureX509TrustManager() }; 
    }

    public static TrustManager[] getUnsecureTrustManagers() {
        return new TrustManager[] { new SimpleX509TrustManager() }; 
    }
    
    static {
        java.io.InputStream fis = null;
        try {
            javax.net.ssl.TrustManagerFactory tmf = javax.net.ssl.TrustManagerFactory.getInstance("X509");
       
            keyStoreFile = new File(SeadroidApplication.getAppContext().getFilesDir(), "KeyStore.bks");
            keyStore = KeyStore.getInstance(KeyStore.getDefaultType());
            try {
                fis = new java.io.FileInputStream(keyStoreFile);
            } catch (FileNotFoundException e1) {
                fis = null;
            }
            try {
                keyStore.load(fis, null);
            } catch (IOException e) {
                Log.e(LOG_TAG, "KeyStore IOException while initializing TrustManagerFactory ", e);
                keyStore = null;
            } catch (CertificateException e) {
                Log.e(LOG_TAG, "KeyStore CertificateException while initializing TrustManagerFactory ", e);
                keyStore = null;
            }
            tmf.init(keyStore);
            TrustManager[] tms = tmf.getTrustManagers();
            if (tms != null) {
                for (TrustManager tm : tms) {
                    if (tm instanceof X509TrustManager) {
                        localTrustManager = (X509TrustManager)tm;
                        break;
                    }
                }
            }
            tmf = javax.net.ssl.TrustManagerFactory.getInstance("X509");
            tmf.init((KeyStore)null);
            tms = tmf.getTrustManagers();
            if (tms != null) {
                for (TrustManager tm : tms) {
                    if (tm instanceof X509TrustManager) {
                        defaultTrustManager = (X509TrustManager) tm;
                        break;
                    }
                }
            }

        } catch (NoSuchAlgorithmException e) {
            Log.e(LOG_TAG, "Unable to get X509 Trust Manager ", e);
        } catch (KeyStoreException e) {
            Log.e(LOG_TAG, "Key Store exception while initializing TrustManagerFactory ", e);
        } finally {
            try {
                if (fis != null)
                    fis.close();
            } catch (IOException e) {
                // ignore
            }
        }
    }

    private TrustManagerFactory() {
    }

    public static KeyStore getKeyStore() {
        return keyStore;
    }

    public static void setLastCertChain(X509Certificate[] chain) {
        lastCertChain = chain;
    }
    
    public static X509Certificate[] getLastCertChain() {
        return lastCertChain;
    }

    public static void addCertificateChain(X509Certificate[] chain) throws CertificateException {
        try {
            javax.net.ssl.TrustManagerFactory tmf = javax.net.ssl.TrustManagerFactory.getInstance("X509");
            for (X509Certificate element : chain) {
                keyStore.setCertificateEntry
                    (element.getSubjectDN().toString(), element);
            }

            tmf.init(keyStore);
            TrustManager[] tms = tmf.getTrustManagers();
            if (tms != null) {
                for (TrustManager tm : tms) {
                    if (tm instanceof X509TrustManager) {
                        localTrustManager = (X509TrustManager) tm;
                        break;
                    }
                }
            }
            
            java.io.OutputStream keyStoreStream = null;
            try {
                keyStoreStream = new java.io.FileOutputStream(keyStoreFile);
                keyStore.store(keyStoreStream, null);
            } catch (FileNotFoundException e) {
                throw new CertificateException("Unable to write KeyStore: " + e.getMessage());
            } catch (CertificateException e) {
                throw new CertificateException("Unable to write KeyStore: " + e.getMessage());
            } catch (IOException e) {
                throw new CertificateException("Unable to write KeyStore: " + e.getMessage());
            } finally {
                try {
                    keyStoreStream.close();
                } catch (IOException e) {
                    // ignore
                }
            }
        } catch (NoSuchAlgorithmException e) {
            Log.e(LOG_TAG, "Unable to get X509 Trust Manager ", e);
        } catch (KeyStoreException e) {
            Log.e(LOG_TAG, "Key Store exception while initializing TrustManagerFactory ", e);
        }
    }

}
