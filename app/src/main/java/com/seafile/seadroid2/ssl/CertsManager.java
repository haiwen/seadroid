package com.seafile.seadroid2.ssl;

import java.security.cert.X509Certificate;
import java.util.List;
import java.util.Map;

import android.text.TextUtils;

import com.blankj.utilcode.util.EncryptUtils;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.framework.datastore.DataStoreKeys;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.preferences.Settings;

/**
 * Save the ssl certificates the user has confirmed to trust
 */
public final class CertsManager {

    private final Map<Account, X509Certificate> cachedCerts = Maps.newConcurrentMap();

    private static CertsManager instance;

    public static synchronized CertsManager instance() {
        if (instance == null) {
            instance = new CertsManager();
        }

        return instance;
    }

    public void saveCertForAccount(final Account account, boolean rememberChoice) {
        List<X509Certificate> certs = SSLTrustManager.instance().getCertsChainForAccount(account);
        if (certs == null || certs.isEmpty()) {
            return;
        }

        final X509Certificate cert = certs.get(0);
        cachedCerts.put(account, cert);

        if (rememberChoice) {
            // save cert info to shared preferences
            String certBase64 = CertsHelper.getCertBase64(cert);
            String keyPrefix = EncryptUtils.encryptMD5ToString(account.getServer());
            Settings.getCommonPreferences().edit().putString(DataStoreKeys.KEY_SERVER_CERT_INFO + "_" + keyPrefix, certBase64).apply();
        }
    }


    public X509Certificate getCertificate(Account account) {
        X509Certificate cert = cachedCerts.get(account);
        if (cert != null) {
            return cert;
        }
        String keyPrefix = EncryptUtils.encryptMD5ToString(account.getServer());
        String certBase64 = Settings.getCommonPreferences().getString(DataStoreKeys.KEY_SERVER_CERT_INFO + "_" + keyPrefix, null);
        if (TextUtils.isEmpty(certBase64)) {
            return null;
        }

        cert = CertsHelper.convertToCert(certBase64);
        if (cert != null) {
            cachedCerts.put(account, cert);
        }

        return cert;
    }
}
