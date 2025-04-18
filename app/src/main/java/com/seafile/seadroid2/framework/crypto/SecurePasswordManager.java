package com.seafile.seadroid2.framework.crypto;

import android.util.Pair;

import com.seafile.seadroid2.framework.util.SLogs;

public class SecurePasswordManager {

    public static Pair<String, String> encryptPassword(String plainPassword) {
        try {
            CryptoHelper.generateKeyIfNecessary();
            byte[][] result = CryptoHelper.encrypt(plainPassword);
            String encryptedBase64 = CryptoHelper.encodeBase64(result[0]);
            String ivBase64 = CryptoHelper.encodeBase64(result[1]);

            return new Pair<>(encryptedBase64, ivBase64);
        } catch (Exception e) {
            SLogs.e(e);
        }
        return null;
    }

    public static String decryptPassword(String encryptedBase64, String ivBase64) {
        try {
            byte[] encrypted = CryptoHelper.decodeBase64(encryptedBase64);
            byte[] iv = CryptoHelper.decodeBase64(ivBase64);
            return CryptoHelper.decrypt(encrypted, iv);
        } catch (Exception e) {
            SLogs.e(e);
        }
        return null;
    }
}
