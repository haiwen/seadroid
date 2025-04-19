package com.seafile.seadroid2.framework.crypto;

import android.security.keystore.KeyGenParameterSpec;
import android.security.keystore.KeyProperties;

import java.nio.charset.StandardCharsets;
import java.security.KeyStore;
import java.util.Base64;

import javax.crypto.Cipher;
import javax.crypto.KeyGenerator;
import javax.crypto.SecretKey;
import javax.crypto.spec.GCMParameterSpec;

public class CryptoHelper {
    private static final String KEY_ALIAS = "seafile_secure_key";
    private static final String ANDROID_KEYSTORE = "AndroidKeyStore";

    public static void generateKeyIfNecessary() throws Exception {
        KeyStore keyStore = KeyStore.getInstance(ANDROID_KEYSTORE);
        keyStore.load(null);

        if (!keyStore.containsAlias(KEY_ALIAS)) {
            KeyGenerator keyGenerator = KeyGenerator.getInstance(
                    KeyProperties.KEY_ALGORITHM_AES, ANDROID_KEYSTORE);
            KeyGenParameterSpec spec = new KeyGenParameterSpec.Builder(
                    KEY_ALIAS,
                    KeyProperties.PURPOSE_ENCRYPT | KeyProperties.PURPOSE_DECRYPT)
                    .setBlockModes(KeyProperties.BLOCK_MODE_GCM)
                    .setEncryptionPaddings(KeyProperties.ENCRYPTION_PADDING_NONE)
                    .build();
            keyGenerator.init(spec);
            keyGenerator.generateKey();
        }
    }

    private static SecretKey getSecretKey() throws Exception {
        KeyStore keyStore = KeyStore.getInstance(ANDROID_KEYSTORE);
        keyStore.load(null);
        return ((KeyStore.SecretKeyEntry) keyStore.getEntry(KEY_ALIAS, null)).getSecretKey();
    }

    public static byte[][] encrypt(String plainText) throws Exception {
        Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
        cipher.init(Cipher.ENCRYPT_MODE, getSecretKey());
        byte[] iv = cipher.getIV();
        byte[] encrypted = cipher.doFinal(plainText.getBytes(StandardCharsets.UTF_8));
        return new byte[][]{encrypted, iv};
    }

    public static String decrypt(byte[] encryptedData, byte[] iv) throws Exception {
        Cipher cipher = Cipher.getInstance("AES/GCM/NoPadding");
        GCMParameterSpec spec = new GCMParameterSpec(128, iv);
        cipher.init(Cipher.DECRYPT_MODE, getSecretKey(), spec);
        byte[] decrypted = cipher.doFinal(encryptedData);
        return new String(decrypted, StandardCharsets.UTF_8);
    }

    public static String encodeBase64(byte[] data) {
        return Base64.getEncoder().encodeToString(data);
    }

    public static byte[] decodeBase64(String encoded) {
        return Base64.getDecoder().decode(encoded);
    }
}
