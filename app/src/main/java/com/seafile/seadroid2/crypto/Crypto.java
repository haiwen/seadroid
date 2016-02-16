package com.seafile.seadroid2.crypto;

import android.util.Base64;
import android.util.Log;

import java.io.UnsupportedEncodingException;
import java.math.BigInteger;
import java.security.GeneralSecurityException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.KeySpec;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;

/**
 * Password strings cannot be used as symmetric encryption keys as is,
 * so some sort of key derivation is required.
 *
 * There are a few ways to derive keys, but most of them are not particularly secure.
 * To ensure encryption keys are both sufficiently random and hard to brute force, you should use standard PBE key derivation methods.
 * Of those, the one currently regarded secure and available on Android is PBKDF2WithHmacSHA1.
 */
public class Crypto {
    private static final String TAG = Crypto.class.getSimpleName();

    public static final String PKCS12_DERIVATION_ALGORITHM = "PBEWITHSHA256AND256BITAES-CBC-BC";
    public static final String PBKDF2_DERIVATION_ALGORITHM = "PBKDF2WithHmacSHA1";
    private static final String CIPHER_ALGORITHM = "AES/CBC/PKCS5Padding";

    private static String DELIMITER = "]";

    private static int KEY_LENGTH = 256;
    // minimum values recommended by PKCS#5, increase as necessary
    private static int ITERATION_COUNT = 1000;
    private static final int PKCS5_SALT_LENGTH = 8;

    private static SecureRandom random = new SecureRandom();

    private Crypto() {
    }

    /**
     * Generate a 32-byte long cryptographically strong random number.
     *
     * This will be used as the file encryption key ("file key").
     *
     * Note that the output of a SecureRandom instance should never be relied upon to be deterministic.
     * For deterministic output from a given input, see {@link MessageDigest} which provides one-way hash functions.
     * For deriving keys from passwords, see {@link SecretKeyFactory}
     *
     * Using the seeded constructor or calling setSeed(byte[]) may completely replace the cryptographically strong default seed
     * causing the instance to return a predictable sequence of numbers unfit for secure use.
     * Due to variations between implementations it is not recommended to use setSeed at all.
     *
     * @return
     */
    public static byte[] generateRadomNumbers() {
        SecureRandom sr = new SecureRandom();
        byte[] output = new byte[32];
        sr.nextBytes(output);
        return output;
    }

    /**
     * 由用户给的repo_id + 密码 生成一个 magic 字符串
     *
     * magic 是一段 32 字节的二进制数据，通过 PBKDF2 算法由“repo_id+password”产生。
     * 即 magic = PBKDF2(repo_id+password, salt, 1000, 32)，PBKDF2 循环次数为 1000次。
     * salt 在 C 语言中定义为一个8字节大小的数组：{ 0xda, 0x90, 0x45, 0xc3, 0x06, 0xc7, 0xcc, 0x26 }
     *
     * @param password
     * @return
     * @throws NoSuchAlgorithmException
     * @throws InvalidKeySpecException
     */
    private static String generateMagic(String password) throws NoSuchAlgorithmException, InvalidKeySpecException, UnsupportedEncodingException {
        int iterations = 1000;
        char[] chars = password.toCharArray();
        char[] slt = {0xda, 0x90, 0x45, 0xc3, 0x06, 0xc7, 0xcc, 0x26};
        final byte[] salt = new String(slt).getBytes("UTF-8");

        PBEKeySpec spec = new PBEKeySpec(chars, salt , iterations, 32);
        SecretKeyFactory skf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA1");
        byte[] hash = skf.generateSecret(spec).getEncoded();
        return iterations + ":" + toHex(salt) + ":" + toHex(hash);
    }

    private static String toHex(byte[] array) throws NoSuchAlgorithmException {
        BigInteger bi = new BigInteger(1, array);
        String hex = bi.toString(16);
        int paddingLength = (array.length * 2) - hex.length();
        if (paddingLength > 0) {
            return String.format("%0" + paddingLength + "d", 0) + hex;
        } else {
            return hex;
        }
    }

    private static boolean verifyRepoPassword(String input, String magic) throws NoSuchAlgorithmException, InvalidKeySpecException, UnsupportedEncodingException {
        final byte[] generateMagic = generateMagic(input).getBytes();

        int diff = generateMagic.length ^ magic.getBytes().length;
        for (int i = 0; i < generateMagic.length && i < magic.getBytes().length; i++) {
            diff |= generateMagic[i] ^ magic.getBytes()[i];
        }
        return diff == 0;
    }

    private static byte[] fromHex(String hex) throws NoSuchAlgorithmException {
        byte[] bytes = new byte[hex.length() / 2];
        for (int i = 0; i < bytes.length; i++) {
            bytes[i] = (byte) Integer.parseInt(hex.substring(2 * i, 2 * i + 2), 16);
        }
        return bytes;
    }

    /**
     * Encrypt the file key with the user provided password.
     *
     * We first use PBKDF2 algorithm (1000 iteratioins of SHA256) to derive a key/iv pair from the password,
     * then use AES 256/CBC to encrypt the file key.
     *
     * The result is called the "encrypted file key".
     * This encrypted file key will be sent to and stored on the server.
     * When you need to access the data, you can decrypt the file key from the encrypted file key.
     *
     *  The iteration count is as recommended by PKCS#5,
     *  but that standard was written a while ago, so you might want to increase it.
     *  For some perspective, AES 256 bit keys used to encrypt backups in Android 4.0 (ICS) are derived using 10,000 iterations and a 512 bit salt;
     *  iOS 4.0 also uses 10,000 iterations.
     *  The size of the salt should typically match the key size, for example 16 bytes when using a AES with a 128 bit key (128 / 8 = 16).
     * @param salt
     * @param password
     */
    public static SecretKey deriveKeyPbkdf2(byte[] salt, String password) {
        try {
            long start = System.currentTimeMillis();
            KeySpec keySpec = new PBEKeySpec(password.toCharArray(), salt, ITERATION_COUNT, KEY_LENGTH);
            SecretKeyFactory keyFactory = SecretKeyFactory.getInstance(PBKDF2_DERIVATION_ALGORITHM);
            byte[] keyBytes = keyFactory.generateSecret(keySpec).getEncoded();
            Log.d(TAG, "key bytes: " + toHex(keyBytes));

            SecretKey result = new SecretKeySpec(keyBytes, "AES");
            long elapsed = System.currentTimeMillis() - start;
            Log.d(TAG, String.format("PBKDF2 key derivation took %d [ms].", elapsed));

            return result;
        } catch (GeneralSecurityException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * All file data is encrypted by the file key with AES 256/CBC.
     *
     * We use PBKDF2 algorithm (1000 iterations of SHA256) to derive key/iv pair from the file key.
     * After encryption, the data is uploaded to the server.
     *
     * @param plaintext
     * @param key
     * @param salt
     * @return
     */
    public static String encrypt(String plaintext, SecretKey key, byte[] salt) {
        try {
            Cipher cipher = Cipher.getInstance(CIPHER_ALGORITHM);

            byte[] iv = generateIv(cipher.getBlockSize());
            Log.d(TAG, "IV: " + toHex(iv));
            IvParameterSpec ivParams = new IvParameterSpec(iv);
            cipher.init(Cipher.ENCRYPT_MODE, key, ivParams);
            Log.d(TAG, "Cipher IV: " + (cipher.getIV() == null ? null : toHex(cipher.getIV())));

            byte[] cipherText = cipher.doFinal(plaintext.getBytes("UTF-8"));

            if (salt != null) {
                return String.format("%s%s%s%s%s", toBase64(salt), DELIMITER,
                        toBase64(iv), DELIMITER, toBase64(cipherText));
            }

            return String.format("%s%s%s", toBase64(iv), DELIMITER,
                    toBase64(cipherText));
        } catch (GeneralSecurityException e) {
            throw new RuntimeException(e);
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    public static byte[] generateIv(int length) {
        byte[] b = new byte[length];
        random.nextBytes(b);

        return b;
    }

    public static String decryptPbkdf2(String ciphertext, String password) {
        String[] fields = ciphertext.split(DELIMITER);
        if (fields.length != 3) {
            throw new IllegalArgumentException("Invalid encypted text format");
        }

        byte[] salt = fromBase64(fields[0]);
        byte[] iv = fromBase64(fields[1]);
        byte[] cipherBytes = fromBase64(fields[2]);
        SecretKey key = deriveKeyPbkdf2(salt, password);

        return decrypt(cipherBytes, key, iv);
    }

    public static String decrypt(byte[] cipherBytes, SecretKey key, byte[] iv) {
        try {
            Cipher cipher = Cipher.getInstance(CIPHER_ALGORITHM);
            IvParameterSpec ivParams = new IvParameterSpec(iv);
            cipher.init(Cipher.DECRYPT_MODE, key, ivParams);
            Log.d(TAG, "Cipher IV: " + toHex(cipher.getIV()));
            byte[] plaintext = cipher.doFinal(cipherBytes);
            String plainrStr = new String(plaintext, "UTF-8");

            return plainrStr;
        } catch (GeneralSecurityException e) {
            throw new RuntimeException(e);
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }


    /*public static String toHex(byte[] bytes) {
        StringBuffer buff = new StringBuffer();
        for (byte b : bytes) {
            buff.append(String.format("%02X", b));
        }

        return buff.toString();
    }*/

    public static String toBase64(byte[] bytes) {
        return Base64.encodeToString(bytes, Base64.NO_WRAP);
    }

    public static byte[] fromBase64(String base64) {
        return Base64.decode(base64, Base64.NO_WRAP);
    }
}
