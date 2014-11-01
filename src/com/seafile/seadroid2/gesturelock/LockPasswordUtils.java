package com.seafile.seadroid2.gesturelock;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.concurrent.atomic.AtomicBoolean;

import android.app.admin.DevicePolicyManager;
import android.content.Context;
import android.content.SharedPreferences;
import android.content.SharedPreferences.Editor;
import android.os.FileObserver;
import android.util.Log;

public class LockPasswordUtils {
    private static final String TAG = "LockPasswordUtils";
    private final static String LOCK_PASSWORD_SALT_FILE = "password_salt";
    private final static String LOCK_PASSWORD_SALT_KEY = "lockscreen.password_salt";
    private static final String LOCK_PASSWORD_FILE = "password.key";
    private static SharedPreferences mSharedPreferences;
    private static Editor mEditor;
    private static File sLockPasswordFilename;
    private static final AtomicBoolean sHaveNonZeroPasswordFile = new AtomicBoolean(
            false);
    private static FileObserver sPasswordObserver;

    private static class PasswordFileObserver extends FileObserver {
        public PasswordFileObserver(String path, int mask) {
            super(path, mask);
        }

        @Override
        public void onEvent(int event, String path) {
            if (LOCK_PASSWORD_FILE.equals(path)) {
                Log.d(TAG, "lock password file changed");
                sHaveNonZeroPasswordFile
                        .set(sLockPasswordFilename.length() > 0);
            }
        }
    }

    public LockPasswordUtils(Context context) {
        mSharedPreferences = context.getSharedPreferences(
                LOCK_PASSWORD_SALT_FILE, Context.MODE_PRIVATE);
        mEditor = mSharedPreferences.edit();
        if (sLockPasswordFilename == null) {
            String dataSystemDirectory = context.getCacheDir()
                    .getAbsolutePath();
            sLockPasswordFilename = new File(dataSystemDirectory,
                    LOCK_PASSWORD_FILE);
            sHaveNonZeroPasswordFile.set(sLockPasswordFilename.length() > 0);
            int fileObserverMask = FileObserver.CLOSE_WRITE
                    | FileObserver.DELETE | FileObserver.MOVED_TO
                    | FileObserver.CREATE;
            sPasswordObserver = new PasswordFileObserver(dataSystemDirectory,
                    fileObserverMask);
            sPasswordObserver.startWatching();
        }
    }

    /**
     * Check to see if the user has stored a lock pattern.
     * 
     * @return Whether a saved pattern exists.
     */
    public static boolean savedPasswordExists() {
        return sHaveNonZeroPasswordFile.get();
    }

    /**
     * Compute the password quality from the given password string.
     */
    public static int computePasswordQuality(String password) {
        boolean hasDigit = false;
        boolean hasNonDigit = false;
        final int len = password.length();
        for (int i = 0; i < len; i++) {
            if (Character.isDigit(password.charAt(i))) {
                hasDigit = true;
            } else {
                hasNonDigit = true;
            }
        }

        if (hasNonDigit && hasDigit) {
            return DevicePolicyManager.PASSWORD_QUALITY_ALPHANUMERIC;
        }
        if (hasNonDigit) {
            return DevicePolicyManager.PASSWORD_QUALITY_ALPHABETIC;
        }
        if (hasDigit) {
            return DevicePolicyManager.PASSWORD_QUALITY_NUMERIC;
        }
        return DevicePolicyManager.PASSWORD_QUALITY_UNSPECIFIED;
    }

    /**
     * Save a lock password. Does not ensure that the password is as good as the
     * requested mode, but will adjust the mode to be as good as the pattern.
     * 
     * @param password
     *            The password to save
     * @param quality
     *            {@see 
     *            DevicePolicyManager#getPasswordQuality(android.content.ComponentName
     *            )}
     * @param isFallback
     *            Specifies if this is a fallback to biometric weak
     */
    public static void saveLockPassword(String password, int quality,
            boolean isFallback) {
        // Compute the hash
        final byte[] hash = passwordToHash(password);
        try {
            // Write the hash to file
            RandomAccessFile raf = new RandomAccessFile(sLockPasswordFilename,
                    "rwd");
            // Truncate the file if pattern is null, to clear the lo
            try {
                if (password == null) {
                    raf.setLength(0);
                } else {
                    raf.write(hash, 0, hash.length);
                }
            } finally {
                if (raf != null)
                    raf.close();
            }
        } catch (FileNotFoundException fnfe) {
            // Cant do much, unless we want to fail over to using the settings
            // provider
            Log.e(TAG, "Unable to save lock pattern to "
                    + sLockPasswordFilename);
        } catch (IOException ioe) {
            // Cant do much
            Log.e(TAG, "Unable to save lock pattern to "
                    + sLockPasswordFilename);
        }
    }

    /**
     * Check to see if a password matches the saved password. If no password
     * exists, always returns true.
     * 
     * @param password
     *            The password to check.
     * @return Whether the password matches the stored one.
     */
    public static boolean checkPassword(String password) {
        try {
            // Read all the bytes from the file
            RandomAccessFile raf = new RandomAccessFile(sLockPasswordFilename,
                    "r");
            final byte[] stored = new byte[(int) raf.length()];
            int got = raf.read(stored, 0, stored.length);
            raf.close();
            if (got <= 0) {
                return true;
            }
            // Compare the hash from the file with the entered password's hash
            return Arrays.equals(stored, passwordToHash(password));
        } catch (FileNotFoundException fnfe) {
            return true;
        } catch (IOException ioe) {
            return true;
        }
    }

    /*
     * Generate a hash for the given password. To avoid brute force attacks, we
     * use a salted hash. Not the most secure, but it is at least a second level
     * of protection. First level is that the file is in a location only
     * readable by the system process.
     * 
     * @param password the gesture pattern.
     * 
     * @return the hash of the pattern in a byte array.
     */
    public static byte[] passwordToHash(String password) {
        if (password == null) {
            return null;
        }
        String algo = null;
        byte[] hashed = null;
        try {
            byte[] saltedPassword = (password + getSalt()).getBytes();
            byte[] sha1 = MessageDigest.getInstance(algo = "SHA-1").digest(
                    saltedPassword);
            byte[] md5 = MessageDigest.getInstance(algo = "MD5").digest(
                    saltedPassword);
            hashed = (toHex(sha1) + toHex(md5)).getBytes();
        } catch (NoSuchAlgorithmException e) {
            Log.w(TAG, "Failed to encode string because of missing algorithm: "
                    + algo);
        }
        return hashed;
    }

    private static String getSalt() {
        long salt = getLong(LOCK_PASSWORD_SALT_KEY, 0);
        if (salt == 0) {
            try {
                salt = SecureRandom.getInstance("SHA1PRNG").nextLong();
                setLong(LOCK_PASSWORD_SALT_KEY, salt);
                Log.v(TAG, "Initialized lock password salt");
            } catch (NoSuchAlgorithmException e) {
                // Throw an exception rather than storing a password we'll never
                // be able to recover
                throw new IllegalStateException(
                        "Couldn't get SecureRandom number", e);
            }
        }
        return Long.toHexString(salt);
    }

    private static String toHex(byte[] ary) {
        final String hex = "0123456789ABCDEF";
        String ret = "";
        for (int i = 0; i < ary.length; i++) {
            ret += hex.charAt((ary[i] >> 4) & 0xf);
            ret += hex.charAt(ary[i] & 0xf);
        }
        return ret;
    }

    private static long getLong(String secureSettingKey, long def) {
        return mSharedPreferences.getLong(LOCK_PASSWORD_SALT_KEY, def);
    }

    private static void setLong(String secureSettingKey, long value) {
        mEditor.putLong(LOCK_PASSWORD_SALT_KEY, value);
        mEditor.commit();
    }
}
