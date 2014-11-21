package com.seafile.seadroid2.gesturelock;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.concurrent.atomic.AtomicBoolean;

import android.content.Context;
import android.os.FileObserver;
import android.util.Log;

/**
 * Gesture Lock encrypt, decrypt utility
 * 
 * @author way
 * 
 */
public class LockPatternUtils {
    private static final String TAG = "LockPatternUtils";
    private static final String LOCK_PATTERN_FILE = "gesture.key";
    /**
     * The minimum number of dots in a valid pattern.
     */
    public static final int MIN_LOCK_PATTERN_SIZE = 4;
    /**
     * The maximum number of incorrect attempts before the user is prevented
     * from trying again for {@link #FAILED_ATTEMPT_TIMEOUT_MS}.
     */
    public static final int FAILED_ATTEMPTS_BEFORE_TIMEOUT = 5;
    /**
     * The minimum number of dots the user must include in a wrong pattern
     * attempt for it to be counted against the counts that affect
     * {@link #FAILED_ATTEMPTS_BEFORE_TIMEOUT} and
     * {@link #FAILED_ATTEMPTS_BEFORE_RESET}
     */
    public static final int MIN_PATTERN_REGISTER_FAIL = MIN_LOCK_PATTERN_SIZE;
    /**
     * How long the user is prevented from trying again after entering the wrong
     * pattern too many times.
     */
    public static final long FAILED_ATTEMPT_TIMEOUT_MS = 30000L;

    private static File sLockPatternFilename;
    private static final AtomicBoolean sHaveNonZeroPatternFile = new AtomicBoolean(
            false);
    private static FileObserver sPasswordObserver;

    private static class LockPatternFileObserver extends FileObserver {
        public LockPatternFileObserver(String path, int mask) {
            super(path, mask);
        }

        @Override
        public void onEvent(int event, String path) {
            Log.d(TAG, "file path" + path);
            if (LOCK_PATTERN_FILE.equals(path)) {
                Log.d(TAG, "lock pattern file changed");
                sHaveNonZeroPatternFile.set(sLockPatternFilename.length() > 0);
            }
        }
    }

    public LockPatternUtils(Context context) {
        if (sLockPatternFilename == null) {
            String dataSystemDirectory = context.getFilesDir()
                    .getAbsolutePath();
            sLockPatternFilename = new File(dataSystemDirectory,
                    LOCK_PATTERN_FILE);
            sHaveNonZeroPatternFile.set(sLockPatternFilename.length() > 0);
            int fileObserverMask = FileObserver.CLOSE_WRITE
                    | FileObserver.DELETE | FileObserver.MOVED_TO
                    | FileObserver.CREATE;
            sPasswordObserver = new LockPatternFileObserver(
                    dataSystemDirectory, fileObserverMask);
            sPasswordObserver.startWatching();
        }
    }

    /**
     * Check to see if the user has stored a lock pattern.
     * 
     * @return Whether a saved pattern exists.
     */
    public boolean savedPatternExists() {
        return sHaveNonZeroPatternFile.get();
    }

    public void clearLock() {
        saveLockPattern(null);
    }

    /**
     * Deserialize a pattern.
     * 
     * @param string
     *            The pattern serialized with {@link #patternToString}
     * @return The pattern.
     */
    public static List<LockPatternView.Cell> stringToPattern(String string) {
        List<LockPatternView.Cell> result = new ArrayList<LockPatternView.Cell>();

        final byte[] bytes = string.getBytes();
        for (int i = 0; i < bytes.length; i++) {
            byte b = bytes[i];
            result.add(LockPatternView.Cell.of(b / 3, b % 3));
        }
        return result;
    }

    /**
     * Serialize a pattern. 
     * 
     * @param pattern
     *            The pattern.
     * @return The pattern in string form.
     */
    public static String patternToString(List<LockPatternView.Cell> pattern) {
        if (pattern == null) {
            return "";
        }
        final int patternSize = pattern.size();

        byte[] res = new byte[patternSize];
        for (int i = 0; i < patternSize; i++) {
            LockPatternView.Cell cell = pattern.get(i);
            res[i] = (byte) (cell.getRow() * 3 + cell.getColumn());
        }
        return new String(res);
    }

    /**
     * Save a lock pattern.
     * 
     * @param pattern
     *            The new pattern to save.
     * @param isFallback
     *            Specifies if this is a fallback to biometric weak
     */
    public void saveLockPattern(List<LockPatternView.Cell> pattern) {
        // Compute the hash
        final byte[] hash = LockPatternUtils.patternToHash(pattern);
        try {
            // Write the hash to file
            RandomAccessFile raf = new RandomAccessFile(sLockPatternFilename,
                    "rwd");
            // Truncate the file if pattern is null, to clear the lock
            if (pattern == null) {
                raf.setLength(0);
            } else {
                raf.write(hash, 0, hash.length);
            }
            raf.close();
        } catch (FileNotFoundException fnfe) {
            // Cant do much, unless we want to fail over to using the settings
            // provider
            Log.e(TAG, "Unable to save lock pattern to " + sLockPatternFilename);
        } catch (IOException ioe) {
            // Cant do much
            Log.e(TAG, "Unable to save lock pattern to " + sLockPatternFilename);
        }
    }

    /*
     * Generate an SHA-1 hash for the pattern. Not the most secure, but it is at
     * least a second level of protection. First level is that the file is in a
     * location only readable by the system process.
     * 
     * @param pattern the gesture pattern.
     * 
     * @return the hash of the pattern in a byte array.
     */
    private static byte[] patternToHash(List<LockPatternView.Cell> pattern) {
        if (pattern == null) {
            return null;
        }

        final int patternSize = pattern.size();
        byte[] res = new byte[patternSize];
        for (int i = 0; i < patternSize; i++) {
            LockPatternView.Cell cell = pattern.get(i);
            res[i] = (byte) (cell.getRow() * 3 + cell.getColumn());
        }
        try {
            MessageDigest md = MessageDigest.getInstance("SHA-1");
            byte[] hash = md.digest(res);
            return hash;
        } catch (NoSuchAlgorithmException nsa) {
            return res;
        }
    }

    /**
     * Check to see if a pattern matches the saved pattern. If no pattern
     * exists, always returns true.
     * 
     * @param pattern
     *            The pattern to check.
     * @return Whether the pattern matches the stored one.
     */
    public boolean checkPattern(List<LockPatternView.Cell> pattern) {
        try {
            // Read all the bytes from the file
            RandomAccessFile raf = new RandomAccessFile(sLockPatternFilename,
                    "r");
            final byte[] stored = new byte[(int) raf.length()];
            int got = raf.read(stored, 0, stored.length);
            raf.close();
            if (got <= 0) {
                return true;
            }
            // Compare the hash from the file with the entered pattern's hash
            return Arrays.equals(stored,
                    LockPatternUtils.patternToHash(pattern));
        } catch (FileNotFoundException fnfe) {
            return true;
        } catch (IOException ioe) {
            return true;
        }
    }
}
