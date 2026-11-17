package com.seafile.seadroid2.framework.datastore;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.db.AppDatabase;
import com.seafile.seadroid2.framework.db.entities.FileCacheStatusEntity;
import com.seafile.seadroid2.framework.service.BackupThreadExecutor;
import com.seafile.seadroid2.framework.util.SafeLogs;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.filefilter.TrueFileFilter;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

/**
 * Moves the downloaded files between media dirs and keeps the cache records in sync.
 * <p>
 * The media dir in use is {@link StorageManager#getMediaDir()}. When the custom media dir is
 * set but unavailable (permission revoked, sd card removed), files are downloaded to the
 * fallback dir ({@link StorageManager#getSelectedVolumeLocation()}). They are moved into the
 * custom media dir by {@link #reconcileFallback()} when it is available again.
 * <p>
 * Must not be called on the main thread.
 */
public final class MediaDirMigrator {
    private static final String TAG = "MediaDirMigrator";

    private static final long DOWNLOAD_STOP_TIMEOUT_MILLIS = 30_000L;

    private static final Object LOCK = new Object();

    private MediaDirMigrator() {
    }

    /**
     * Use a new media dir.
     *
     * @param newCustomDir the dir selected by user, empty means the default location
     * @param moveFiles    false to switch without moving the files of the current custom media dir,
     *                     only for the case that it is unavailable
     */
    public static void changeMediaDir(String newCustomDir, boolean moveFiles) throws Exception {
        synchronized (LOCK) {
            StorageManager storageManager = StorageManager.getInstance();
            if (moveFiles && storageManager.isCustomMediaDirUnavailable()) {
                throw new IllegalStateException("the current media dir is unavailable");
            }

            File configuredDir = storageManager.getConfiguredMediaDir();
            File fallbackDir = storageManager.getSelectedVolumeLocation().mediaPath;
            File newDir = TextUtils.isEmpty(newCustomDir)
                    ? fallbackDir
                    : StorageManager.getCustomMediaDir(newCustomDir);

            List<File> sourceDirs = new ArrayList<>();
            if (moveFiles) {
                sourceDirs.add(configuredDir);
            }
            // files downloaded while the custom media dir was unavailable
            if (!isSameDir(fallbackDir, newDir) && !containsDir(sourceDirs, fallbackDir)) {
                sourceDirs.add(fallbackDir);
            }
            sourceDirs.removeIf(dir -> isSameDir(dir, newDir));

            // the downloader must not write into the old dir while it is moved
            BackupThreadExecutor.getInstance().suspendDownloadAndWait(DOWNLOAD_STOP_TIMEOUT_MILLIS);
            try {
                for (File sourceDir : sourceDirs) {
                    mergeInto(sourceDir, newDir);
                }

                AppDataManager.writeCustomStorageDir(newCustomDir);
                StorageManager.resetInstance();
            } finally {
                BackupThreadExecutor.getInstance().resumeDownload();
            }
        }
    }

    /**
     * Move the files downloaded to the fallback dir while the custom media dir was unavailable
     * into the custom media dir.
     *
     * @return true if any file is moved
     */
    public static boolean reconcileFallback() throws Exception {
        synchronized (LOCK) {
            StorageManager storageManager = StorageManager.getInstance();
            if (!storageManager.isUsingCustomMediaDir() || storageManager.isCustomMediaDirUnavailable()) {
                return false;
            }

            File fallbackDir = storageManager.getSelectedVolumeLocation().mediaPath;
            if (!hasFiles(fallbackDir)) {
                return false;
            }

            File customDir = storageManager.getConfiguredMediaDir();
            SafeLogs.d(TAG, "reconcileFallback()", "move " + fallbackDir + " -> " + customDir);

            BackupThreadExecutor.getInstance().suspendDownloadAndWait(DOWNLOAD_STOP_TIMEOUT_MILLIS);
            try {
                mergeInto(fallbackDir, customDir);
            } finally {
                BackupThreadExecutor.getInstance().resumeDownload();
            }
            return true;
        }
    }

    /**
     * Copy all files of srcDir into dstDir, update the cache records, then delete srcDir.
     * If a file exists in both dirs, the newer one is kept.
     */
    private static void mergeInto(File srcDir, File dstDir) throws IOException {
        if (!srcDir.exists()) {
            return;
        }

        String srcPrefix = withSlash(srcDir);
        String dstPrefix = withSlash(dstDir);
        SafeLogs.d(TAG, "mergeInto()", srcPrefix + " -> " + dstPrefix);

        Collection<File> files = FileUtils.listFiles(srcDir, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE);
        for (File file : files) {
            File target = new File(dstDir, file.getAbsolutePath().substring(srcPrefix.length()));
            if (target.exists() && target.lastModified() > file.lastModified()) {
                continue;
            }
            FileUtils.copyFile(file, target, true);
        }

        // all files are copied, the records can point to the new dir before the old one is deleted
        migrateCachePaths(srcPrefix, dstPrefix);

        StorageManager.getInstance().deleteMediaDir(srcDir);
    }

    private static void migrateCachePaths(String oldPrefix, String newPrefix) {
        int pageSize = 500;
        int offset = 0;
        List<FileCacheStatusEntity> page;

        do {
            page = AppDatabase.getInstance().fileCacheStatusDAO().getPaged(pageSize, offset);
            List<FileCacheStatusEntity> toUpdate = new ArrayList<>();

            for (FileCacheStatusEntity entity : page) {
                if (entity.target_path != null && entity.target_path.startsWith(oldPrefix)) {
                    String relative = entity.target_path.substring(oldPrefix.length());
                    entity.target_path = Utils.pathJoin(newPrefix, relative);
                    toUpdate.add(entity);
                }
            }

            if (!toUpdate.isEmpty()) {
                AppDatabase.getInstance().fileCacheStatusDAO().updateAll(toUpdate);
            }

            offset += pageSize;
        } while (page.size() == pageSize); // the last page will < pageSize
    }

    private static boolean hasFiles(File dir) {
        if (!dir.isDirectory()) {
            return false;
        }
        Iterator<File> iterator = FileUtils.iterateFiles(dir, TrueFileFilter.INSTANCE, TrueFileFilter.INSTANCE);
        return iterator.hasNext();
    }

    private static boolean isSameDir(File a, File b) {
        return TextUtils.equals(withSlash(a), withSlash(b));
    }

    private static boolean containsDir(List<File> dirs, File dir) {
        for (File d : dirs) {
            if (isSameDir(d, dir)) {
                return true;
            }
        }
        return false;
    }

    private static String withSlash(File dir) {
        return Utils.pathJoin(dir.getAbsolutePath(), "/");
    }
}
