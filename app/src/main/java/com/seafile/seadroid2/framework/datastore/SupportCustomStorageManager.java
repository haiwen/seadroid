package com.seafile.seadroid2.framework.datastore;

import android.content.Context;
import android.media.MediaScannerConnection;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.os.StatFs;
import android.os.storage.StorageVolume;
import android.text.TextUtils;
import android.util.Pair;

import androidx.core.os.EnvironmentCompat;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.lang.reflect.Method;
import java.util.Collection;
import java.util.List;


//old
/**
 * This class decides where to store Seadroid's data in the file system.
 * <p>
 * Up till API 18, there was not much choice. Only CLASSIC_LOCATION did make any sense
 * to use. Starting with KitKat new options have appeared. API 19+ offers an API to get a list
 * of possible storage locations, which will include things like SD cards or USB connected flash
 * drives.
 * <p>
 * This StorageManager allows the user to make the choice where to store its data. It remembers
 * this choice in the SettingsManager.
 * <p>
 * This StorageManager also does an auto-selection of the best storage on first use of Seadroid.
 * <p>
 * The following data falls into the scope of this StorageManager:
 * <p>
 * - Downloaded repository files
 * -> Indexed by Gallery
 * -> synced server content, might be deleted locally
 * -> persistent, as deletion would cause user inconvenience
 * <p>
 * - Temp files (e.g. pending for upload, download in progress)
 * -> NOT indexed by Gallery
 * -> important content, not to be deleted prematurely
 * -> only temporary
 * -> might be moved to "Downloaded repository files", so should be same mount point
 * <p>
 * - Image thumbnails
 * -> NOT indexed by Gallery
 * -> long term storage
 * -> not important content, can be deleted if necessary
 * <p>
 * - JSON Cache files
 * -> NOT indexed by Gallery
 * -> long term storage
 * -> not important content, can be deleted if necessary
 * <p>
 * This class offers a set of methods to retrieve the base dir for specific types of data.
 * <p>
 */

//new

/**
 * <h2>Technical background</h2>
 * The getContext().getExternalMediaDirs() function returns the list of paths ending in the "/Android/media/" suffix of all memory cards in the phone
 * <code>
 * for example:
 * /storage/emulated/0/Android/media/package
 * /storage/emulated/1/Android/media/package
 * /storage/emulated/2/Android/media/package
 * ...
 * (Fake data)
 * </code>
 *
 * <p>
 * In the new version, v3.0.18, the app allows users to customize the storage location.
 * so, if the user changed location, the "media" path is no longer used as the storage location by default.
 * </p>
 *
 * <p>
 * If there are multiple memory cards in the phone, getDefaultMedia will have multiple media paths
 * Useful links:
 * - https://developer.android.com/guide/topics/data/data-storage.html
 */
public class SupportCustomStorageManager implements MediaScannerConnection.OnScanCompletedListener {

    protected static final String DEBUG_TAG = "StorageManager";

    private static SupportCustomStorageManager instance = null;

    private final Location CLASSIC_LOCATION;

    public SupportCustomStorageManager() {
        CLASSIC_LOCATION = buildDefaultLocation();
    }

    public static void resetInstance(){
        instance = new SupportCustomStorageManager();
    }

    public static SupportCustomStorageManager getInstance() {
        if (instance == null) {
            instance = new SupportCustomStorageManager();
        }
        return instance;
    }

    public Location getStorageLocation() {
        return CLASSIC_LOCATION;
    }

    private Location buildDefaultLocation() {
        Location defaultLoc = new Location();

        String customMediaDir = AppDataManager.getCustomStorageDir();
        // /storage/emulated/0/Android/media/package/Seafile/
        if (TextUtils.isEmpty(customMediaDir) || !com.seafile.seadroid2.framework.util.FileUtils.isAvailable(new File(customMediaDir))) {
            defaultLoc.id = -1; // Android IDs start at 0. so "-1" is safe for us

            File[] externalMediaDirs = getDefaultMediaCacheDirs();
            String rootPath = externalMediaDirs[0].getAbsolutePath();
            defaultLoc.mediaPath = new File(rootPath + "/Seafile/");
        } else {
            defaultLoc.id = 1;
            defaultLoc.mediaPath = new File(Utils.pathJoin(customMediaDir, "/Seafile/"));
        }

        // /storage/emulated/0/Android/data/package/cache
        File[] externalCacheDirs = getDefaultAppCacheDir();
        String appCachePath = externalCacheDirs[0].getAbsolutePath();
        defaultLoc.cachePath = new File(appCachePath);

        fillLocationInfo(defaultLoc);
        return defaultLoc;
    }

    /**
     * fill description info
     */
    private void fillLocationInfo(Location loc) {
        loc.available = loc.mediaPath != null && EnvironmentCompat.getStorageState(loc.mediaPath).equals(Environment.MEDIA_MOUNTED);

        //
        if (loc.mediaPath == null) {
            return;
        }

        Pair<String, String> prettyVolumeName = getPrettyVolumeName(loc.mediaPath.getAbsolutePath());
        if (prettyVolumeName != null) {
            loc.volume = prettyVolumeName.first;
            loc.label = prettyVolumeName.second;
        }
    }


    public Pair<String, String> getPrettyVolumeName(String volumePath) {
        android.os.storage.StorageManager storageManager = (android.os.storage.StorageManager) getContext().getSystemService(Context.STORAGE_SERVICE);
        if (storageManager == null) {
            return null;
        }

        List<StorageVolume> volumes = storageManager.getStorageVolumes();
        for (StorageVolume volume : volumes) {
            try {
                File dir;

                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                    dir = volume.getDirectory();
                } else {
                    // Before API 30 way to get a volume path
                    Method getPathMethod = StorageVolume.class.getDeclaredMethod("getPathFile");
                    dir = (File) getPathMethod.invoke(volume);
                }

                if (dir == null) {
                    continue;
                }

                if (!volumePath.startsWith(dir.getAbsolutePath())) {
                    continue;
                }

                String label = volume.getDescription(getContext());
                String name = dir.getAbsolutePath();
                return new Pair<>(name, label); // e.g. "Internal storage" or "SD card"
            } catch (Exception e) {
                // Will fallback to volumePath
            }
        }

        return null;
    }


    /**
     * Return the base directory for media storage to be used in Seadroid.
     * <p>
     * It guaranties to always return a valid directory.
     * However, this directory might change at runtime, So it should never be cached.
     *
     * @return the base directory for media storage to be used in Seadroid.
     */
    public final File getMediaDir() {
        return getDirectoryCreateIfNeeded(getStorageLocation().mediaPath);
    }

    private File getDirectoryCreateIfNeeded(File dir) {
        if (dir.exists()) {
            return dir;
        } else {
            dir.mkdirs();
        }
        return dir;
    }

    protected final Context getContext() {
        return SeadroidApplication.getAppContext();
    }

    /**
     * Callback when a file has been added/removed by us in the Android Media Store
     *
     * @param path
     * @param uri
     */
    public void onScanCompleted(String path, Uri uri) {
    }

    /**
     * Store temp files in a subdirectory below the media directory.
     * <p>
     * This should be a subdirectory of getMediaDir() so that temp files
     * can be efficiently moved into the media storage.
     *
     * @return base of where to store temp files
     */
    public final File getTempDir() {
        File base = getStorageLocation().cachePath;
        File tmpDir = new File(base, ".temp");
        return getDirectoryCreateIfNeeded(tmpDir);
    }

    public final File getGlideCacheDir() {
        File base = getStorageLocation().cachePath;
        File f = new File(base, "glide");
        return getDirectoryCreateIfNeeded(f);
    }

    public final File getTakeCameraDir() {
        File base = getStorageLocation().cachePath;
        File f = new File(base, "camera");
        return getDirectoryCreateIfNeeded(f);
    }

    /**
     * Store JSON cache files in private internal cache.
     * <p>
     * This cache directory will contain json files listing the repositories and directory listings.
     * this can be pretty private (especially the repository listing). So these should not be readable
     * by other apps. Therefore we save them in internal storage, where only Seadroid has access to.
     *
     * @return base of where to store JSON cache files
     */
    public final File getJsonCacheDir() {
        File base = getContext().getCacheDir();
        return getDirectoryCreateIfNeeded(base);
    }

    /**
     * Deletes full cache
     * remember to clear cache from database after called this method
     */
    public final void clearMedia() {
        Collection<File> fileList = FileUtils.listFiles(getMediaDir(), null, true);

        FileUtils.deleteQuietly(getMediaDir());

        notifyAndroidGalleryDirectoryChange(fileList);
    }

    /**
     * Deletes full cache
     * remember to clear cache from database after called this method
     */
    public final void clearAllCache() {
        Collection<File> fileList = FileUtils.listFiles(getMediaDir(), null, true);

        FileUtils.deleteQuietly(getMediaDir());
        FileUtils.deleteQuietly(getTakeCameraDir());
        FileUtils.deleteQuietly(getGlideCacheDir());
        FileUtils.deleteQuietly(getJsonCacheDir());
        FileUtils.deleteQuietly(getTempDir());

        notifyAndroidGalleryDirectoryChange(fileList);
    }


    /**
     * A directory was added, changed or removed. Notify the gallery.
     *
     * @param fileList
     */
    private void notifyAndroidGalleryDirectoryChange(Collection<File> fileList) {

        int count = 0;
        String[] list = new String[fileList.size()];
        for (File f : fileList) {
            list[count++] = f.getAbsolutePath();
        }

        MediaScannerConnection.scanFile(getContext(), list, null, this);
    }

    /**
     * Get the media directories offered by Android.
     *
     * @return
     */
    protected File[] getDefaultMediaCacheDirs() {
        return getContext().getExternalMediaDirs();
    }

    /**
     * Get the cache directories offered by Android.
     *
     * @return
     */
    protected File[] getDefaultAppCacheDir() {
        return getContext().getExternalCacheDirs();
    }

    /**
     * Get partition size of the mount point containing dir
     *
     * @param dir
     * @return
     */
    protected long getStorageSize(File dir) {
        StatFs stat = new StatFs(dir.getParentFile().getAbsolutePath());
        return stat.getTotalBytes();
    }

    /**
     * Get free size of the mount point containing dir
     *
     * @param dir
     * @return
     */
    protected long getStorageFreeSpace(File dir) {
        try {
            StatFs stat = new StatFs(dir.getParentFile().getAbsolutePath());
            return stat.getAvailableBytes();
        } catch (Exception e) {
            e.printStackTrace();
        }
        return 0;
    }

    /**
     * Return space used by Seadroid
     *
     * @return
     */
    public final long getUsedSpace() {
        File mediaDir = getMediaDir();

        if (!mediaDir.exists()) return 0L;

        return FileUtils.sizeOfDirectory(mediaDir);
    }

    public static class Location {
        /**
         * Our internal ID of this storage.
         */
        public int id;

        public String label;

        public String volume;

        /**
         * Base media directory
         */
        public File mediaPath;

        /**
         * Base cache directory
         */
        public File cachePath;

        /**
         * Store photos taken by the camera
         */
        public File externalImagePath;

        /**
         * Store videos taken by the camera
         */
        public File externalVideoPath;

        /**
         * Text description
         */
        @Deprecated
        public String description;

        /**
         * Is this location available (mounted)?
         */
        public boolean available;

        /**
         * Is this the currently selected location?
         */
        @Deprecated
        public boolean selected;
    }
}
