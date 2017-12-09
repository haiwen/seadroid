package com.seafile.seadroid2.data;

import android.content.Context;
import android.media.MediaScannerConnection;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.support.v4.os.EnvironmentCompat;
import android.text.format.Formatter;
import android.util.Log;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;

import org.apache.commons.io.FileUtils;

import java.io.File;
import java.util.ArrayList;
import java.util.Collection;

/**
 * This class decides where to store Seadroid's data in the file system.
 *
 * Up till API 18, there was not much choice. Only CLASSIC_LOCATION did make any sense
 * to use. Starting with KitKat new options have appeared. API 19+ offers an API to get a list
 * of possible storage locations, which will include things like SD cards or USB connected flash
 * drives.
 *
 * This StorageManager allows the user to make the choice where to store its data. It remembers
 * this choice in the SettingsManager.
 *
 * This StorageManager also does an auto-selection of the best storage on first use of Seadroid.
 *
 * The following data falls into the scope of this StorageManager:
 *
 * - Downloaded repository files
 *     -> Indexed by Gallery
 *     -> synced server content, might be deleted locally
 *     -> persistent, as deletion would cause user inconvenience
 *
 * - Temp files (e.g. pending for upload, download in progress)
 *     -> NOT indexed by Gallery
 *     -> important content, not to be deleted prematurely
 *     -> only temporary
 *     -> might be moved to "Downloaded repository files", so should be same mount point
 *
 * - Image thumbnails
 *     -> NOT indexed by Gallery
 *     -> long term storage
 *     -> not important content, can be deleted if necessary
 *
 * - JSON Cache files
 *     -> NOT indexed by Gallery
 *     -> long term storage
 *     -> not important content, can be deleted if necessary
 *
 * This class offers a set of methods to retrieve the base dir for specific types of data.
 *
 * Useful links:
 * - https://developer.android.com/guide/topics/data/data-storage.html
 */
public abstract class StorageManager implements MediaScannerConnection.OnScanCompletedListener {

    protected static final String DEBUG_TAG = "StorageManager";

    private static StorageManager instance = null;

    private final Location CLASSIC_LOCATION;

    public StorageManager() {
        CLASSIC_LOCATION = buildClassicLocation();
    }

    /**
     * Fetch instance of the StorageManager
     * @return
     */
    public final static StorageManager getInstance() {
        if (instance == null) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                instance = new StorageManagerLollipop();
            } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                instance = new StorageManagerKitKat();
            } else {
                instance = new StorageManagerGingerbread();
            }
        }
        return instance;
    }

    private Location buildClassicLocation() {
        Location classic = new Location();
        classic.id = -1; // Android IDs start at 0. so "-1" is safe for us
        classic.mediaPath = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + "/Seafile/");
        classic.cachePath = new File(classic.mediaPath, "cache");
        fillLocationInfo(classic);
        return classic;
    }

    private void fillLocationInfo(Location loc) {
        loc.available = loc.mediaPath != null && EnvironmentCompat.getStorageState(loc.mediaPath).equals(Environment.MEDIA_MOUNTED);

        String label;
        // labels "primary/secondary" are as defined by https://possiblemobile.com/2014/03/android-external-storage/
        if (loc.id <= 0) {
            label = getContext().getString(R.string.storage_manager_primary_storage);
        } else {
            label = getContext().getString(R.string.storage_manager_secondary_storage);
        }
        if (loc.available) {
            loc.description = getContext().getString(R.string.storage_manager_storage_description,
                            label,
                            Formatter.formatFileSize(getContext(), getStorageFreeSpace(loc.mediaPath)),
                            Formatter.formatFileSize(getContext(), getStorageSize(loc.mediaPath)));
        } else {
            loc.description = getContext().getString(R.string.storage_manager_storage_description_not_available,
                            label);
        }
    }

    /**
     * Get the media directories offered by Android.
     *
     * @return
     */
    protected abstract File[] getSystemMediaDirs();

    /**
     * Get the cache directories offered by Android.
     *
     * @return
     */
    protected abstract File[] getSystemCacheDirs();

    /**
     * Get partition size of the mount point containing dir
     *
     * @param dir
     * @return
     */
    protected abstract long getStorageSize(File dir);

    /**
     * Get free size of the mount point containing dir
     *
     * @param dir
     * @return
     */
    protected abstract long getStorageFreeSpace(File dir);

    /**
     * Allows this device multiple storage locations?
     * @return
     */
    public abstract boolean supportsMultipleStorageLocations();

    /**
     * Callback when a file has been added/removed by us in the Android Media Store
     *
     * @param path
     * @param uri
     */
    public void onScanCompleted(String path, Uri uri) {
    }

    public final ArrayList<Location> getStorageLocations() {
        ArrayList<Location> retList = new ArrayList<>();

        int selectedDir = SettingsManager.instance().getStorageDir();

        retList.add(CLASSIC_LOCATION);

        File[] dirs = getSystemMediaDirs();
        File[] cacheDirs = getSystemCacheDirs();
        for (int i = 0; i < dirs.length; i++) {

            // omit the mount point where CLASSIC_LOCATION lies (would be duplicate)
            if (i == 0)
                continue;

            Location location = new Location();
            location.id = i;
            location.mediaPath = dirs[i];
            location.cachePath = cacheDirs[i];
            retList.add(location);
        }

        // add current selection at the end, even if the storage is currently unavailable
        if (selectedDir != CLASSIC_LOCATION.id && selectedDir >= dirs.length) {
            Location location = new Location();
            location.id = selectedDir;
            location.mediaPath = null;
            location.cachePath = null;
            retList.add(location);
        }

        for (Location loc: retList) {
            loc.currentSelection = (loc.id == selectedDir);
            fillLocationInfo(loc); // fill in size & description info
        }

        return retList;
    }

    /**
     * Set the new storage directory.
     *
     * This will change the settings and move files from the old to the new location.
     * Therefore, this method might take a while to finish.
     *
     * @param id the ID of the new storage location
     */
    public final void setStorageDir(int id) {

        int oldID = SettingsManager.instance().getStorageDir();

        if (oldID == id)
            return;

        Location newLocation = lookupStorageLocation(id);

        File newMediaDir = newLocation.mediaPath;

        if (newLocation.mediaPath == null || !EnvironmentCompat.getStorageState(newMediaDir).equals(Environment.MEDIA_MOUNTED)) {
            Log.i(DEBUG_TAG, "Selected storage dir is unavailable! " + newMediaDir);
            return;
        }

        Location oldLocation = lookupStorageLocation(oldID);
        if (oldLocation != null) {
            AccountManager manager = new AccountManager(getContext());

            try {
                // move cached files from old location to new location (might take a while)

                for (Account account: manager.getAccountList()) {
                    DataManager dataManager = new DataManager(account);
                    File oldAccountDir = new File(dataManager.getAccountDir());

                    if (oldAccountDir.isDirectory()) {
                        FileUtils.copyDirectoryToDirectory(oldAccountDir, newMediaDir);
                    }
                }
                notifyAndroidGalleryDirectoryChange(FileUtils.listFiles(newMediaDir, null, true));

            } catch (Exception e) {
                Log.e(DEBUG_TAG, "Could not move cache to new location", e);
                return;
            }

            // remove everything in the old cache directories (thumbnails, etc).
            clearCache();
        }

        Log.i(DEBUG_TAG, "Setting storage directory to " + newMediaDir);
        SettingsManager.instance().setStorageDir(id);
    }

    /**
     * Decide, which storage to use. This will be called only once,
     * on first start of Seadroid. After that, it's read from the Settings.
     *
     *  -> API 19+
     *  It selects the media with the most free space in it.
     *
     *  -> API 1-18
     *  On pre-KitKat, only CLASSIC_LOCATION is available. So there is no choice.
     *
     * This method does not change the Settings. It just evaluates what the best storage location
     * might be.
     *
     * @return storage ID with the most free space
    */
    private Location getPreferredStorage() {

        /* Backwards compatibility on upgrade:
         * If there is already CLASSIC_LOCATION present on the system, prefer it
         */
        if (CLASSIC_LOCATION.mediaPath.exists() && CLASSIC_LOCATION.mediaPath.isDirectory()) {

            return CLASSIC_LOCATION;

        } else {

            // auto-select the location with the most free space available

            Location best = null;
            for (Location location: getStorageLocations()) {
                if (!location.available)
                    continue;

                if (best == null || getStorageFreeSpace(best.mediaPath) < getStorageFreeSpace(location.mediaPath))
                    best = location;
            }

            if (best == null)
                return CLASSIC_LOCATION;

            return best;
        }
    }

    /**
     * Return the base directory for media storage to be used in Seadroid.
     *
     * It guaranties to always return a valid directory.
     * However, this directory might change at runtime, So it should never be cached.
     *
     * @return the base directory for media storage to be used in Seadroid.
     */
    public final File getMediaDir() {
        return getDirectoryCreateIfNeeded(getStorageLocation().mediaPath);
    }

    private Location lookupStorageLocation(int id) {
        for (Location location: getStorageLocations()) {
            if (location.id == id)
                return location;
        }
        return null;
    }

    /**
     * Return the storage location to be used in Seadroid.
     *
     * It guaranties to always return a valid one.
     *
     * @return Location info
     */
    public Location getStorageLocation() {

        int storageDirID = SettingsManager.instance().getStorageDir();
        Location storageLocation = lookupStorageLocation(storageDirID);

        // if there is one configured but unavailable, use fallback
        if (storageDirID >= 0 && !storageLocation.available) {
            Log.i(DEBUG_TAG, "Configured storage location " + storageDirID + " has become unavailable, falling back.");
            return CLASSIC_LOCATION;
        }

        // on first start of Seadroid, no location is configured yet
        if (storageDirID == Integer.MIN_VALUE) {

            storageLocation = getPreferredStorage();

            Log.i(DEBUG_TAG, "First start of Seadroid, auto-setting storage directory to " + storageLocation.id);
            SettingsManager.instance().setStorageDir(storageLocation.id);
        }

        if (storageLocation == null || !storageLocation.available) {
            Log.i(DEBUG_TAG, "Storage location " + storageLocation.id + " has become unavailable, falling back.");
            return CLASSIC_LOCATION;
        }

        // an explicit path is configured
        return storageLocation;
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
     * Store temp files in a subdirectory below the media directory.
     *
     * This should be a subdirectory of getMediaDir() so that temp files
     * can be efficiently moved into the media storage.
     *
     * @return base of where to store temp files
     */
    public final File getTempDir() {
        File base = getMediaDir();
        File tmpDir = new File(base, "temp");
        return getDirectoryCreateIfNeeded(tmpDir);
    }

    /**
     * Store JSON cache files in private internal cache.
     *
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
     * Store thumbnails in a subdirectory below the Seadroid cache directory.
     *
     * @return base of where to store thumbnails
     */
    public final File getThumbnailsDir() {
        File base = getStorageLocation().cachePath;
        File thumbnailsDir = new File(base, "thumbnails");
        return getDirectoryCreateIfNeeded(thumbnailsDir);
    }

    /**
     * A file was added, changed or removed. Notify the gallery.
     *
     * @param file
     */
    public final void notifyAndroidGalleryFileChange(File file) {
        MediaScannerConnection.scanFile(getContext(),
            new String[]{file.toString()},
            null,
            this);
    }

    /**
     * A directory was added, changed or removed. Notify the gallery.
     *
     * @param fileList
     */
    private final void notifyAndroidGalleryDirectoryChange(Collection<File> fileList) {

        int count = 0;
        String[] list = new String[fileList.size()];
        for (File f: fileList) {
            list[count++] = f.getAbsolutePath();
        }

        MediaScannerConnection.scanFile(getContext(),
                list, null, this);
    }

    /**
     * Deletes full cache
     * remember to clear cache from database after called this method
     */
    public final void clearCache() {
        Collection<File> fileList = FileUtils.listFiles(getMediaDir(), null, true);

        FileUtils.deleteQuietly(getMediaDir());
        FileUtils.deleteQuietly(getJsonCacheDir());
        FileUtils.deleteQuietly(getTempDir());
        FileUtils.deleteQuietly(getThumbnailsDir());

        notifyAndroidGalleryDirectoryChange(fileList);
    }

    /**
     * Deletes cache directory under a specific account<br>
     * remember to clear cache from database after called this method
     */
    public final void clearAccount(Account account) {
        DataManager dataManager = new DataManager(account);

        File accountDir = new File(dataManager.getAccountDir());
        Collection<File> fileList = FileUtils.listFiles(accountDir, null, true);

        FileUtils.deleteQuietly(accountDir);

        notifyAndroidGalleryDirectoryChange(fileList);
    }

    /**
     * Return space used by Seadroid
     *
     * @return
     */
    public final long getUsedSpace() {
        File mediaDir = getMediaDir();
        File thumbDir = getThumbnailsDir();

        if (!mediaDir.exists() || !thumbDir.exists()) return 0L;

        return FileUtils.sizeOfDirectory(mediaDir) + FileUtils.sizeOfDirectory(thumbDir);
    }

    public static class Location {
        /**
         * Our internal ID of this storage.
         */
        public int id;

        /**
         * Base media directory
         */
        public File mediaPath;

        /**
         * Base cache directory
         */
        public File cachePath;

        /**
         * Text description
         */
        public String description;

        /**
         * Is this location available (mounted)?
         */
        public boolean available;

        /**
         * Is this the currently selected location?
         */
        public boolean currentSelection;
    }
}
