package com.seafile.seadroid2.data;

import android.annotation.TargetApi;
import android.content.ContentResolver;
import android.content.ContentValues;
import android.net.Uri;
import android.os.Build;
import android.os.StatFs;
import android.provider.MediaStore;
import android.util.Log;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.util.Utils;

import java.io.File;


/**
 * StorageManager implementation for KitKat devices.
 */
@TargetApi(Build.VERSION_CODES.KITKAT)
public class StorageManagerKitKat extends StorageManager {

    @Override
    protected File[] getSystemMediaDirs() {
        /**
         * KitKat does offer media directories on every storage medium.
         * So this is a good API to store our data in.
         *
         * Unfortunately, KitKat has a bug, that files in this directory
         * won't be indexed by the gallery. This bug can be workarounded
         * however, which we do in Utils.notifyAndroidGalleryFileChange()
         */
        return getContext().getExternalFilesDirs(null);
    }

    @Override
    protected File[] getSystemCacheDirs() {
        return getContext().getExternalCacheDirs();
    }

    @Override
    protected long getStorageSize(File dir) {
        StatFs stat = new StatFs(dir.getParentFile().getAbsolutePath());
        return stat.getTotalBytes();
    }

    @Override
    protected long getStorageFreeSpace(File dir) {
        StatFs stat = new StatFs(dir.getParentFile().getAbsolutePath());
        return stat.getAvailableBytes();
    }

    @Override
    public boolean supportsMultipleStorageLocations() {
        return true;
    }

    @Override
    public void onScanCompleted(String path, Uri uri) {
        super.onScanCompleted(path, uri);

        /*
         * According to the Android API, media files stored in Context.getExternalFilesDir[s]()
         * should be indexed by the Media Scanner.
         *
         * Unfortunately, the Android framework has a bug there: The files are indexed into the Media
         * Store, but not as "photos" or "videos", but as generic "files". As such they won't show
         * up in the Android Gallery app.
         *
         * https://code.google.com/p/android/issues/detail?id=68056#c1 explains this
         * and suggests a workaround. The following code is an implementation of that workaround.
         *
         * Starting with API 21, Android has extended its API with Context.getExternalMediaDirs(),
         * which works as advertised. So this workaround is only necessary on API levels 19 and 20.
         */
        if (uri != null && uri.getPath().startsWith("/external/file")) {
            ContentResolver contentResolver = SeadroidApplication.getAppContext().getContentResolver();
            // if a files has just been added as a generic "file", fix the MediaStore and change the
            // file type to image or video.

            ContentValues v = new ContentValues();
            String mimeType = Utils.getFileMimeType(path);

            if (mimeType.startsWith("image/")) {
                v.put(MediaStore.Files.FileColumns.MEDIA_TYPE, MediaStore.Files.FileColumns.MEDIA_TYPE_IMAGE);
            } else if (mimeType.startsWith("video/")) {
                v.put(MediaStore.Files.FileColumns.MEDIA_TYPE, MediaStore.Files.FileColumns.MEDIA_TYPE_VIDEO);
            }

            int rows = contentResolver.update(uri, v, null, null);
            Log.d(DEBUG_TAG, "-> rows=" + rows);
        }
    }
}
