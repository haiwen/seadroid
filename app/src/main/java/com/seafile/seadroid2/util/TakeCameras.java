package com.seafile.seadroid2.util;

import android.content.Context;
import android.net.Uri;
import android.os.Environment;

import androidx.core.content.FileProvider;

import com.seafile.seadroid2.BuildConfig;

import java.io.File;

public class TakeCameras {

    public static String getStoragePictureFolder(Context context) {
        return Environment.DIRECTORY_PICTURES;
    }

    public static String getStorageDownloadFolder(Context context) {
        return Environment.DIRECTORY_DOWNLOADS;
    }

    // -> /storage/emulated/0/Android/data/package/files/Pictures
    public static File getStoragePath(Context context) {
        return context.getExternalFilesDir(getStoragePictureFolder(context));
    }

    public static Uri buildTakePhotoUri(Context context) {
        File parentFolder = getStoragePath(context);
        File file = new File(parentFolder, "sd_photo_" + System.currentTimeMillis() + ".jpg");
        return FileProvider.getUriForFile(context, BuildConfig.FILE_PROVIDER_AUTHORITIES, file);
    }

    public static Uri buildTakePhotoUriAfterCleanOldCacheFiles(Context context) {
        File parentFolder = getStoragePath(context);
        File[] fs = parentFolder.listFiles();
        if (fs != null) {
            for (File f : fs) {
                boolean s = f.delete();
                SLogs.d(s + "->" + f.getAbsolutePath());
            }
        }

        File file = new File(parentFolder, "sd_photo_" + System.currentTimeMillis() + ".jpg");
        return FileProvider.getUriForFile(context, BuildConfig.FILE_PROVIDER_AUTHORITIES, file);
    }

    public static Uri buildTakeVideoUriAfterCleanOldFiles(Context context) {
        File parentFolder = context.getExternalFilesDir(Environment.DIRECTORY_DCIM);
        if (parentFolder == null) {
            return null;
        }

        File[] fs = parentFolder.listFiles();
        if (fs != null) {
            for (File f : fs) {
                boolean s = f.delete();
                SLogs.d(s + "->" + f.getAbsolutePath());
            }
        }

        File file = new File(parentFolder, "st_video_" + System.currentTimeMillis() + ".mp4");
        return FileProvider.getUriForFile(context, BuildConfig.FILE_PROVIDER_AUTHORITIES, file);
    }


    /**
     * for take photo
     * {@link #buildTakePhotoUriAfterCleanOldCacheFiles(Context)}
     */
    public static File uri2File(Context context, Uri u) {
        File parentFile = TakeCameras.getStoragePath(context);
        String p = u.getPath().replace("/external_files_path/" + getStoragePictureFolder(context), "");
        return new File(parentFile.getPath() + p);
    }
}
