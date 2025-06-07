package com.seafile.seadroid2.framework.util;

import android.content.Context;
import android.net.Uri;

import androidx.annotation.NonNull;
import androidx.core.content.FileProvider;

import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.framework.datastore.StorageManager;

import java.io.File;

import kotlin.Pair;

public class TakeCameras {

    @NonNull
    public static Pair<Uri, File> buildPhotoUri(Context context) {
        return buildUriAndDeleteOldFiles(context, ".jpg");
    }

    @NonNull
    public static Pair<Uri, File> buildVideoUri(Context context) {
        return buildUriAndDeleteOldFiles(context, ".mp4");
    }

    private static Pair<Uri, File> buildUriAndDeleteOldFiles(Context context, String fileExtFormat) {
        File parentFolder = StorageManager.getInstance().getTakeCameraDir();

        File[] fs = parentFolder.listFiles();
        if (fs != null) {
            for (File f : fs) {
                boolean s = f.delete();
                Logs.d("delete: " + s + "-> " + f.getAbsolutePath());
            }
        }
        String newName = genNewName(fileExtFormat);
        File file = new File(parentFolder, newName);
        Uri uri = FileProvider.getUriForFile(context, BuildConfig.FILE_PROVIDER_AUTHORITIES, file);
        return new Pair<>(uri, file);
    }

    private static String genNewName(String prefix) {
        long mills = System.currentTimeMillis();
        String date = TimeUtils.millis2String(mills, "yyyyMMddHHmmss");
        return "sf_" + date + prefix;
    }

}
