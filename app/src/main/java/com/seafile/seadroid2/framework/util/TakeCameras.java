package com.seafile.seadroid2.framework.util;

import android.content.Context;
import android.net.Uri;
import android.os.Environment;

import androidx.core.content.FileProvider;

import com.blankj.utilcode.util.PathUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.google.android.gms.common.util.DataUtils;
import com.seafile.seadroid2.BuildConfig;

import java.io.File;

import kotlin.Pair;

public class TakeCameras {


    /**
     * Photos and videos taken are stored here
     * /DCMI/Seafile/images
     * /DCMI/Seafile/videos
     */
    public static File getMediaStoragePath(String folderName) {
        String dcmi = PathUtils.getExternalDcimPath();
        dcmi += "/Seafile/" + folderName;

        File parentFolderFile = new File(dcmi);
        if (!parentFolderFile.exists()) {
            parentFolderFile.mkdirs();
        }

        return parentFolderFile;
    }

    public static Pair<Uri, File> buildTakePhotoUri(Context context) {
        File parentFolder = getMediaStoragePath("images");
        String fileName = getOneNewName("jpg");
        File file = new File(parentFolder, fileName);
        Uri uri = FileProvider.getUriForFile(context, BuildConfig.FILE_PROVIDER_AUTHORITIES, file);
        return new Pair<>(uri, file);
    }

    private static String getOneNewName(String prefix) {
        long mills = System.currentTimeMillis();
        long seconds = mills / 1000;
        String date = TimeUtils.millis2String(mills, "yyyyMMdd");
        return date + "_" + seconds + "." + prefix;
    }

    public static Pair<Uri, File> buildTakeVideoUri(Context context) {
        File parentFolder = getMediaStoragePath("videos");
        String fileName = getOneNewName("mp4");
        File file = new File(parentFolder, fileName);
        Uri uri = FileProvider.getUriForFile(context, BuildConfig.FILE_PROVIDER_AUTHORITIES, file);
        return new Pair<>(uri, file);
    }

}
