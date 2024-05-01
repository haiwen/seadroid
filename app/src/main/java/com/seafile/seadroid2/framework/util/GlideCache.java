package com.seafile.seadroid2.framework.util;

import android.content.Context;
import androidx.annotation.NonNull;

import com.bumptech.glide.GlideBuilder;
import com.bumptech.glide.annotation.GlideModule;
import com.bumptech.glide.load.engine.cache.DiskLruCacheFactory;
import com.bumptech.glide.module.AppGlideModule;
import com.seafile.seadroid2.SeadroidApplication;

import java.io.File;

@GlideModule
public class GlideCache extends AppGlideModule {
    @Override
    public void applyOptions(@NonNull Context context, @NonNull GlideBuilder builder) {
        super.applyOptions(context, builder);
//        String rootPath = SeadroidApplication.getAppContext().getExternalFilesDir(Environment.DIRECTORY_DOCUMENTS).getAbsolutePath();
        File[] externalMediaDirs = SeadroidApplication.getAppContext().getExternalMediaDirs();
        String rootPath = externalMediaDirs[0].getAbsolutePath();
        File dirPath = new File(rootPath + "/GlideCache/");
        builder.setDiskCache(new DiskLruCacheFactory(dirPath.getAbsolutePath(), 1024 * 1024 * 100));
    }
}
