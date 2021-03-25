package com.seafile.seadroid2.util;

import android.content.Context;
import android.os.Environment;
import android.support.annotation.NonNull;

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
        String rootPath = SeadroidApplication.getAppContext().getExternalFilesDir(Environment.DIRECTORY_DOCUMENTS).getAbsolutePath();
        File dirPath = new File(rootPath + "/GlideCache/");
        builder.setDiskCache(new DiskLruCacheFactory(dirPath.getAbsolutePath(), 1024 * 1024 * 50));
    }
}
