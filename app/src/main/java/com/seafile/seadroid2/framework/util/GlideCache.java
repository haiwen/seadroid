package com.seafile.seadroid2.framework.util;

import android.content.Context;

import androidx.annotation.NonNull;

import com.bumptech.glide.Glide;
import com.bumptech.glide.GlideBuilder;
import com.bumptech.glide.Registry;
import com.bumptech.glide.annotation.GlideModule;
import com.bumptech.glide.integration.okhttp3.OkHttpUrlLoader;
import com.bumptech.glide.load.engine.cache.DiskLruCacheFactory;
import com.bumptech.glide.load.model.GlideUrl;
import com.bumptech.glide.module.AppGlideModule;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.http.interceptor.CurrentTokenInterceptor;
import com.seafile.seadroid2.framework.http.interceptor.HeaderInterceptor;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

import okhttp3.Interceptor;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;

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
        GlideApp.tearDown();
    }

    @Override
    public boolean isManifestParsingEnabled() {
        return false;
    }

    @Override
    public void registerComponents(@NonNull Context context, @NonNull Glide glide, @NonNull Registry registry) {
        try {
            OkHttpClient client = getClient();
            registry.replace(GlideUrl.class, InputStream.class, new OkHttpUrlLoader.Factory(client));
        } catch (IllegalStateException e) {
            SLogs.d("No current account?");
        }
    }

    private OkHttpClient getClient() {
        return new OkHttpClient.Builder().addInterceptor(new CurrentTokenInterceptor()).build();
    }
}
