package com.seafile.seadroid2.framework.glide;

import android.content.Context;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.PathUtils;
import com.bumptech.glide.Glide;
import com.bumptech.glide.GlideBuilder;
import com.bumptech.glide.Registry;
import com.bumptech.glide.annotation.GlideModule;
import com.bumptech.glide.integration.okhttp3.OkHttpUrlLoader;
import com.bumptech.glide.load.engine.cache.DiskLruCacheFactory;
import com.bumptech.glide.load.model.GlideUrl;
import com.bumptech.glide.module.AppGlideModule;
import com.seafile.seadroid2.framework.http.UnsafeOkHttpClient;
import com.seafile.seadroid2.framework.http.interceptor.CurrentTokenInterceptor;
import com.seafile.seadroid2.framework.util.SLogs;

import java.io.File;
import java.io.InputStream;

import okhttp3.OkHttpClient;

@GlideModule
public class SeafGlideCache extends AppGlideModule {


    @Override
    public void applyOptions(@NonNull Context context, @NonNull GlideBuilder builder) {
        super.applyOptions(context, builder);
        String rootPath = PathUtils.getExternalAppFilesPath();
        File dirPath = new File(rootPath + "/cache/glide/");
        builder.setDiskCache(new DiskLruCacheFactory(dirPath.getAbsolutePath(), 1024 * 1024 * 500));
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

            registry.append(GlideImage.class, InputStream.class, new GlideImageModelLoaderFactory());

        } catch (IllegalStateException e) {
            SLogs.d("SeaGlideCache","No current account?");
        }
    }

    private OkHttpClient getClient() {
        UnsafeOkHttpClient unsafeOkHttpClient = new UnsafeOkHttpClient();
        OkHttpClient.Builder builder = unsafeOkHttpClient.getBuilder();
        builder.followRedirects(true);
        builder.addInterceptor(new CurrentTokenInterceptor());
        return builder.build();
    }
}
