package com.shuyu.gsyvideoplayer.cache;

import android.content.Context;
import android.net.Uri;
import android.text.TextUtils;

import com.danikula.videocache.CacheListener;
import com.danikula.videocache.HttpProxyCacheServer;
import com.danikula.videocache.file.Md5FileNameGenerator;
import com.shuyu.gsyvideoplayer.utils.CommonUtil;
import com.shuyu.gsyvideoplayer.utils.FileUtils;
import com.shuyu.gsyvideoplayer.utils.StorageUtils;

import java.io.File;
import java.io.IOException;
import java.util.Map;

import tv.danmaku.ijk.media.player.IMediaPlayer;

/**
 Proxy Cache Manager
 */

public class ProxyCacheManager implements ICacheManager, CacheListener {

    //video agency
    protected HttpProxyCacheServer proxy;


    protected File mCacheDir;

    protected boolean mCacheFile;

    private static ProxyCacheManager proxyCacheManager;

    private ICacheManager.ICacheAvailableListener cacheAvailableListener;

    protected ProxyCacheUserAgentHeadersInjector userAgentHeadersInjector = new ProxyCacheUserAgentHeadersInjector();

    public static synchronized ProxyCacheManager instance() {
        if (proxyCacheManager == null) {
            proxyCacheManager = new ProxyCacheManager();
        }
        return proxyCacheManager;
    }


    @Override
    public void onCacheAvailable(File cacheFile, String url, int percentsAvailable) {
        if (cacheAvailableListener != null) {
            cacheAvailableListener.onCacheAvailable(cacheFile, url, percentsAvailable);
        }
    }

    @Override
    public void doCacheLogic(Context context, IMediaPlayer mediaPlayer, String originUrl, Map<String, String> header, File cachePath) {
        String url = originUrl;
        userAgentHeadersInjector.mMapHeadData.clear();
        if (header != null) {
            userAgentHeadersInjector.mMapHeadData.putAll(header);
        }
        if (url.startsWith("http") && !url.contains("127.0.0.1") && !url.contains(".m3u8")) {
            HttpProxyCacheServer proxy = getProxy(context.getApplicationContext(), cachePath);
            if (proxy != null) {
                url = proxy.getProxyUrl(url);
                mCacheFile = (!url.startsWith("http"));
                if (!mCacheFile) {
                    proxy.registerCacheListener(this, originUrl);
                }
            }
        } else if ((!url.startsWith("http") && !url.startsWith("rtmp")
                && !url.startsWith("rtsp") && !url.contains(".m3u8"))) {
            mCacheFile = true;
        }
        try {
            mediaPlayer.setDataSource(context, Uri.parse(url), header);
        } catch (IOException e) {
            e.printStackTrace();
        }

    }

    @Override
    public void clearCache(Context context, File cachePath, String url) {
        if (TextUtils.isEmpty(url)) {
            String path = StorageUtils.getIndividualCacheDirectory
                    (context.getApplicationContext()).getAbsolutePath();
            FileUtils.deleteFiles(new File(path));
        } else {
            Md5FileNameGenerator md5FileNameGenerator = new Md5FileNameGenerator();
            String name = md5FileNameGenerator.generate(url);
            if (cachePath != null) {
                String tmpPath = cachePath.getAbsolutePath() + File.separator + name + ".download";
                String path = cachePath.getAbsolutePath() + File.separator + name;
                CommonUtil.deleteFile(tmpPath);
                CommonUtil.deleteFile(path);
            } else {
                String pathTmp = StorageUtils.getIndividualCacheDirectory
                        (context.getApplicationContext()).getAbsolutePath()
                        + File.separator + name + ".download";
                String path = StorageUtils.getIndividualCacheDirectory
                        (context.getApplicationContext()).getAbsolutePath()
                        + File.separator + name;
                CommonUtil.deleteFile(pathTmp);
                CommonUtil.deleteFile(path);
            }
        }
    }

    @Override
    public void release() {
        if (proxy != null) {
            try {
                proxy.unregisterCacheListener(this);
            } catch (Exception e) {
                e.printStackTrace();
            }
        }
    }

    @Override
    public boolean cachePreview(Context context, File cacheDir, String url) {
        HttpProxyCacheServer proxy = getProxy(context.getApplicationContext(), cacheDir);
        if (proxy != null) {
            url = proxy.getProxyUrl(url);
        }
        return (!url.startsWith("http"));
    }

    @Override
    public boolean hadCached() {
        return mCacheFile;
    }


    @Override
    public void setCacheAvailableListener(ICacheAvailableListener cacheAvailableListener) {
        this.cacheAvailableListener = cacheAvailableListener;
    }

    /**
     Create a caching proxy service with a file directory.
     */
    public HttpProxyCacheServer newProxy(Context context, File file) {
        if (!file.exists()) {
            file.mkdirs();
        }
        HttpProxyCacheServer.Builder builder = new HttpProxyCacheServer.Builder(context);
        builder.cacheDirectory(file);
        builder.headerInjector(userAgentHeadersInjector);
        mCacheDir = file;
        return builder.build();
    }

    public void setProxy(HttpProxyCacheServer proxy) {
        this.proxy = proxy;
    }

    /**
     Create a caching proxy service
     */
    public HttpProxyCacheServer newProxy(Context context) {
        return new HttpProxyCacheServer.Builder(context.getApplicationContext())
                .headerInjector(userAgentHeadersInjector).build();
    }


    /**
     Get caching proxy service
     */
    protected static HttpProxyCacheServer getProxy(Context context) {
        HttpProxyCacheServer proxy = ProxyCacheManager.instance().proxy;
        return proxy == null ? (ProxyCacheManager.instance().proxy =
                ProxyCacheManager.instance().newProxy(context)) : proxy;
    }


    /**
     Get caching proxy service, with file directory
     */
    public static HttpProxyCacheServer getProxy(Context context, File file) {

        //If empty, return the default
        if (file == null) {
            return getProxy(context);
        }

        //If there is already a cache file path, then determine whether the cache file path is consistent
        if (ProxyCacheManager.instance().mCacheDir != null
                && !ProxyCacheManager.instance().mCacheDir.getAbsolutePath().equals(file.getAbsolutePath())) {
            //Inconsistent, close the old one first
            HttpProxyCacheServer proxy = ProxyCacheManager.instance().proxy;

            if (proxy != null) {
                proxy.shutdown();
            }
            //open new
            return (ProxyCacheManager.instance().proxy =
                    ProxyCacheManager.instance().newProxy(context, file));
        } else {
            //There is no cache file or consistent, return to the original
            HttpProxyCacheServer proxy = ProxyCacheManager.instance().proxy;

            return proxy == null ? (ProxyCacheManager.instance().proxy =
                    ProxyCacheManager.instance().newProxy(context, file)) : proxy;
        }
    }

}
