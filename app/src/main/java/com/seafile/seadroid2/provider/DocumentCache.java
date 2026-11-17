package com.seafile.seadroid2.provider;

import android.os.SystemClock;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

public class DocumentCache {

    private static final long CACHE_EXPIRATION = TimeUnit.MILLISECONDS.convert(5, TimeUnit.SECONDS);
    private final Map<String, Long> mCache = new ConcurrentHashMap<>();


    public boolean isExpired(String documentId) {
        Long expireTimeLong = mCache.get(documentId);
        if (expireTimeLong == null) {
            return true;//not found, need to re-load
        }

        if (expireTimeLong + CACHE_EXPIRATION < SystemClock.elapsedRealtime()) {
            return true;//expired, need to re-load
        }

        return false;//ok
    }


    public void put(String documentId) {
        mCache.put(documentId, SystemClock.elapsedRealtime());
    }

    public void remove(String documentId) {
        mCache.remove(documentId);
    }
}
