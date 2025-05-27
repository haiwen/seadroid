package com.seafile.seadroid2.provider;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;

public class DocumentCache {

    private static final long CACHE_EXPIRATION = TimeUnit.MILLISECONDS.convert(15, TimeUnit.SECONDS);
    private final Map<String, Long> mCache = new ConcurrentHashMap<>();

    public int get(String documentId) {
        Long expireTimeLong = mCache.get(documentId);
        if (expireTimeLong == null) {
            return -1;//not found, need to load
        }

        if (expireTimeLong + CACHE_EXPIRATION < System.currentTimeMillis()) {
            return -1;//expired, need to load
        }

        return 1;//ok
    }


    public void put(String documentId) {
        mCache.put(documentId, System.currentTimeMillis());
    }

    public void remove(String documentId) {
        mCache.remove(documentId);
    }
}
