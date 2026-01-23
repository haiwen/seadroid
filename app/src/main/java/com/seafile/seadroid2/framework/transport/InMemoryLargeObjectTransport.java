package com.seafile.seadroid2.framework.transport;

import java.lang.ref.SoftReference;
import java.util.concurrent.ConcurrentHashMap;

public final class InMemoryLargeObjectTransport implements LargeObjectTransport {

    private static final ConcurrentHashMap<String, SoftReference<Object>> CACHE = new ConcurrentHashMap<>();

    @Override
    public void put(String key, Object data) {
        if (key == null) {
            return;
        }
        // remove old
        CACHE.remove(key);
        //
        CACHE.put(key, new SoftReference<>(data));
    }

    @SuppressWarnings("unchecked")
    @Override
    public <T> T get(String key) {
        SoftReference<Object> ref = CACHE.get(key);
        return ref != null ? (T) ref.get() : null;
    }

    @Override
    public void remove(String key) {
        CACHE.remove(key);
    }
}
