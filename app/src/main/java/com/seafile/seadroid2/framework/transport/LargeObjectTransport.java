package com.seafile.seadroid2.framework.transport;

public interface LargeObjectTransport {
    void put(String key, Object data);

    <T> T get(String key);

    void remove(String key);
}
