package com.seafile.seadroid2.framework.transport;

public final class TransportHolder {

    private static final LargeObjectTransport INSTANCE = new InMemoryLargeObjectTransport();

    public static LargeObjectTransport getInstance() {
        return INSTANCE;
    }
}
