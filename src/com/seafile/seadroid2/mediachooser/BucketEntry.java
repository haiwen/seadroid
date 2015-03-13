package com.seafile.seadroid2.mediachooser;

public class BucketEntry {
    public String bucketName;
    public int bucketId;
    public String bucketUrl = null;

    public BucketEntry(int id, String name, String url) {
        bucketId = id;
        bucketName = ensureNotNull(name);
        bucketUrl = url;
    }

    @Override
    public int hashCode() {
        return bucketId;
    }

    @Override
    public boolean equals(Object object) {
        if (!(object instanceof BucketEntry)) return false;
        BucketEntry entry = (BucketEntry) object;
        return bucketId == entry.bucketId;
    }

    public static String ensureNotNull(String value) {
        return value == null ? "" : value;
    }
}
