/*
 * Copyright 2013 - learnNcode (learnncode@gmail.com)
 * 
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */


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
