package com.seafile.seadroid2.cameraupload;

import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.provider.MediaStore;

import com.seafile.seadroid2.data.StorageManager;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;

/**
 * Helper class to fetch the list of buckets (Gallery folders) from the
 * media content providers.
 */
public class GalleryBucketUtils {
    private static final String DEBUG_TAG = "GalleryBucketUtils";

    /**
     * Per default we will upload images/videos from these buckets
     *
     * - https://en.wikipedia.org/wiki/Design_rule_for_Camera_File_system
     * - https://stackoverflow.com/questions/6248887/android-device-specific-camera-path-issue
     */
    public static final String[] CAMERA_BUCKET_NAMES = {"Camera", "100ANDRO", "100MEDIA"};
    public static final String IMAGES = "IMAGES";

    public static class Bucket {
        public String id;
        public String name;
        public String imageId;
        public String videoId;
        public String isImages;
        public String videoPath;
        public String imagePath;
        public int image_id = -1;
        public boolean isCameraBucket;

        @Override
        public boolean equals(Object obj) {
            if (this == obj)
                return true;
            if (obj == null || (obj.getClass() != this.getClass()))
                return false;

            Bucket a = (Bucket) obj;
            if (a.name == null || name == null)
                return false;

            return a.name.equals(this.name);
        }

        @Override
        public int hashCode() {
            return name.hashCode();
        }
    }

    /**
     * Fetch the list of buckets.
     *
     * Image and Video buckets are merged into one list. Duplicates are removed.
     *
     * @param context
     * @return the list of buckets.
     */
    public static List<Bucket> getMediaBuckets(Context context) {
        List<Bucket> video;
        List<Bucket> image;
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.Q) {
            video = getVideoBucketsBelowApi29(context);
            image = getImageBucketsBelowApi29(context);
        } else {
            video = getVideoBuckets(context);
            image = getImageBuckets(context);
        }

        List<Bucket> merged = image;

        VIDEO: for (Bucket v: video) {
            for (Bucket i: image) {
                if (v.id.equals(i.id))
                    continue VIDEO;
            }
            merged.add(v);
        }
        return merged;
    }

    private static List<Bucket> getVideoBucketsBelowApi29(Context context) {
        Uri images = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
        String[] projection = new String[]{
                MediaStore.Video.Media.BUCKET_ID,
                MediaStore.Video.Media.BUCKET_DISPLAY_NAME,
                MediaStore.Video.Media.DATA
        };

        String BUCKET_ORDER_BY = MediaStore.Video.Media.BUCKET_DISPLAY_NAME + " ASC";
        String BUCKET_GROUP_BY = "1) GROUP BY 1,(2";
        Cursor cursor = context.getContentResolver().query(images,
                projection,            // Which columns to return
                BUCKET_GROUP_BY,       // Which rows to return (all rows)
                null,                  // Selection arguments (none)
                BUCKET_ORDER_BY        // Ordering
        );

        List<Bucket> buckets = new ArrayList<Bucket>();

        if (cursor == null) {
            return buckets;
        }

        while (cursor.moveToNext()) {
            int bucketIdColumnIndex = cursor.getColumnIndex(MediaStore.Video.Media.BUCKET_ID);
            int bucketColumnIndex = cursor.getColumnIndex(MediaStore.Video.Media.BUCKET_DISPLAY_NAME);
            int dataColumnIndex = cursor.getColumnIndex(MediaStore.Video.Media.DATA);
            Bucket b = new Bucket();
            b.id = cursor.getString(bucketIdColumnIndex);
            b.name = cursor.getString(bucketColumnIndex);
            b.isCameraBucket = false;
            if (b.name == null) {
                continue;
            }
            for (String name : CAMERA_BUCKET_NAMES) {
                if (b.name.equalsIgnoreCase(name)) {
                    b.isCameraBucket = true;
                }
            }

            // ignore buckets created by Seadroid
            String file = cursor.getString(dataColumnIndex);
            if (file == null || !file.startsWith(StorageManager.getInstance().getMediaDir().getAbsolutePath()))
                buckets.add(b);
        }
        cursor.close();

        return buckets;
    }

    private static List<Bucket> getImageBucketsBelowApi29(Context context) {
        Uri images = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
        String[] projection = new String[]{
                MediaStore.Images.Media.BUCKET_ID,
                MediaStore.Images.Media.BUCKET_DISPLAY_NAME,
                MediaStore.Video.Media.DATA,
                MediaStore.Images.Media._ID
        };

        String BUCKET_ORDER_BY = MediaStore.Images.Media.BUCKET_DISPLAY_NAME + " ASC";
        String BUCKET_GROUP_BY = "1) GROUP BY 1,(2";
        Cursor cursor = context.getContentResolver().query(images,
                projection,            // Which columns to return
                BUCKET_GROUP_BY,       // Which rows to return (all rows)
                null,                  // Selection arguments (none)
                BUCKET_ORDER_BY        // Ordering
        );

        List<Bucket> buckets = new ArrayList<Bucket>();

        if (cursor == null) {
            return buckets;
        }

        while (cursor.moveToNext()) {
            int bucketIdColumnIndex = cursor.getColumnIndex(MediaStore.Images.Media.BUCKET_ID);
            int bucketColumnIndex = cursor.getColumnIndex(MediaStore.Images.Media.BUCKET_DISPLAY_NAME);
            int dataColumnIndex = cursor.getColumnIndex(MediaStore.Video.Media.DATA);
            int idColumnIndex = cursor.getColumnIndex(MediaStore.Images.Media._ID);
            Bucket b = new Bucket();
            b.id = cursor.getString(bucketIdColumnIndex);
            b.name = cursor.getString(bucketColumnIndex);
            b.image_id = cursor.getInt(idColumnIndex);
            b.isCameraBucket = false;
            if (b.name == null) {
                continue;
            }
            for (String name : CAMERA_BUCKET_NAMES) {
                if (b.name.equalsIgnoreCase(name)) {
                    b.isCameraBucket = true;
                }
            }

            // ignore buckets created by Seadroid
            String file = cursor.getString(dataColumnIndex);
            if (file == null || !file.startsWith(StorageManager.getInstance().getMediaDir().getAbsolutePath()))
                buckets.add(b);
        }
        cursor.close();

        return buckets;
    }

    private static List<Bucket> getVideoBuckets(Context context) {
        Uri images = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
        String[] projection = new String[]{
                MediaStore.Video.Media._ID,
                MediaStore.Video.Media.BUCKET_ID,
                MediaStore.Video.Media.BUCKET_DISPLAY_NAME,
        };
        Cursor cursor = context.getContentResolver().query(images,
                projection,            // Which columns to return
                null,       // Which rows to return (all rows)
                null,                  // Selection arguments (none)
                null        // Ordering
        );

        List<Bucket> buckets = new ArrayList<Bucket>();

        if (cursor == null) {
            return buckets;
        }

        while (cursor.moveToNext()) {
            int bucketIdColumnIndex = cursor.getColumnIndex(MediaStore.Video.Media.BUCKET_ID);
            int bucketColumnIndex = cursor.getColumnIndex(MediaStore.Video.Media.BUCKET_DISPLAY_NAME);
            Bucket b = new Bucket();
            b.id = cursor.getString(bucketIdColumnIndex);
            b.name = cursor.getString(bucketColumnIndex);
            String video_id = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media._ID));
            b.videoId = video_id;
            b.isCameraBucket = false;
            if (b.name == null) {
                continue;
            }
            for (String name : CAMERA_BUCKET_NAMES) {
                if (b.name.equalsIgnoreCase(name)) {
                    b.isCameraBucket = true;
                }
            }
            buckets.add(b);
        }
        cursor.close();
        LinkedHashSet<Bucket> bucketsSet = new LinkedHashSet<>(buckets.size());
        bucketsSet.addAll(buckets);
        List<GalleryBucketUtils.Bucket> tempBuckets = new ArrayList<>(bucketsSet.size());
        Iterator iterator = bucketsSet.iterator();
        while (iterator.hasNext()) {
            GalleryBucketUtils.Bucket bucket = (GalleryBucketUtils.Bucket) iterator.next();
            tempBuckets.add(bucket);
        }
        return tempBuckets;
    }

    private static List<Bucket> getImageBuckets(Context context) {
        Uri images = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
        String[] projection = new String[]{
                MediaStore.Images.Media.BUCKET_ID,
                MediaStore.Images.Media.BUCKET_DISPLAY_NAME,
                MediaStore.Images.Media._ID
        };

        Cursor cursor = context.getContentResolver().query(images,
                projection,            // Which columns to return
                null,       // Which rows to return (all rows)
                null,                  // Selection arguments (none)
                null        // Ordering
        );

        List<Bucket> buckets = new ArrayList<Bucket>();

        if (cursor == null) {
            return buckets;
        }

        while (cursor.moveToNext()) {
            int bucketIdColumnIndex = cursor.getColumnIndex(MediaStore.Images.Media.BUCKET_ID);
            int bucketColumnIndex = cursor.getColumnIndex(MediaStore.Images.Media.BUCKET_DISPLAY_NAME);
            Bucket b = new Bucket();
            b.id = cursor.getString(bucketIdColumnIndex);
            b.name = cursor.getString(bucketColumnIndex);
            String image_id = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Images.Media._ID));
            b.imageId = image_id;
            b.isCameraBucket = false;
            b.isImages = GalleryBucketUtils.IMAGES;
            if (b.name == null) {
                continue;
            }
            for (String name : CAMERA_BUCKET_NAMES) {
                if (b.name.equalsIgnoreCase(name)) {
                    b.isCameraBucket = true;
                }
            }
            buckets.add(b);
        }
        cursor.close();
        LinkedHashSet<Bucket> bucketsSet = new LinkedHashSet<>(buckets.size());
        bucketsSet.addAll(buckets);
        List<GalleryBucketUtils.Bucket> tempBuckets = new ArrayList<>(bucketsSet.size());
        Iterator iterator = bucketsSet.iterator();
        while (iterator.hasNext()) {
            GalleryBucketUtils.Bucket bucket = (GalleryBucketUtils.Bucket) iterator.next();
            tempBuckets.add(bucket);
        }
        return tempBuckets;
    }
}