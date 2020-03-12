package com.seafile.seadroid2.cameraupload;

import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore;

import com.seafile.seadroid2.data.StorageManager;

import java.util.ArrayList;
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

    public static class Bucket {
        public String id;
        public String name;
        public int image_id = -1;
        public boolean isCameraBucket;
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
        List<Bucket> video = getVideoBuckets(context);
        List<Bucket> image = getImageBuckets(context);

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

    private static List<Bucket> getVideoBuckets(Context context) {
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
            for (String name : CAMERA_BUCKET_NAMES) {
                if (b.name != null && b.name.equalsIgnoreCase(name)) {
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

    private static List<Bucket> getImageBuckets(Context context) {
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
            for (String name : CAMERA_BUCKET_NAMES) {
                if (b.name != null && b.name.equalsIgnoreCase(name)) {
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
}
