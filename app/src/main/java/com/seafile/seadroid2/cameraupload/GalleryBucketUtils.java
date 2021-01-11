package com.seafile.seadroid2.cameraupload;

import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.util.Utils;

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
                MediaStore.Video.Media._ID,
                MediaStore.Video.Media.BUCKET_ID,
                MediaStore.Video.Media.BUCKET_DISPLAY_NAME,
        };

        String BUCKET_ORDER_BY = MediaStore.Video.Media.BUCKET_DISPLAY_NAME + " ASC";
        String BUCKET_GROUP_BY = "1) GROUP BY 1,(2";
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

            b.isCameraBucket = false;
            for (String name : CAMERA_BUCKET_NAMES) {
                if (b.name != null && b.name.equalsIgnoreCase(name)) {
                    b.isCameraBucket = true;
                }
            }

            // ignore buckets created by Seadroid
            String id = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media._ID));
            Uri uri = Uri.withAppendedPath(MediaStore.Video.Media.EXTERNAL_CONTENT_URI, id);
            String file = Utils.getRealPathFromURI(SeadroidApplication.getAppContext(),uri);
            if (file == null || !file.startsWith(StorageManager.getInstance().getMediaDir().getAbsolutePath()))
                buckets.add(b);
        }
        cursor.close();

        return buckets;
    }

    private static List<Bucket> getImageBuckets(Context context) {
        final Uri contentUri = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
        List<Bucket> buckets = new ArrayList<Bucket>();
        final String[] projection = {
                MediaStore.Images.Media.BUCKET_DISPLAY_NAME,
                MediaStore.Images.Media.BUCKET_ID,
                MediaStore.Images.Media._ID
        };

        try (final Cursor cursor = context.getContentResolver().query(
                contentUri,
                projection,
                null,
                null,
                null
        )) {
            if ((cursor != null) && (cursor.moveToFirst() == true)) {
                final int columnBucketName = cursor.getColumnIndexOrThrow(MediaStore.Images.Media.BUCKET_DISPLAY_NAME);
                final int columnBucketId = cursor.getColumnIndexOrThrow(MediaStore.Images.Media.BUCKET_ID);
                final int idColumnIndex = cursor.getColumnIndexOrThrow(MediaStore.Images.Media._ID);

                do {
                    final String bucketId = cursor.getString(columnBucketId);
                    final String bucketName = cursor.getString(columnBucketName);
                    Bucket b = new Bucket();
                    b.id = bucketId;
                    b.name = bucketName;
                    b.image_id = cursor.getInt(idColumnIndex);
                    b.isCameraBucket = false;
                    for (String name : CAMERA_BUCKET_NAMES) {
                        if (b.name != null && b.name.equalsIgnoreCase(name)) {
                            b.isCameraBucket = true;
                        }
                    }

                    buckets.add(b);

                } while (cursor.moveToNext());
            }
        }
        return buckets;

    }
}
