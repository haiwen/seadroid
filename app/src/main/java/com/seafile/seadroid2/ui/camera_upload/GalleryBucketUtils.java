package com.seafile.seadroid2.ui.camera_upload;

import android.Manifest;
import android.content.Context;
import android.content.pm.PackageManager;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.provider.MediaStore;
import android.text.TextUtils;

import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.framework.datastore.StorageManager;
import com.seafile.seadroid2.framework.util.SLogs;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * Helper class to fetch the list of buckets (Gallery folders) from the
 * media content providers.
 */
public class GalleryBucketUtils {
    /**
     * Per default we will upload images/videos from these buckets
     *
     * <br>
     * - <a href="https://en.wikipedia.org/wiki/Design_rule_for_Camera_File_system">https://en.wikipedia.org/wiki/Design_rule_for_Camera_File_system</a>
     * <br>
     * - <a href="https://stackoverflow.com/questions/6248887/android-device-specific-camera-path-issue">https://stackoverflow.com/questions/6248887/android-device-specific-camera-path-issue</a>
     */
    public static final List<String> CAMERA_BUCKET_NAMES_LIST = CollectionUtils.newArrayList("CAMERA", "100ANDRO", "100MEDIA");

    public static class Bucket {
        public String bucketId;
        public String bucketName;

        //media uri info

        /**
         * media id is the image or video id in the media store
         */
        public String mediaId; // image or video id
        public boolean isImage;
        public Uri uri;

        public boolean isCameraBucket;

        /**
         * <b>
         * the equals here only work for deduplication, you can't use this function to compare whether the data is exactly the same
         * </b>
         */
        @Override
        public boolean equals(Object obj) {
            if (this == obj) {
                return true;
            }

            if (obj == null || (obj.getClass() != this.getClass())) {
                return false;
            }

            Bucket a = (Bucket) obj;
            if (a.bucketName == null || bucketName == null) {
                return false;
            }

            return Objects.equals(a.bucketName, bucketName) && Objects.equals(a.bucketId, bucketId);
        }

        @Override
        public int hashCode() {
            return Objects.hash(bucketName, bucketId);
        }
    }

    /**
     * Fetch the list of buckets.
     * <p>
     * Image and Video buckets are merged into one list. Duplicates are removed.
     *
     * @param context
     * @return the list of buckets.
     */
    public static List<Bucket> getMediaBuckets(Context context) {

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            if (ContextCompat.checkSelfPermission(context, Manifest.permission.READ_MEDIA_IMAGES) != PackageManager.PERMISSION_GRANTED) {
                return null;
            }
        } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            if (ContextCompat.checkSelfPermission(context, Manifest.permission.MANAGE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                return null;
            }
        } else {
            if (ContextCompat.checkSelfPermission(context, Manifest.permission.READ_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
                return null;
            }
        }

        List<Bucket> videos;
        List<Bucket> images;
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.Q) {
            videos = getVideoBucketsBelowAndroid10Api29(context);
            images = getImageBucketsBelowAndroid10Api29(context);
        } else {
            videos = getVideoBuckets(context);
            images = getImageBuckets(context);
        }

        List<Bucket> merged = new ArrayList<>();
        merged.addAll(videos);
        merged.addAll(images);
        return merged.stream().distinct().collect(Collectors.toList());
    }

    private static List<Bucket> getVideoBucketsBelowAndroid10Api29(Context context) {
        Uri images = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
        String[] projection = new String[]{
                MediaStore.Video.Media._ID,
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

        String localCacheAbsPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();

        try {
            while (cursor.moveToNext()) {
                Bucket b = new Bucket();

                b.mediaId = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media._ID));
                b.bucketId = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media.BUCKET_ID));
                b.bucketName = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media.BUCKET_DISPLAY_NAME));

                if (TextUtils.isEmpty(b.bucketName)) {
                    SLogs.i("skip bucket media -> video media id: " + b.mediaId + ", because bucket display name is null");
                    continue;
                }

                b.isCameraBucket = CAMERA_BUCKET_NAMES_LIST.contains(b.bucketName.toUpperCase());

                Uri baseUri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
                b.uri = Uri.withAppendedPath(baseUri, b.mediaId);

                // ignore buckets created by Seadroid
                String localPath = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Video.Media.DATA));
                if (localPath != null && localPath.startsWith(localCacheAbsPath)) {
                    continue;
                }

                buckets.add(b);
            }
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } finally {
            cursor.close();
        }

        return buckets;
    }

    private static List<Bucket> getImageBucketsBelowAndroid10Api29(Context context) {
        Uri images = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;

        //MediaStore.Images.Media.DATA 在 android10以上被弃用，建议使用MediaStore.Images.Media.RELATIVE_PATH(api28+)
        String[] projection = new String[]{
                MediaStore.Images.Media._ID,
                MediaStore.Images.Media.BUCKET_ID,
                MediaStore.Images.Media.BUCKET_DISPLAY_NAME,
                MediaStore.Images.Media.DATA
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

        String localCacheAbsPath = StorageManager.getInstance().getMediaDir().getAbsolutePath();

        try {
            while (cursor.moveToNext()) {
                Bucket b = new Bucket();

                b.bucketId = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Images.Media.BUCKET_ID));
                b.bucketName = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Images.Media.BUCKET_DISPLAY_NAME));
                b.mediaId = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Images.Media._ID));

                if (TextUtils.isEmpty(b.bucketName)) {
                    SLogs.i("skip bucket media -> image media id: " + b.mediaId + ", because bucket display name is null");
                    continue;
                }

                Uri baseUri = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
                b.uri = Uri.withAppendedPath(baseUri, b.mediaId);

                b.isCameraBucket = CAMERA_BUCKET_NAMES_LIST.contains(b.bucketName.toUpperCase());

                // ignore buckets created by Seadroid
                String localPath = cursor.getString(cursor.getColumnIndexOrThrow(MediaStore.Images.Media.DATA));
                if (localPath != null && localPath.startsWith(localCacheAbsPath)) {
                    continue;
                }

                buckets.add(b);
            }
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } finally {
            cursor.close();
        }

        return buckets;
    }

    private static List<Bucket> getVideoBuckets(Context context) {
        Uri images = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
        String[] projection = new String[]{
                MediaStore.Video.Media._ID,
                MediaStore.Video.Media.BUCKET_ID,
                MediaStore.Video.Media.BUCKET_DISPLAY_NAME
        };
        String sortOrder = MediaStore.Video.Media.DATE_ADDED + " DESC";
        Cursor cursor = context.getContentResolver().query(images,
                projection, // Which columns to return
                null,       // Which rows to return (all rows)
                null,       // Selection arguments (none)
                sortOrder
        );

        List<Bucket> buckets = new ArrayList<Bucket>();

        if (cursor == null) {
            return buckets;
        }

        try {
            while (cursor.moveToNext()) {
                Bucket b = new Bucket();
                int mediaIdIndex = cursor.getColumnIndexOrThrow(projection[0]);
                int idIndex = cursor.getColumnIndexOrThrow(projection[1]);
                int nameIndex = cursor.getColumnIndexOrThrow(projection[2]);

                b.isImage = false;
                b.mediaId = cursor.getString(mediaIdIndex);
                b.bucketId = cursor.getString(idIndex);
                b.bucketName = cursor.getString(nameIndex);

                Uri baseUri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;
                b.uri = Uri.withAppendedPath(baseUri, b.mediaId);

                if (TextUtils.isEmpty(b.bucketName)) {
                    SLogs.i("skip bucket media -> video media id: " + b.mediaId + ", because bucket display name is null");
                    continue;
                }

                b.isCameraBucket = CAMERA_BUCKET_NAMES_LIST.contains(b.bucketName.toUpperCase());

                buckets.add(b);
            }
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } finally {
            cursor.close();
        }

        return buckets;
    }

    private static List<Bucket> getImageBuckets(Context context) {
        Uri images = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
        // Android 13+
        String[] projection = Build.VERSION.SDK_INT >= Build.VERSION_CODES.R ? new String[]{
                MediaStore.Images.Media._ID,
                MediaStore.Images.Media.BUCKET_ID,
                MediaStore.Images.Media.BUCKET_DISPLAY_NAME,
                MediaStore.Images.Media.DATE_TAKEN  // 添加时间戳字段
        } : new String[]{
                MediaStore.Images.Media._ID,
                MediaStore.Images.Media.BUCKET_ID,
                MediaStore.Images.Media.BUCKET_DISPLAY_NAME
        };

        String sortOrder = MediaStore.Images.Media.DATE_ADDED + " DESC";

        Cursor cursor = context.getContentResolver().query(images,
                projection, // Which columns to return
                null,       // Which rows to return (all rows)
                null,       // Selection arguments (none)
                sortOrder
        );

        List<Bucket> buckets = new ArrayList<Bucket>();

        if (cursor == null) {
            return buckets;
        }
        try {
            while (cursor.moveToNext()) {
                Bucket b = new Bucket();
                int mediaIdIndex = cursor.getColumnIndexOrThrow(projection[0]);
                int idIndex = cursor.getColumnIndexOrThrow(projection[1]);
                int nameIndex = cursor.getColumnIndexOrThrow(projection[2]);

                b.isImage = true;
                b.mediaId = cursor.getString(mediaIdIndex); // media id
                b.bucketId = cursor.getString(idIndex);// bucket id
                b.bucketName = cursor.getString(nameIndex);// bucket display name

                Uri baseUri = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
                b.uri = Uri.withAppendedPath(baseUri, b.mediaId);

                if (TextUtils.isEmpty(b.bucketName)) {
                    SLogs.i("skip bucket media -> image media id: " + b.mediaId + ", because bucket display name is null");
                    continue;
                }

                b.isCameraBucket = CAMERA_BUCKET_NAMES_LIST.contains(b.bucketName.toUpperCase());

                buckets.add(b);
            }
        } catch (IllegalArgumentException e) {
            e.printStackTrace();
        } finally {
            cursor.close();
        }

        return buckets;
    }
}