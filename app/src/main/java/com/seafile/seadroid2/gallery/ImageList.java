package com.seafile.seadroid2.gallery;

import java.util.HashMap;
import java.util.List;

import android.content.ContentResolver;
import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore.Images.Media;

import com.google.common.base.Joiner;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

/**
 * Represents an ordered collection of Image objects. Provides an API to add
 * and remove an image.
 */
public class ImageList extends BaseImageList implements IImageList {

    @SuppressWarnings("unused")
    private static final String TAG = "ImageList";

    private static final String[] ACCEPTABLE_IMAGE_TYPES =
            new String[] { "image/jpeg", "image/png", "image/gif" };

    public HashMap<String, String> getBucketIds() {
        Uri uri = mBaseUri.buildUpon()
                .appendQueryParameter("distinct", "true").build();
        Cursor cursor = Media.query(
                mContentResolver, uri,
                new String[] {
                    Media.BUCKET_DISPLAY_NAME,
                    Media.BUCKET_ID},
                whereClause(), whereClauseArgs(), null);
        try {
            HashMap<String, String> hash = Maps.newHashMap();
            while (cursor.moveToNext()) {
                hash.put(cursor.getString(1), cursor.getString(0));
            }
            return hash;
        } finally {
            cursor.close();
        }
    }

    /**
     * ImageList constructor.
     */
    public ImageList(ContentResolver resolver, Uri imageUri,
            int sort, String bucketId) {
        super(resolver, imageUri, sort, bucketId);
    }

    private static final String WHERE_CLAUSE =
            "(" + Media.MIME_TYPE + " in (?, ?, ?))";

    protected String whereClause() {
        int count = ImageManager.getAllBucketIds().size();
        List<String> chars = Lists.newArrayList();
        for (int i = 0; i < count; i++) {
            chars.add("?");
        }

        String clause = WHERE_CLAUSE + " AND "
            + Media.BUCKET_ID + " in " + "(" + Joiner.on(", ").join(chars) + ")";

        return clause;
    }

    protected String[] whereClauseArgs() {
        int count = ACCEPTABLE_IMAGE_TYPES.length;
        List<String> ids = ImageManager.getAllBucketIds();
        int idsCount = ids.size();
        String[] result = new String[count + idsCount];
        System.arraycopy(ACCEPTABLE_IMAGE_TYPES, 0, result, 0, count);
        for (int i = 0; i < idsCount; i++) {
            result[count + i] = ids.get(i);
        }
        return result;
    }

    @Override
    protected Cursor createCursor() {
        Cursor c = Media.query(
                mContentResolver, mBaseUri, IMAGE_PROJECTION,
                whereClause(), whereClauseArgs(), sortOrder());
        return c;
    }

    static final String[] IMAGE_PROJECTION = new String[] {
            Media._ID,
            Media.DATA,
            Media.DATE_TAKEN,
            Media.MINI_THUMB_MAGIC,
            Media.ORIENTATION,
            Media.TITLE,
            Media.MIME_TYPE,
            Media.DATE_MODIFIED};

    private static final int INDEX_ID = 0;
    private static final int INDEX_DATA_PATH = 1;
    private static final int INDEX_DATE_TAKEN = 2;
    private static final int INDEX_MINI_THUMB_MAGIC = 3;
    private static final int INDEX_ORIENTATION = 4;
    private static final int INDEX_TITLE = 5;
    private static final int INDEX_MIME_TYPE = 6;
    private static final int INDEX_DATE_MODIFIED = 7;

    @Override
    protected long getImageId(Cursor cursor) {
        return cursor.getLong(INDEX_ID);
    }

    @Override
    protected BaseImage loadImageFromCursor(Cursor cursor) {
        long id = cursor.getLong(INDEX_ID);
        String dataPath = cursor.getString(INDEX_DATA_PATH);
        long dateTaken = cursor.getLong(INDEX_DATE_TAKEN);
        if (dateTaken == 0) {
            dateTaken = cursor.getLong(INDEX_DATE_MODIFIED) * 1000;
        }
        long miniThumbMagic = cursor.getLong(INDEX_MINI_THUMB_MAGIC);
        int orientation = cursor.getInt(INDEX_ORIENTATION);
        String title = cursor.getString(INDEX_TITLE);
        String mimeType = cursor.getString(INDEX_MIME_TYPE);
        if (title == null || title.length() == 0) {
            title = dataPath;
        }
        return new Image(this, mContentResolver, id, cursor.getPosition(),
                contentUri(id), dataPath, mimeType, dateTaken, title,
                orientation);
    }
}
