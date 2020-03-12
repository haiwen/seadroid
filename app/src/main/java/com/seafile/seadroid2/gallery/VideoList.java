package com.seafile.seadroid2.gallery;

import java.util.HashMap;
import java.util.List;

import android.content.ContentResolver;
import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore.Images;
import android.provider.MediaStore.Video.Media;

import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;

/**
 * A collection of all the <code>VideoObject</code> in gallery.
 */
public class VideoList extends BaseImageList {

    @SuppressWarnings("unused")
    private static final String TAG = "BaseImageList";

    private static final String[] VIDEO_PROJECTION = new String[] {
            Media._ID,
            Media.DATA,
            Media.DATE_TAKEN,
            Media.TITLE,
            Media.MINI_THUMB_MAGIC,
            Media.MIME_TYPE,
            Media.DATE_MODIFIED};

    private static final int INDEX_ID = 0;
    private static final int INDEX_DATA_PATH = 1;
    private static final int INDEX_DATE_TAKEN = 2;
    private static final int INDEX_TITLE = 3;
    private static final int INDEX_MIMI_THUMB_MAGIC = 4;
    private static final int INDEX_MIME_TYPE = 5;
    private static final int INDEX_DATE_MODIFIED = 6;

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
        long miniThumbMagic = cursor.getLong(INDEX_MIMI_THUMB_MAGIC);
        String title = cursor.getString(INDEX_TITLE);
        String mimeType = cursor.getString(INDEX_MIME_TYPE);
        if (title == null || title.length() == 0) {
            title = dataPath;
        }
        return new VideoObject(this, mContentResolver,
                id, cursor.getPosition(), contentUri(id), dataPath,
                mimeType, dateTaken, title);
    }

    public VideoList(ContentResolver resolver, Uri uri, int sort,
            String bucketId) {
        super(resolver, uri, sort, bucketId);
    }

    public HashMap<String, String> getBucketIds() {
        Uri uri = mBaseUri.buildUpon()
                .appendQueryParameter("distinct", "true").build();
        Cursor c = Images.Media.query(
                mContentResolver, uri,
                new String[] {
                    Media.BUCKET_DISPLAY_NAME,
                    Media.BUCKET_ID
                },
                whereClause(), whereClauseArgs(), sortOrder());
        try {
            HashMap<String, String> hash = Maps.newHashMap();
            while (c.moveToNext()) {
                hash.put(c.getString(1), c.getString(0));
            }
            return hash;
        } finally {
            c.close();
        }
    }

    protected String whereClause() {
        int count = ImageManager.getAllBucketIds().size();
        List<String> chars = Lists.newArrayList();
        for (int i = 0; i < count; i++) {
            chars.add("?");
        }

        String clause = Media.BUCKET_ID + " in " + "(" + Joiner.on(", ").join(chars) + ")";

        return clause;
    }

    protected String[] whereClauseArgs() {
        return Iterables.toArray(ImageManager.getAllBucketIds(), String.class);
    }

    @Override
    protected Cursor createCursor() {
        Cursor c = Images.Media.query(
                mContentResolver, mBaseUri, VIDEO_PROJECTION,
                whereClause(), whereClauseArgs(), sortOrder());
        return c;
    }
}
