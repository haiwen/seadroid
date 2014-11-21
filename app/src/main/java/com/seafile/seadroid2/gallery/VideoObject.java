package com.seafile.seadroid2.gallery;

import android.content.ContentResolver;
import android.graphics.Bitmap;
import android.media.ThumbnailUtils;
import android.net.Uri;
import android.provider.MediaStore.Images;
import android.provider.MediaStore.Video;
import android.util.Log;

import java.io.IOException;
import java.io.InputStream;

/**
 * Represents a particular video and provides access to the underlying data and
 * two thumbnail bitmaps as well as other information such as the id, and the
 * path to the actual video data.
 */
public class VideoObject extends BaseImage implements IImage {
    private static final String TAG = "VideoObject";
    /**
     * Constructor.
     *
     * @param id        the image id of the image
     * @param cr        the content resolver
     */
    protected VideoObject(BaseImageList container, ContentResolver cr,
            long id, int index, Uri uri, String dataPath,
            String mimeType, long dateTaken, String title) {
        super(container, cr, id, index, uri, dataPath,
                mimeType, dateTaken, title);
    }

    @Override
    public boolean equals(Object other) {
        if (other == null || !(other instanceof VideoObject)) return false;
        return fullSizeImageUri().equals(
                ((VideoObject) other).fullSizeImageUri());
    }

    @Override
    public int hashCode() {
        return fullSizeImageUri().toString().hashCode();
    }

    @Override
    public Bitmap fullSizeBitmap(int minSideLength, int maxNumberOfPixels,
            boolean rotateAsNeeded, boolean useNative) {
        return ThumbnailUtils.createVideoThumbnail(mDataPath,
                Video.Thumbnails.MINI_KIND);
    }

    @Override
    public InputStream fullSizeImageData() {
        try {
            InputStream input = mContentResolver.openInputStream(
                    fullSizeImageUri());
            return input;
        } catch (IOException ex) {
            return null;
        }
    }

    @Override
    public int getHeight() {
         return 0;
    }

    @Override
    public int getWidth() {
        return 0;
    }

    public boolean isReadonly() {
        return false;
    }

    public boolean isDrm() {
        return false;
    }

    public boolean rotateImageBy(int degrees) {
       return false;
    }

    public Bitmap thumbBitmap(boolean rotateAsNeeded) {
        return fullSizeBitmap(THUMBNAIL_TARGET_SIZE, THUMBNAIL_MAX_NUM_PIXELS);
    }

    @Override
    public Bitmap miniThumbBitmap() {
        try {
            long id = mId;
            return BitmapManager.instance().getThumbnail(mContentResolver,
                    id, Images.Thumbnails.MICRO_KIND, null, true);
        } catch (Throwable ex) {
            Log.e(TAG, "miniThumbBitmap got exception", ex);
            return null;
        }
    }

    @Override
    public String toString() {
        return new StringBuilder("VideoObject").append(mId).toString();
    }
}
