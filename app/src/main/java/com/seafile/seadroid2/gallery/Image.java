package com.seafile.seadroid2.gallery;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.media.ExifInterface;
import android.net.Uri;
import android.provider.BaseColumns;
import android.provider.MediaStore.Images;
import android.provider.MediaStore.Images.ImageColumns;
import android.util.Log;

import java.io.IOException;

/**
 * The class for normal images in gallery.
 */
public class Image extends BaseImage implements IImage {
    private static final String TAG = "BaseImage";

    private ExifInterface mExif;

    private int mRotation;

    public Image(BaseImageList container, ContentResolver cr,
            long id, int index, Uri uri, String dataPath,
            String mimeType, long dateTaken, String title,
            int rotation) {
        super(container, cr, id, index, uri, dataPath,
                mimeType, dateTaken, title);
        mRotation = rotation;
    }

    @Override
    public int getDegreesRotated() {
        return mRotation;
    }

    protected void setDegreesRotated(int degrees) {
        if (mRotation == degrees) return;
        mRotation = degrees;
        ContentValues values = new ContentValues();
        values.put(ImageColumns.ORIENTATION, mRotation);
        mContentResolver.update(mUri, values, null, null);

        //TODO: Consider invalidate the cursor in container
        // ((BaseImageList) getContainer()).invalidateCursor();
    }

    public boolean isReadonly() {
        String mimeType = getMimeType();
        return !"image/jpeg".equals(mimeType) && !"image/png".equals(mimeType);
    }

    public boolean isDrm() {
        return false;
    }

    /**
     * Replaces the tag if already there. Otherwise, adds to the exif tags.
     * @param tag
     * @param value
     */
    public void replaceExifTag(String tag, String value) {
        if (mExif == null) {
            loadExifData();
        }
        mExif.setAttribute(tag, value);
    }

    private void loadExifData() {
        try {
            mExif = new ExifInterface(mDataPath);
        } catch (IOException ex) {
            Log.e(TAG, "cannot read exif", ex);
        }
    }

    private void saveExifData() throws IOException {
        if (mExif != null) {
            mExif.saveAttributes();
        }
    }

    private void setExifRotation(int degrees) {
        try {
            degrees %= 360;
            if (degrees < 0) degrees += 360;

            int orientation = ExifInterface.ORIENTATION_NORMAL;
            switch (degrees) {
                case 0:
                    orientation = ExifInterface.ORIENTATION_NORMAL;
                    break;
                case 90:
                    orientation = ExifInterface.ORIENTATION_ROTATE_90;
                    break;
                case 180:
                    orientation = ExifInterface.ORIENTATION_ROTATE_180;
                    break;
                case 270:
                    orientation = ExifInterface.ORIENTATION_ROTATE_270;
                    break;
            }

            replaceExifTag(ExifInterface.TAG_ORIENTATION,
                    Integer.toString(orientation));
            saveExifData();
        } catch (Exception ex) {
            Log.e(TAG, "unable to save exif data with new orientation "
                    + fullSizeImageUri(), ex);
        }
    }

    /**
     * Save the rotated image by updating the Exif "Orientation" tag.
     * @param degrees
     */
    public boolean rotateImageBy(int degrees) {
        int newDegrees = (getDegreesRotated() + degrees) % 360;
        setExifRotation(newDegrees);
        setDegreesRotated(newDegrees);

        return true;
    }

    private static final String[] THUMB_PROJECTION = new String[] {
        BaseColumns._ID,
    };

    public Bitmap thumbBitmap(boolean rotateAsNeeded) {
        Bitmap bitmap = null;
        BitmapFactory.Options options = new BitmapFactory.Options();
        options.inDither = false;
        options.inPreferredConfig = Bitmap.Config.ARGB_8888;
        bitmap = BitmapManager.instance().getThumbnail(mContentResolver, mId,
                Images.Thumbnails.MINI_KIND, options, false);

        if (bitmap != null && rotateAsNeeded) {
            bitmap = Util.rotate(bitmap, getDegreesRotated());
        }

        return bitmap;
    }
}