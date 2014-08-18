package com.seafile.seadroid2.gallery;

import android.content.ContentResolver;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.ParcelFileDescriptor;
import android.provider.MediaStore.Images;
import android.util.Log;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

/**
 * Represents a particular image and provides access to the underlying bitmap
 * and two thumbnail bitmaps as well as other information such as the id, and
 * the path to the actual image data.
 */
public abstract class BaseImage implements IImage {
    private static final String TAG = "BaseImage";
    private static final int UNKNOWN_LENGTH = -1;
    protected ContentResolver mContentResolver;

    // Database field
    protected Uri mUri;
    protected long mId;
    protected String mDataPath;
    protected final int mIndex;
    protected String mMimeType;
    private final long mDateTaken;
    private String mTitle;

    protected BaseImageList mContainer;

    private int mWidth = UNKNOWN_LENGTH;
    private int mHeight = UNKNOWN_LENGTH;

    protected BaseImage(BaseImageList container, ContentResolver cr,
            long id, int index, Uri uri, String dataPath, String mimeType,
            long dateTaken, String title) {
        mContainer = container;
        mContentResolver = cr;
        mId = id;
        mIndex = index;
        mUri = uri;
        mDataPath = dataPath;
        mMimeType = mimeType;
        mDateTaken = dateTaken;
        mTitle = title;
    }

    @Override
	public String getDataPath() {
        return mDataPath;
    }

    @Override
    public boolean equals(Object other) {
        if (other == null || !(other instanceof Image)) return false;
        return mUri.equals(((Image) other).mUri);
    }

    @Override
    public int hashCode() {
        return mUri.hashCode();
    }

    @Override
	public Bitmap fullSizeBitmap(int minSideLength, int maxNumberOfPixels) {
        return fullSizeBitmap(minSideLength, maxNumberOfPixels,
                IImage.ROTATE_AS_NEEDED, IImage.NO_NATIVE);
    }

    @Override
	public Bitmap fullSizeBitmap(int minSideLength, int maxNumberOfPixels,
            boolean rotateAsNeeded, boolean useNative) {
        Uri url = mContainer.contentUri(mId);
        if (url == null) return null;

        Bitmap b = Util.makeBitmap(minSideLength, maxNumberOfPixels,
                url, mContentResolver, useNative);

        if (b != null && rotateAsNeeded) {
            b = Util.rotate(b, getDegreesRotated());
        }

        return b;
    }

    @Override
	public InputStream fullSizeImageData() {
        try {
            InputStream input = mContentResolver.openInputStream(mUri);
            return input;
        } catch (IOException ex) {
            return null;
        }
    }

    @Override
	public Uri fullSizeImageUri() {
        return mUri;
    }

    @Override
	public IImageList getContainer() {
        return mContainer;
    }

    @Override
	public long getDateTaken() {
        return mDateTaken;
    }

    @Override
	public int getDegreesRotated() {
        return 0;
    }

    @Override
	public String getMimeType() {
        return mMimeType;
    }

    @Override
	public String getTitle() {
        return mTitle;
    }

    private void setupDimension() {
        ParcelFileDescriptor input = null;
        try {
            input = mContentResolver.openFileDescriptor(mUri, "r");
            BitmapFactory.Options options = new BitmapFactory.Options();
            options.inJustDecodeBounds = true;
            BitmapManager.instance().decodeFileDescriptor(
                    input.getFileDescriptor(), options);
            mWidth = options.outWidth;
            mHeight = options.outHeight;
        } catch (FileNotFoundException ex) {
            mWidth = 0;
            mHeight = 0;
        } finally {
            Util.closeSilently(input);
        }
    }

    @Override
	public int getWidth() {
        if (mWidth == UNKNOWN_LENGTH) setupDimension();
        return mWidth;
    }

    @Override
	public int getHeight() {
        if (mHeight == UNKNOWN_LENGTH) setupDimension();
        return mHeight;
    }

    @Override
	public Bitmap miniThumbBitmap() {
        Bitmap b = null;
        try {
            long id = mId;
            b = BitmapManager.instance().getThumbnail(mContentResolver, id,
                    Images.Thumbnails.MICRO_KIND, null, false);
        } catch (Throwable ex) {
            Log.e(TAG, "miniThumbBitmap got exception", ex);
            return null;
        }
        if (b != null) {
            b = Util.rotate(b, getDegreesRotated());
        }
        return b;
    }

    protected void onRemove() {
    }

    @Override
    public String toString() {
        return mUri.toString();
    }
}
