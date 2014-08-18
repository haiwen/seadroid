package com.seafile.seadroid2.gallery;

import android.content.ContentResolver;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.net.Uri;
import android.os.ParcelFileDescriptor;
import android.util.Log;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.InputStream;

class UriImage implements IImage {
    private static final String TAG = "UriImage";
    private final Uri mUri;
    private final IImageList mContainer;
    private final ContentResolver mContentResolver;

    UriImage(IImageList container, ContentResolver cr, Uri uri) {
        mContainer = container;
        mContentResolver = cr;
        mUri = uri;
    }

    @Override
	public int getDegreesRotated() {
        return 0;
    }

    @Override
	public String getDataPath() {
        return mUri.getPath();
    }

    private InputStream getInputStream() {
        try {
            if (mUri.getScheme().equals("file")) {
                return new java.io.FileInputStream(mUri.getPath());
            } else {
                return mContentResolver.openInputStream(mUri);
            }
        } catch (FileNotFoundException ex) {
            return null;
        }
    }

    private ParcelFileDescriptor getPFD() {
        try {
            if (mUri.getScheme().equals("file")) {
                String path = mUri.getPath();
                return ParcelFileDescriptor.open(new File(path),
                        ParcelFileDescriptor.MODE_READ_ONLY);
            } else {
                return mContentResolver.openFileDescriptor(mUri, "r");
            }
        } catch (FileNotFoundException ex) {
            return null;
        }
    }

    @Override
	public Bitmap fullSizeBitmap(int minSideLength, int maxNumberOfPixels) {
        return fullSizeBitmap(minSideLength, maxNumberOfPixels,
                IImage.ROTATE_AS_NEEDED, IImage.NO_NATIVE);
    }

    public Bitmap fullSizeBitmap(int minSideLength, int maxNumberOfPixels,
            boolean rotateAsNeeded) {
        return fullSizeBitmap(minSideLength, maxNumberOfPixels,
                rotateAsNeeded, IImage.NO_NATIVE);
    }

    @Override
	public Bitmap fullSizeBitmap(int minSideLength, int maxNumberOfPixels,
            boolean rotateAsNeeded, boolean useNative) {
        try {
            ParcelFileDescriptor pfdInput = getPFD();
            Bitmap b = Util.makeBitmap(minSideLength, maxNumberOfPixels,
                    pfdInput, useNative);
            return b;
        } catch (Exception ex) {
            Log.e(TAG, "got exception decoding bitmap ", ex);
            return null;
        }
    }

    @Override
	public Uri fullSizeImageUri() {
        return mUri;
    }

    @Override
	public InputStream fullSizeImageData() {
        return getInputStream();
    }

    @Override
	public Bitmap miniThumbBitmap() {
        return thumbBitmap(IImage.ROTATE_AS_NEEDED);
    }

    @Override
	public String getTitle() {
        return mUri.toString();
    }

    @Override
	public Bitmap thumbBitmap(boolean rotateAsNeeded) {
        return fullSizeBitmap(THUMBNAIL_TARGET_SIZE, THUMBNAIL_MAX_NUM_PIXELS,
                rotateAsNeeded);
    }

    private BitmapFactory.Options snifBitmapOptions() {
        ParcelFileDescriptor input = getPFD();
        if (input == null) return null;
        try {
            BitmapFactory.Options options = new BitmapFactory.Options();
            options.inJustDecodeBounds = true;
            BitmapManager.instance().decodeFileDescriptor(
                    input.getFileDescriptor(), options);
            return options;
        } finally {
            Util.closeSilently(input);
        }
    }

    @Override
	public String getMimeType() {
        BitmapFactory.Options options = snifBitmapOptions();
        return (options != null && options.outMimeType != null)
                ? options.outMimeType
                : "";
    }

    @Override
	public int getHeight() {
        BitmapFactory.Options options = snifBitmapOptions();
        return (options != null) ? options.outHeight : 0;
    }

    @Override
	public int getWidth() {
        BitmapFactory.Options options = snifBitmapOptions();
        return (options != null) ? options.outWidth : 0;
    }

    @Override
	public IImageList getContainer() {
        return mContainer;
    }

    @Override
	public long getDateTaken() {
        return 0;
    }

    @Override
	public boolean isReadonly() {
        return true;
    }

    @Override
	public boolean isDrm() {
        return false;
    }

    @Override
	public boolean rotateImageBy(int degrees) {
        return false;
    }
}
