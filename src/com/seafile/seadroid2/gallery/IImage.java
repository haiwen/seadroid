package com.seafile.seadroid2.gallery;

import android.graphics.Bitmap;
import android.net.Uri;

import java.io.InputStream;

/**
 * The interface of all images used in gallery.
 */
public interface IImage {
    static final int THUMBNAIL_TARGET_SIZE = 320;
    static final int MINI_THUMB_TARGET_SIZE = 96;
    static final int THUMBNAIL_MAX_NUM_PIXELS = 512 * 384;
    static final int MINI_THUMB_MAX_NUM_PIXELS = 128 * 128;
    static final int UNCONSTRAINED = -1;

    /** Get the image list which contains this image. */
    IImageList getContainer();

    /** Get the bitmap for the full size image. */
    Bitmap fullSizeBitmap(int minSideLength, int maxNumberOfPixels);
    Bitmap fullSizeBitmap(int minSideLength, int maxNumberOfPixels, boolean rotateAsNeeded, boolean useNative);
    int getDegreesRotated();
    static final boolean ROTATE_AS_NEEDED = true;
    static final boolean NO_ROTATE = false;
    static final boolean USE_NATIVE = true;
    static final boolean NO_NATIVE = false;

    /** Get the input stream associated with a given full size image. */
    InputStream fullSizeImageData();
    Uri fullSizeImageUri();

    /** Get the path of the (full size) image data. */
    String getDataPath();

    // Get the title of the image
    String getTitle();

    // Get metadata of the image
    long getDateTaken();

    String getMimeType();

    int getWidth();

    int getHeight();

    // Get property of the image
    boolean isReadonly();
    boolean isDrm();

    // Get the bitmap of the medium thumbnail
    Bitmap thumbBitmap(boolean rotateAsNeeded);

    // Get the bitmap of the mini thumbnail.
    Bitmap miniThumbBitmap();

    // Rotate the image
    boolean rotateImageBy(int degrees);
}
