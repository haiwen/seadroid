package com.seafile.seadroid2.util;

import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.media.ThumbnailUtils;

public class BitmapUtil {
    private BitmapUtil() {}

    /**
     * This function and decodeSampledBitmapFromStream below are used to
     * generate thumbnails for pictures without leading to Out of memory
     * problem.
     *
     * @see http://developer.android.com/training/displaying-bitmaps/load-bitmap.html
     *
     */
    public static int calculateInSampleSize(BitmapFactory.Options options, int reqWidth, int reqHeight) {
        // Raw height and width of image
        final int height = options.outHeight;
        final int width = options.outWidth;
        int inSampleSize = 1;

        if (height > reqHeight || width > reqWidth) {

            final int halfHeight = height / 2;
            final int halfWidth = width / 2;

            // Calculate the largest inSampleSize value that is a power of 2 and keeps both
            // height and width larger than the requested height and width.
            while ((halfHeight / inSampleSize) > reqHeight
                   && (halfWidth / inSampleSize) > reqWidth) {
                inSampleSize *= 2;
            }
        }

        return inSampleSize;
    }

    public static Bitmap calculateThumbnail(String filePath, int reqWidth, int reqHeight) {

        // First decode with inJustDecodeBounds=true to check dimensions
        final BitmapFactory.Options options = new BitmapFactory.Options();
        options.inJustDecodeBounds = true;
        BitmapFactory.decodeFile(filePath, options);

        // Calculate inSampleSize
        options.inSampleSize = calculateInSampleSize(options, reqWidth, reqHeight);

        // Decode bitmap with inSampleSize set
        options.inJustDecodeBounds = false;
        // return BitmapFactory.decodeResource(res, resId, options);
        Bitmap bmp = BitmapFactory.decodeFile(filePath, options);
        return ThumbnailUtils.extractThumbnail(bmp, reqWidth, reqHeight);
    }

    public static Bitmap calculateThumbnail(String filePath, int size) {
        return calculateThumbnail(filePath, size, size);
    }
}
