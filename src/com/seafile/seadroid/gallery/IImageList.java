package com.seafile.seadroid.gallery;

import android.net.Uri;

import java.util.HashMap;

//
// ImageList and Image classes have one-to-one correspondence.
// The class hierarchy (* = abstract class):
//
//    IImageList
//    - BaseImageList (*)
//      - VideoList
//      - ImageList
//        - DrmImageList
//    - SingleImageList (contains UriImage)
//    - ImageListUber
//
//    IImage
//    - BaseImage (*)
//      - VideoObject
//      - Image
//        - DrmImage
//    - UriImage
//

/**
 * The interface of all image collections used in gallery.
 */
public interface IImageList {
    public HashMap<String, String> getBucketIds();

    /**
     * Returns the count of image objects.
     *
     * @return       the number of images
     */
    public int getCount();

    /**
     * @return true if the count of image objects is zero.
     */
    public boolean isEmpty();

    /**
     * Returns the image at the ith position.
     *
     * @param i     the position
     * @return      the image at the ith position
     */
    public IImage getImageAt(int i);

    /**
     * Returns the image with a particular Uri.
     *
     * @param uri
     * @return      the image with a particular Uri. null if not found.
     */
    public IImage getImageForUri(Uri uri);

    /**
     *
     * @param image
     * @return true if the image was removed.
     */
    public boolean removeImage(IImage image);

    /**
     * Removes the image at the ith position.
     * @param i     the position
     */
    public boolean removeImageAt(int i);

    public int getImageIndex(IImage image);

    /**
     * Closes this list to release resources, no further operation is allowed.
     */
    public void close();
}