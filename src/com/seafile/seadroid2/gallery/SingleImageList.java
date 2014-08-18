package com.seafile.seadroid2.gallery;

import android.content.ContentResolver;
import android.net.Uri;

import java.util.HashMap;

/**
 * An implementation of interface <code>IImageList</code> which contains only
 * one image.
 */
public class SingleImageList implements IImageList {

    @SuppressWarnings("unused")
    private static final String TAG = "BaseImageList";

    private IImage mSingleImage;
    private Uri mUri;

    public SingleImageList(ContentResolver resolver, Uri uri) {
        mUri = uri;
        mSingleImage = new UriImage(this, resolver, uri);
    }

    @Override
	public HashMap<String, String> getBucketIds() {
        throw new UnsupportedOperationException();
    }

    @Override
	public int getCount() {
        return 1;
    }

    @Override
	public boolean isEmpty() {
        return false;
    }

    @Override
	public int getImageIndex(IImage image) {
        return image == mSingleImage ? 0 : -1;
    }

    @Override
	public IImage getImageAt(int i) {
        return i == 0 ? mSingleImage : null;
    }

    @Override
	public boolean removeImage(IImage image) {
        return false;
    }

    @Override
	public boolean removeImageAt(int index) {
        return false;
    }

    @Override
	public IImage getImageForUri(Uri uri) {
        return uri.equals(mUri) ? mSingleImage : null;
    }

    @Override
	public void close() {
        mSingleImage = null;
        mUri = null;
    }
}
