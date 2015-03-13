package com.seafile.seadroid2.mediachooser.async;

import android.app.ActivityManager;
import android.content.Context;
import android.support.v4.app.Fragment;
import android.widget.ImageView;
import com.seafile.seadroid2.mediachooser.GalleryCache;
import com.seafile.seadroid2.mediachooser.GalleryRetainCache;

public class VideoLoadAsync extends MediaAsync<String, String, String> {

    public Fragment fragment;

    private ImageView mImageView;
    private static GalleryCache mCache;
    private boolean mIsScrolling;
    private int mWidth;


    public VideoLoadAsync(Fragment fragment, ImageView imageView, boolean isScrolling, int width) {
        mImageView = imageView;
        this.fragment = fragment;
        mWidth = width;
        mIsScrolling = isScrolling;

        final int memClass = ((ActivityManager) fragment.getActivity().getSystemService(Context.ACTIVITY_SERVICE))
                .getMemoryClass();
        final int size = 1024 * 1024 * memClass / 8;

        // Handle orientation change.
        GalleryRetainCache c = GalleryRetainCache.getOrCreateRetainableCache();
        mCache = c.mRetainedCache;

        if (mCache == null) {
            // The maximum bitmap pixels allowed in respective direction.
            // If exceeding, the cache will automatically scale the
            // bitmaps.
            /*	final int MAX_PIXELS_WIDTH  = 100;
			final int MAX_PIXELS_HEIGHT = 100;*/
            mCache = new GalleryCache(size, mWidth, mWidth);
            c.mRetainedCache = mCache;
        }
    }

    @Override
    protected String doInBackground(String... params) {
        String url = params[0].toString();
        return url;
    }

    @Override
    protected void onPostExecute(String result) {
        mCache.loadBitmap(fragment, result, mImageView, mIsScrolling);
    }

}
