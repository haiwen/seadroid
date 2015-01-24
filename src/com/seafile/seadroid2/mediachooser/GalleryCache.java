package com.seafile.seadroid2.mediachooser;

import android.graphics.Bitmap;
import android.media.ThumbnailUtils;
import android.os.AsyncTask;
import android.provider.MediaStore.Video.Thumbnails;
import android.support.v4.app.Fragment;
import android.support.v4.util.LruCache;
import android.widget.ImageView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.mediachooser.adapter.BucketGridAdapter;
import com.seafile.seadroid2.mediachooser.adapter.GridViewAdapter;
import com.seafile.seadroid2.mediachooser.fragment.VideoFragment;

import java.util.ArrayList;


public class GalleryCache {
    private LruCache<String, Bitmap> mBitmapCache;
    private ArrayList<String> mCurrentTasks;
    private int mMaxWidth;

    public GalleryCache(int size, int maxWidth, int maxHeight) {
        mMaxWidth = maxWidth;

        mBitmapCache = new LruCache<String, Bitmap>(size) {
            @Override
            protected int sizeOf(String key, Bitmap b) {
                // Assuming that one pixel contains four bytes.
                return b.getHeight() * b.getWidth() * 4;
            }
        };

        mCurrentTasks = new ArrayList<String>();
    }

    private void addBitmapToCache(String key, Bitmap bitmap) {
        if (getBitmapFromCache(key) == null) {
            mBitmapCache.put(key, bitmap);
        }
    }

    private Bitmap getBitmapFromCache(String key) {
        return mBitmapCache.get(key);
    }

    /**
     * Gets a bitmap from cache. <br/>
     * <br/>
     * If it is not in cache, this method will: <br/>
     * <b>1:</b> check if the bitmap url is currently being processed in the
     * BitmapLoaderTask and cancel if it is already in a task (a control to see
     * if it's inside the currentTasks list). <br/>
     * <b>2:</b> check if an internet connection is available and continue if
     * so. <br/>
     * <b>3:</b> download the bitmap, scale the bitmap if necessary and put it
     * into the memory cache. <br/>
     * <b>4:</b> Remove the bitmap url from the currentTasks list. <br/>
     * <b>5:</b> Notify the ListAdapter.
     *
     * @param mainActivity - Reference to activity object, in order to call
     *                     notifyDataSetChanged() on the ListAdapter.
     * @param imageKey     - The bitmap url (will be the key).
     * @param imageView    - The ImageView that should get an available bitmap or a
     *                     placeholder image.
     * @param isScrolling  - If set to true, we skip executing more tasks since the user
     *                     probably has flinged away the view.
     */
    public void loadBitmap(Fragment mainActivity, String imageKey,
                           ImageView imageView, boolean isScrolling) {
        final Bitmap bitmap = getBitmapFromCache(imageKey);


        if (bitmap != null) {
            imageView.setImageBitmap(bitmap);
        } else {
            imageView.setImageResource(R.drawable.ic_loading);
            //			imageView.setImageResource(R.drawable.transprent_drawable);
            if (!isScrolling && !mCurrentTasks.contains(imageKey)) {
                if (mainActivity instanceof VideoFragment) {
                    BitmapLoaderTask task = new BitmapLoaderTask(imageKey, ((VideoFragment) mainActivity).getAdapter());
                    task.execute();

                } /*else if (mainActivity instanceof BucketVideoFragment) {
                    BitmapLoaderTask task = new BitmapLoaderTask(imageKey, ((BucketVideoFragment) mainActivity).getAdapter());
                    task.execute();
                }*/
            }
        }
    }

    private class BitmapLoaderTask extends AsyncTask<Void, Void, Bitmap> {
        private GridViewAdapter mAdapter;
        private BucketGridAdapter mBucketGridAdapter;
        private String mImageKey;

        public BitmapLoaderTask(String imageKey, GridViewAdapter adapter) {
            mAdapter = adapter;
            mImageKey = imageKey;
        }

        public BitmapLoaderTask(String imageKey, BucketGridAdapter adapter) {
            mBucketGridAdapter = adapter;
            mImageKey = imageKey;
        }

        @Override
        protected void onPreExecute() {
            mCurrentTasks.add(mImageKey);
        }

        @Override
        protected Bitmap doInBackground(Void... params) {
            Bitmap bitmap = null;
            try {
                bitmap = ThumbnailUtils.createVideoThumbnail(mImageKey, Thumbnails.FULL_SCREEN_KIND);

                if (bitmap != null) {
                    bitmap = Bitmap.createScaledBitmap(bitmap, mMaxWidth, mMaxWidth, false);
                    addBitmapToCache(mImageKey, bitmap);
                    return bitmap;
                }
                return null;
            } catch (Exception e) {
                if (e != null) {
                    e.printStackTrace();
                }
                return null;
            }
        }

        @Override
        protected void onPostExecute(Bitmap param) {
            mCurrentTasks.remove(mImageKey);
            if (param != null) {
                if (mAdapter != null) {
                    mAdapter.notifyDataSetChanged();
                } else {
                    mBucketGridAdapter.notifyDataSetChanged();
                }
            }
        }
    }

    public void clear() {
        mBitmapCache.evictAll();
    }


}
