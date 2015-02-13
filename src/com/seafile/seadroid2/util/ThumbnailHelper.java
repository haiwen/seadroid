package com.seafile.seadroid2.util;

import android.content.res.Resources;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.drawable.BitmapDrawable;
import android.graphics.drawable.Drawable;
import android.os.AsyncTask;
import android.widget.ImageView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.gallery.LruCache;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.lang.ref.WeakReference;

/**
 * Helper class to asynchronically download and display thumbnails
 *
 * Most of the code comes from:
 * http://android-developers.blogspot.de/2010/07/multithreading-for-performance.html
 *
 */
public class ThumbnailHelper {

    private LruCache<String, Bitmap> mMemoryCache;

    // use 512kb memory for the thumbnails memory cache.
    private final int cacheSize = 512;

    public ThumbnailHelper() {

        mMemoryCache = new LruCache<String, Bitmap>(cacheSize);

    }

    private void addBitmapToMemoryCache(String key, Bitmap bitmap) {
        if (getBitmapFromMemCache(key) == null) {
            mMemoryCache.put(key, bitmap);
        }
    }

    private Bitmap getBitmapFromMemCache(String key) {
        return mMemoryCache.get(key);
    }

    private static String buildCacheKey(String repoId, String path) {
        return String.valueOf(repoId+"/"+path);

    }

    /**
     * Download thumbnail for the file given and put it into the imageView given.
     *
     * @param dataManager
     * @param imageView
     * @param repoId
     * @param filePath
     */
    public void addThumbnailAsync(DataManager dataManager, ImageView imageView, String repoName, String repoId, String filePath) {

        // Objects are reused through scrolling. So abort any pending downloads on this imageView
        cancelPotentialDownload(imageView);

        final Bitmap bitmap = getBitmapFromMemCache(buildCacheKey(repoId, filePath));
        if (bitmap != null) {
            imageView.setImageBitmap(bitmap);
        } else {
            // Objects are reused through scrolling. So abort any pending downloads on this imageView
            cancelPotentialDownload(imageView);

            DownloadThumbnailTask task = new DownloadThumbnailTask(dataManager, imageView);
            DownloadedDrawable downloadedDrawable = new DownloadedDrawable(task, imageView.getResources());
            imageView.setImageDrawable(downloadedDrawable);
            task.execute(repoName, repoId, filePath);
        }
    }

    private static class DownloadedDrawable extends BitmapDrawable {
        private final WeakReference<DownloadThumbnailTask> bitmapDownloaderTaskReference;

        public DownloadedDrawable(DownloadThumbnailTask bitmapDownloaderTask, Resources r) {
            super(r, BitmapFactory.decodeResource(r, R.drawable.file_image));
            bitmapDownloaderTaskReference =
                    new WeakReference<DownloadThumbnailTask>(bitmapDownloaderTask);
        }

        public DownloadThumbnailTask getBitmapDownloaderTask() {
            return bitmapDownloaderTaskReference.get();
        }
    }

    private static DownloadThumbnailTask getBitmapDownloaderTask(ImageView imageView) {
        if (imageView != null) {
            Drawable drawable = imageView.getDrawable();
            if (drawable instanceof DownloadedDrawable) {
                DownloadedDrawable downloadedDrawable = (DownloadedDrawable)drawable;
                return downloadedDrawable.getBitmapDownloaderTask();
            }
        }
        return null;
    }

    private void cancelPotentialDownload(ImageView imageView) {
        DownloadThumbnailTask bitmapDownloaderTask = getBitmapDownloaderTask(imageView);

        if (bitmapDownloaderTask != null) {
            bitmapDownloaderTask.cancel(true);
        }
    }

    private class DownloadThumbnailTask extends AsyncTask<String, Void, Bitmap> {

        private final WeakReference<ImageView> viewholderRef;
        private final WeakReference<DataManager> datamanagerRef;

        DownloadThumbnailTask(DataManager dataManager, ImageView viewholder) {
            viewholderRef = new WeakReference<ImageView>(viewholder);
            datamanagerRef = new WeakReference<DataManager>(dataManager);
        }

        protected Bitmap doInBackground(String... params) {

            final int THUMBNAIL_SIZE = DataManager.caculateThumbnailSizeOfDevice();
            DataManager dm = datamanagerRef.get();

            String repoName = params[0];
            String repoId = params[1];
            String path = params[2];

            // delay actual download a bit. otherwise fast scrolling will cause a network fetch
            // for every entry that flowed past the screen. this way, the cancelling will stop the
            // task before the http request has been made
            if (!dm.isThumbnailCached(repoId, path, THUMBNAIL_SIZE)) {
                try {
                    Thread.sleep(200);
                } catch (InterruptedException e) {
                }
            }

            if (!isCancelled() && dm != null) {
                File thumb = dm.getThumbnail(repoName, repoId, path, THUMBNAIL_SIZE);

                try {
                    FileInputStream in = new FileInputStream(thumb);
                    Bitmap b = BitmapFactory.decodeStream(in);
                    addBitmapToMemoryCache(String.valueOf(buildCacheKey(repoId, path)), b);
                    return b;

                } catch (FileNotFoundException e) {
                    return null;
                }
            } else {
                return null;
            }
        }

        protected void onPostExecute(Bitmap result) {
            ImageView v = viewholderRef.get();
            if (!isCancelled() && v != null && result != null) {
                DownloadThumbnailTask bitmapDownloaderTask = getBitmapDownloaderTask(v);
                // Change bitmap only if this process is still associated with it
                if (this == bitmapDownloaderTask) {
                    v.setImageBitmap(result);
                }
            }
        }
    }

}
