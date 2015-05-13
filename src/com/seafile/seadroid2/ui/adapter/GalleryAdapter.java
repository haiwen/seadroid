package com.seafile.seadroid2.ui.adapter;

import android.graphics.Bitmap;
import android.support.v4.view.PagerAdapter;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.ProgressBar;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.assist.FailReason;
import com.nostra13.universalimageloader.core.listener.ImageLoadingListener;
import com.nostra13.universalimageloader.core.listener.ImageLoadingProgressListener;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.ui.activity.GalleryActivity;
import uk.co.senab.photoview.PhotoView;
import uk.co.senab.photoview.PhotoViewAttacher;

import java.util.ArrayList;

/**
 * Gallery Adapter
 */
public class GalleryAdapter extends PagerAdapter {
    public static final String DEBUG_TAG = "GalleryAdapter";

    private GalleryActivity mActivity;
    private ArrayList<String> mPhotoLinks;
    private LayoutInflater inflater;
    private DisplayImageOptions options;

    public GalleryAdapter(GalleryActivity context, Account account,
                          ArrayList<String> photoUrls) {
        mActivity = context;
        mPhotoLinks = photoUrls;
        inflater = context.getLayoutInflater();
        options = new DisplayImageOptions.Builder()
                .showImageForEmptyUri(R.drawable.ic_gallery_empty2)
                .showImageOnFail(android.R.drawable.stat_notify_error)
                .cacheInMemory(true)
                .cacheOnDisk(true)
                .considerExifParams(true)
                .extraForDownloader(account)
                .build();
    }

    @Override
    public int getCount() {
        return mPhotoLinks.size();
    }

    public String getItem(int position) {
        return mPhotoLinks.get(position);
    }

    public void setItems(ArrayList<String> links) {
        mPhotoLinks = links;
    }

    @Override
    public View instantiateItem(ViewGroup container, final int position) {
        View contentView = inflater.inflate(R.layout.gallery_view_item, container, false);
        final PhotoView photoView = (PhotoView) contentView.findViewById(R.id.gallery_photoview);
        final ProgressBar progressBar = (ProgressBar) contentView.findViewById(R.id.gallery_progress_bar);
        ImageLoader.getInstance().displayImage(mPhotoLinks.get(position), photoView, options, new ImageLoadingListener() {
            @Override
            public void onLoadingStarted(String s, View view) {
                //Log.d(DEBUG_TAG, "ImageLoadingListener >> onLoadingStarted");
                progressBar.setVisibility(View.VISIBLE);
            }

            @Override
            public void onLoadingFailed(String s, View view, FailReason failReason) {
                //Log.d(DEBUG_TAG, "ImageLoadingListener >> onLoadingFailed");
                progressBar.setVisibility(View.INVISIBLE);
            }

            @Override
            public void onLoadingComplete(String s, View view, Bitmap bitmap) {
                //Log.d(DEBUG_TAG, "ImageLoadingListener >> onLoadingComplete");
                progressBar.setVisibility(View.INVISIBLE);
            }

            @Override
            public void onLoadingCancelled(String s, View view) {
                //Log.d(DEBUG_TAG, "ImageLoadingListener >> onLoadingCancelled");
                progressBar.setVisibility(View.INVISIBLE);

            }
        }, new ImageLoadingProgressListener() {
            @Override
            public void onProgressUpdate(String s, View view, int i, int i1) {
                // There isn`t any way to get the actual loading bytes and total bytes with which
                // could show the progress bar with precise percent
                // see https://github.com/nostra13/Android-Universal-Image-Loader/issues/402 for details
                progressBar.setVisibility(View.VISIBLE);
            }
        });

        photoView.setOnPhotoTapListener(new PhotoViewAttacher.OnPhotoTapListener() {
            @Override
            public void onPhotoTap(View view, float x, float y) {
                mActivity.hideOrShowActionBar();
            }
        });

        container.addView(contentView, ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);

        return contentView;
    }

    /**
     * when you call notifyDataSetChanged(),
     * the view pager will remove all views and reload them all.
     * As so the reload effect is obtained.
     */
    @Override
    public int getItemPosition(Object object) {
        return POSITION_NONE;
    }

    @Override
    public void destroyItem(ViewGroup container, int position, Object object) {
        container.removeView((View) object);
    }

    @Override
    public boolean isViewFromObject(View view, Object object) {
        return view == object;
    }

}
