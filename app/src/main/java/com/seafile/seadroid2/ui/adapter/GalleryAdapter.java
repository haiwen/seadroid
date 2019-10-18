package com.seafile.seadroid2.ui.adapter;

import android.support.v4.view.PagerAdapter;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;

import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.nostra13.universalimageloader.core.listener.ImageLoadingListener;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafPhoto;
import com.seafile.seadroid2.ui.AnimateFirstDisplayListener;
import com.seafile.seadroid2.ui.activity.GalleryActivity;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.List;

import uk.co.senab.photoview.PhotoView;
import uk.co.senab.photoview.PhotoViewAttacher;

/**
 * Gallery Adapter
 */
public class GalleryAdapter extends PagerAdapter {
    public static final String DEBUG_TAG = "GalleryAdapter";

    /** unique task id */
    public static int taskID;

    private GalleryActivity mActivity;
    private List<SeafPhoto> seafPhotos;
    private LayoutInflater inflater;
    private DisplayImageOptions options;
    private Account mAccount;
    private DataManager dm;

    public GalleryAdapter(GalleryActivity context, Account account,
                          List<SeafPhoto> photos, DataManager dataManager) {
        mActivity = context;
        seafPhotos = photos;
        inflater = context.getLayoutInflater();
        options = new DisplayImageOptions.Builder()
                .showImageForEmptyUri(R.drawable.ic_gallery_empty2)
                .showImageOnFail(R.drawable.gallery_loading_failed)
                .cacheInMemory(true)
                .cacheOnDisk(true)
                .considerExifParams(true)
                .extraForDownloader(account)
                .build();
        mAccount = account;
        dm = dataManager;
    }

    @Override
    public int getCount() {
        return seafPhotos.size();
    }

    public void setItems(List<SeafPhoto> photos) {
        seafPhotos = photos;
    }

    public void downloadPhoto() {
        notifyDataSetChanged();
    }

    @Override
    public View instantiateItem(ViewGroup container, final int position) {
        View contentView = inflater.inflate(R.layout.gallery_view_item, container, false);
        final PhotoView photoView = (PhotoView) contentView.findViewById(R.id.gallery_photoview);
        final ProgressBar progressBar = (ProgressBar) contentView.findViewById(R.id.gallery_progress_bar);
        final String repoName = seafPhotos.get(position).getRepoName();
        final String repoID = seafPhotos.get(position).getRepoID();
        SeafPhoto seafPhoto = seafPhotos.get(position);
        final String filePath = Utils.pathJoin(seafPhoto.getDirPath(),
                seafPhoto.getName());
        final File file = dm.getLocalRepoFile(repoName, repoID, filePath);
        if (file.exists()) {
            ImageLoader.getInstance().displayImage("file://" + file.getAbsolutePath().toString(), photoView, options);
            seafPhoto.setDownloaded(true);
        } else {
            ImageLoadingListener animateFirstListener = new AnimateFirstDisplayListener();
            String urlicon = dm.getThumbnailLink(repoName, repoID, filePath, Utils.getThumbnailWidth());
            ImageLoader.getInstance().displayImage(urlicon, photoView, options, animateFirstListener);

        }

        photoView.setOnPhotoTapListener(new PhotoViewAttacher.OnPhotoTapListener() {
            @Override
            public void onPhotoTap(View view, float x, float y) {
                mActivity.hideOrShowToolBar();
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
