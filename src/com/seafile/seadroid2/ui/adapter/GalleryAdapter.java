package com.seafile.seadroid2.ui.adapter;

import android.support.v4.view.PagerAdapter;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.TextView;
import com.nostra13.universalimageloader.core.DisplayImageOptions;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.transfer.DownloadStateListener;
import com.seafile.seadroid2.transfer.DownloadTask;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.ui.activity.GalleryActivity;
import com.seafile.seadroid2.util.Utils;
import uk.co.senab.photoview.PhotoView;

import java.io.File;
import java.util.ArrayList;

/**
 * Gallery Adapter
 */
public class GalleryAdapter extends PagerAdapter {
    public static final String DEBUG_TAG = "GalleryAdapter";

    /** task id must be unique among different activity lifecycle, otherwise can`t track the downloadTask */
    private int taskID = (int) System.currentTimeMillis();
    private GalleryActivity mActivity;
    private DataManager dataMgr;
    private ArrayList<SeafDirent> dirents;
    private String repoName;
    private String repoId;
    private String dirPath;
    private LayoutInflater inflater;

    public GalleryAdapter(GalleryActivity context,
                          DataManager dataManager,
                          ArrayList<SeafDirent> seafDirents,
                          String name,
                          String id,
                          String path) {
        mActivity = context;
        dataMgr = dataManager;
        dirents = seafDirents;
        repoName = name;
        dirPath = path;
        repoId = id;
        inflater = context.getLayoutInflater();
    }

    DisplayImageOptions options = new DisplayImageOptions.Builder()
            .showImageOnLoading(R.drawable.gallery_loading)
            .showImageForEmptyUri(R.drawable.gallery_loading)
            .showImageOnFail(R.drawable.gallery_loading)
            .cacheOnDisk(true)
            .considerExifParams(true)
            .build();

    @Override
    public int getCount() {
        return dirents.size();
    }

    @Override
    public View instantiateItem(ViewGroup container, final int position) {
        View contentView = inflater.inflate(R.layout.gallery_view_item, container, false);
        final PhotoView photoView = (PhotoView) contentView.findViewById(R.id.gallery_photoview);
        final TextView progressText = (TextView) contentView.findViewById(R.id.gallery_progress_text);
        final ProgressBar progressBar = (ProgressBar) contentView.findViewById(R.id.gallery_progress_bar);
        final SeafCachedFile scf = dataMgr.getCachedFile(repoName, repoId, Utils.pathJoin(dirPath, dirents.get(position).name));
        if (scf != null) {
            final File cachedFile = dataMgr.getLocalCachedFile(repoName, repoId, Utils.pathJoin(dirPath, dirents.get(position).name), scf.fileID);
            if (cachedFile != null) {
                ImageLoader.getInstance().displayImage("file://" + cachedFile.getAbsolutePath(), photoView, options);
            }
        } else {

            // load the image
            final DownloadTask dt = new DownloadTask(
                    ++taskID,
                    dataMgr.getAccount(),
                    repoName,
                    repoId,
                    Utils.pathJoin(dirPath, dirents.get(position).name),
                    new DownloadStateListener() {
                        @Override
                        public void onFileDownloadProgress(int taskID) {
                            if (mActivity.getTxService() == null)
                                return;
                            DownloadTaskInfo dti = mActivity.getTxService().getDownloadTaskInfo(taskID);
                            if (dti == null)
                                return;

                            if (dti.fileSize == 0)
                                return;
                            progressText.setText(Utils.readableFileSize(dti.finished) + "/" + Utils.readableFileSize(dti.fileSize));
                            progressText.setVisibility(View.VISIBLE);
                            progressBar.setVisibility(View.VISIBLE);
                            progressBar.setIndeterminate(false);
                            progressBar.setProgress((int) (dti.finished * 100 / dti.fileSize));
                        }

                        @Override
                        public void onFileDownloaded(int taskID) {

                            if (mActivity.getTxService() == null)
                                return;

                            DownloadTaskInfo dti = mActivity.getTxService().getDownloadTaskInfo(taskID);
                            if (dti == null)
                                return;

                            progressText.setText(Utils.readableFileSize(dti.finished) + "/" + Utils.readableFileSize(dti.fileSize));
                            progressText.setVisibility(View.GONE);
                            progressBar.setVisibility(View.GONE);
                            progressBar.setProgress(100);
                            ImageLoader.getInstance().displayImage("file://" + dti.localFilePath, photoView);
                        }

                        @Override
                        public void onFileDownloadFailed(int taskID) {
                            if (mActivity.getTxService() == null)
                                return;
                            DownloadTaskInfo dti = mActivity.getTxService().getDownloadTaskInfo(taskID);
                            if (dti == null)
                                return;
                            progressText.setVisibility(View.GONE);
                            progressBar.setVisibility(View.GONE);
                            Log.e(DEBUG_TAG, "failed download image " + dti.pathInRepo);
                        }
                    });

            // must use this method to keep consistent with other modules
            mActivity.getTxService().addDownloadTask(dt);

        }

        container.addView(contentView, ViewGroup.LayoutParams.MATCH_PARENT, ViewGroup.LayoutParams.MATCH_PARENT);

        return contentView;
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
