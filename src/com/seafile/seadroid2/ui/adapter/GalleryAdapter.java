package com.seafile.seadroid2.ui.adapter;

import android.support.v4.view.PagerAdapter;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.transfer.DownloadStateListener;
import com.seafile.seadroid2.transfer.DownloadTask;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.ui.ProgressWheel;
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

    private int taskID = 1000000000;
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

    @Override
    public int getCount() {
        return dirents.size();
    }

    @Override
    public View instantiateItem(ViewGroup container, int position) {
        View contentView = inflater.inflate(R.layout.gallery_view_item, container, false);
        final PhotoView photoView = (PhotoView) contentView.findViewById(R.id.gallery_photoview);
        final ProgressWheel pw = (ProgressWheel) contentView.findViewById(R.id.pw_spinner);
        final SeafCachedFile scf = dataMgr.getCachedFile(repoName, repoId, Utils.pathJoin(dirPath, dirents.get(position).name));
        if (scf != null) {
            final File cachedFile = dataMgr.getLocalCachedFile(repoName, repoId, Utils.pathJoin(dirPath, dirents.get(position).name), scf.fileID);
            if (cachedFile != null) {
                ImageLoader.getInstance().displayImage("file://" + cachedFile.getAbsolutePath(), photoView);
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
                            Log.d(DEBUG_TAG, "download progress " + (int) (dti.finished * 360 / dti.fileSize));
                            pw.setProgress((int) (dti.finished * 360 / dti.fileSize));
                            pw.setVisibility(View.VISIBLE);
                        }

                        @Override
                        public void onFileDownloaded(int taskID) {

                            if (mActivity.getTxService() == null)
                                return;

                            DownloadTaskInfo dti = mActivity.getTxService().getDownloadTaskInfo(taskID);
                            if (dti == null)
                                return;

                            pw.setProgress(360);
                            pw.setVisibility(View.GONE);
                            ImageLoader.getInstance().displayImage("file://" + dti.localFilePath, photoView);
                        }

                        @Override
                        public void onFileDownloadFailed(int taskID) {
                            if (mActivity.getTxService() == null)
                                return;
                            DownloadTaskInfo dti = mActivity.getTxService().getDownloadTaskInfo(taskID);
                            if (dti == null)
                                return;

                            Log.e(DEBUG_TAG, "failed download image " + dti.pathInRepo);
                        }
                    });

            // must use this method in order to be consistent with other modules
            mActivity.getTxService().addDownloadTask(dt);

        }

        // Now just add PhotoView to ViewPager and return it
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
