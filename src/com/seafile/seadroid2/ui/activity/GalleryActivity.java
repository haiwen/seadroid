package com.seafile.seadroid2.ui.activity;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.view.PagerAdapter;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.*;
import android.view.ViewGroup.LayoutParams;
import android.widget.TextView;
import com.google.common.collect.Lists;
import com.nostra13.universalimageloader.core.ImageLoader;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.transfer.DownloadStateListener;
import com.seafile.seadroid2.transfer.DownloadTask;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.util.Utils;
import com.todddavies.components.progressbar.ProgressWheel;
import uk.co.senab.photoview.PhotoView;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Gallery Activity for supporting a gallery to browse photos.
 * Support local and cloud photos, it will downloaded first in second case
 */
public class GalleryActivity extends Activity {
    public static final String DEBUG_TAG = "GalleryActivity";

    private ViewPager mViewPager;
    private TextView mPageIndex;
    private TextView mPageCount;
    private Account account;
    private DataManager dataManager;
    private TransferService txService;
    private ArrayList<SeafDirent> dirents;
    private int taskID = 1000000000;
    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
            //Log.d(DEBUG_TAG, "bind TransferService");
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.gallery_activity_layout);

        if (getActionBar() != null)
            getActionBar().hide();

        mViewPager = (HackyViewPager) findViewById(R.id.gallery_pager);
        mViewPager.setOnPageChangeListener(new ViewPager.OnPageChangeListener() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                if (dirents == null)
                    return;

                mPageIndex.setText(String.valueOf(position + 1));
                mPageCount.setText(String.valueOf(dirents.size()));
            }

            @Override
            public void onPageSelected(int position) {}

            @Override
            public void onPageScrollStateChanged(int state) {}
        });

        mPageIndex = (TextView) findViewById(R.id.gallery_page_index);
        mPageCount = (TextView) findViewById(R.id.gallery_page_count);

        repoName = getIntent().getStringExtra("repoName");
        repoID = getIntent().getStringExtra("repoId");
        dirPath = getIntent().getStringExtra("path");
        account = getIntent().getParcelableExtra("account");
        dataManager = new DataManager(account);

        fetchPicsOriginalUrls(repoName, repoID, dirPath);

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        //Log.d(DEBUG_TAG, "try bind TransferService");
    }

    private void fetchPicsOriginalUrls(String repoName, String repoID, String dirPath) {
        if (!Utils.isNetworkOn()) {
            ToastUtils.show(this, R.string.network_down);
            return;
        }

        ConcurrentAsyncTask.execute(new DownloadPicsByPathTask(), repoName, repoID, dirPath);
    }

    private String repoName;
    private String repoID;
    private String dirPath;

    private class DownloadPicsByPathTask extends AsyncTask<String, Void, ArrayList<SeafDirent>> {
        SeafException err = null;

        @Override
        protected ArrayList<SeafDirent> doInBackground(String... params) {
            if (params.length != 3) {
                Log.e(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }

            repoName = params[0];
            repoID = params[1];
            dirPath = params[2];

            List<SeafDirent> dirents;
            try {
                dirents = dataManager.getDirentsFromServer(repoID, dirPath);
            } catch (SeafException e) {
                err = e;
                e.printStackTrace();
                return null;
            }

            if (dirents == null)
                return null;

            ArrayList<SeafDirent> seafDirents = Lists.newArrayList();
            for (SeafDirent seafDirent : dirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) { // only cache image type files
                    seafDirents.add(seafDirent);
                }

            }

            return seafDirents;
        }

        @Override
        protected void onPostExecute(ArrayList<SeafDirent> seafDirents) {
            dirents = seafDirents;
            if (seafDirents.isEmpty())
                return;
            mViewPager.setAdapter(new SamplePagerAdapter(seafDirents, repoName, repoID, dirPath, account));
        }
    }

    class SamplePagerAdapter extends PagerAdapter {
        private ArrayList<SeafDirent> dirents;
        private String repoName;
        private String repoId;
        private String dirPath;
        private Account mAccount;
        private LayoutInflater inflater;

        public SamplePagerAdapter(ArrayList<SeafDirent> seafDirents, String name, String id, String path, Account account) {
            dirents = seafDirents;
            repoName = name;
            dirPath = path;
            repoId = id;
            mAccount = account;
            inflater = getLayoutInflater();
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
            final SeafCachedFile scf = dataManager.getCachedFile(repoName, repoId, Utils.pathJoin(dirPath, dirents.get(position).name));
            if (scf != null) {
                final File cachedFile = dataManager.getLocalCachedFile(repoName, repoId, Utils.pathJoin(dirPath, dirents.get(position).name), scf.fileID);
                if (cachedFile != null) {
                    ImageLoader.getInstance().displayImage("file://" + cachedFile.getAbsolutePath(), photoView);
                }
            } else {

                // load the image
                final DownloadTask dt = new DownloadTask(
                        ++taskID,
                        account,
                        repoName,
                        repoId,
                        Utils.pathJoin(dirPath, dirents.get(position).name),
                        new DownloadStateListener() {
                            @Override
                            public void onFileDownloadProgress(int taskID) {
                                if (txService == null)
                                    return;
                                DownloadTaskInfo dti = txService.getDownloadTaskInfo(taskID);
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

                                if (txService == null)
                                    return;

                                DownloadTaskInfo dti = txService.getDownloadTaskInfo(taskID);
                                if (dti == null)
                                    return;

                                pw.setProgress(360);
                                pw.setVisibility(View.GONE);
                                ImageLoader.getInstance().displayImage("file://" + dti.localFilePath, photoView);
                            }

                            @Override
                            public void onFileDownloadFailed(int taskID) {
                                if (txService == null)
                                    return;
                                DownloadTaskInfo dti = txService.getDownloadTaskInfo(taskID);
                                if (dti == null)
                                    return;

                                Log.e(DEBUG_TAG, "failed download image " + dti.pathInRepo);
                            }
                        });

                // must use this method in order to be consistent with other modules
                txService.addDownloadTask(dt);

            }

            // Now just add PhotoView to ViewPager and return it
            container.addView(contentView, LayoutParams.MATCH_PARENT, LayoutParams.MATCH_PARENT);

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

    @Override
    protected void onDestroy() {
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }

        super.onDestroy();
    }
}

