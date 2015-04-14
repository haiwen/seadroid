/*******************************************************************************
 * Copyright 2011, 2012 Chris Banes.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *******************************************************************************/
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
import android.view.MenuItem.OnMenuItemClickListener;
import android.view.ViewGroup.LayoutParams;
import android.widget.LinearLayout;
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
import uk.co.senab.photoview.PhotoView;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Lock/Unlock button is added to the ActionBar.
 * Use it to temporarily disable ViewPager navigation in order to correctly interact with ImageView by gestures.
 * Lock/Unlock state of ViewPager is saved and restored on configuration changes.
 * <p/>
 * Julia Zudikova
 */

public class GalleryViewPagerActivity extends Activity {
    public static final String DEBUG_TAG = "GalleryViewPagerActivity";

    private static final String ISLOCKED_ARG = "isLocked";
    private ViewPager mViewPager;
    private MenuItem menuLockItem;
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
            Log.d(DEBUG_TAG, "bind TransferService");
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_view_pager);
        mViewPager = (HackyViewPager) findViewById(R.id.gallery_view_pager);
        setContentView(mViewPager);

        repoName = getIntent().getStringExtra("repoName");
        repoID = getIntent().getStringExtra("repoId");
        dirPath = getIntent().getStringExtra("path");
        account = getIntent().getParcelableExtra("account");
        dataManager = new DataManager(account);

        fetchPicsOriginalUrls(repoName, repoID, dirPath);

        if (savedInstanceState != null) {
            boolean isLocked = savedInstanceState.getBoolean(ISLOCKED_ARG, false);
            ((HackyViewPager) mViewPager).setLocked(isLocked);
        }

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");
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
        protected void onPreExecute() {
            //Log.d(DEBUG_TAG, "loading dirents");
        }

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
            final LinearLayout progress = (LinearLayout) contentView.findViewById(R.id.gallery_progressContainer);
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
                                Log.d(DEBUG_TAG, "download progress " + (int) (dti.finished * 100 / dti.fileSize));
                                progress.setVisibility(View.VISIBLE);
                                photoView.setVisibility(View.GONE);
                            }

                            @Override
                            public void onFileDownloaded(int taskID) {

                                if (txService == null)
                                    return;

                                DownloadTaskInfo dti = txService.getDownloadTaskInfo(taskID);
                                if (dti == null)
                                    return;

                                ImageLoader.getInstance().displayImage("file://" + dti.localFilePath, photoView);
                                progress.setVisibility(View.GONE);
                                photoView.setVisibility(View.VISIBLE);
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
    public boolean onCreateOptionsMenu(Menu menu) {
        getMenuInflater().inflate(R.menu.viewpager_menu, menu);
        return super.onCreateOptionsMenu(menu);
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        menuLockItem = menu.findItem(R.id.menu_lock);
        toggleLockBtnTitle();
        menuLockItem.setOnMenuItemClickListener(new OnMenuItemClickListener() {
            @Override
            public boolean onMenuItemClick(MenuItem item) {
                toggleViewPagerScrolling();
                toggleLockBtnTitle();
                return true;
            }
        });

        return super.onPrepareOptionsMenu(menu);
    }

    private void toggleViewPagerScrolling() {
        if (isViewPagerActive()) {
            ((HackyViewPager) mViewPager).toggleLock();
        }
    }

    private void toggleLockBtnTitle() {
        boolean isLocked = false;
        if (isViewPagerActive()) {
            isLocked = ((HackyViewPager) mViewPager).isLocked();
        }
        String title = (isLocked) ? getString(R.string.menu_unlock) : getString(R.string.menu_lock);
        if (menuLockItem != null) {
            menuLockItem.setTitle(title);
        }
    }

    private boolean isViewPagerActive() {
        return (mViewPager != null && mViewPager instanceof HackyViewPager);
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        if (isViewPagerActive()) {
            outState.putBoolean(ISLOCKED_ARG, ((HackyViewPager) mViewPager).isLocked());
        }
        super.onSaveInstanceState(outState);
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

