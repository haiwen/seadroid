package com.seafile.seadroid2.ui.activity;

import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.widget.TextView;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.HackyViewPager;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.adapter.GalleryAdapter;
import com.seafile.seadroid2.util.Utils;

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
    private TransferService txService;
    private DataManager dataMgr;
    private ArrayList<SeafDirent> mDirents;
    private Account mAccount;
    private String repoName;
    private String repoID;
    private String dirPath;
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
                if (mDirents == null)
                    return;

                mPageIndex.setText(String.valueOf(position + 1));
                mPageCount.setText(String.valueOf(mDirents.size()));
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
        mAccount = getIntent().getParcelableExtra("account");
        dataMgr = new DataManager(mAccount);

        requestPhotoInfos(repoName, repoID, dirPath);

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        //Log.d(DEBUG_TAG, "try bind TransferService");
    }

    private void requestPhotoInfos(String repoName, String repoID, String dirPath) {
        if (!Utils.isNetworkOn()) {
            ToastUtils.show(this, R.string.network_down);
            return;
        }

        ConcurrentAsyncTask.execute(new DownloadPicsByPathTask(), repoName, repoID, dirPath);
    }

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
                dirents = dataMgr.getDirentsFromServer(repoID, dirPath);
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
            mDirents = seafDirents;
            if (seafDirents.isEmpty())
                return;
            mViewPager.setAdapter(new GalleryAdapter(GalleryActivity.this,
                    dataMgr, seafDirents, repoName, repoID, dirPath));
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

    public TransferService getTxService() {
        return txService;
    }
}

