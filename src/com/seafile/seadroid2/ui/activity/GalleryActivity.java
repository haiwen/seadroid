package com.seafile.seadroid2.ui.activity;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.graphics.Point;
import android.graphics.Rect;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;
import android.view.animation.DecelerateInterpolator;
import android.widget.ImageView;
import android.widget.TextView;
import com.actionbarsherlock.app.SherlockActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.AnimationRect;
import com.seafile.seadroid2.ui.HackyViewPager;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.ZoomOutPageTransformer;
import com.seafile.seadroid2.ui.adapter.GalleryAdapter;
import com.seafile.seadroid2.util.Utils;
import uk.co.senab.photoview.PhotoView;

import java.io.File;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * A gallery of images with sliding, zooming, multi-touch and single touch support
 * Local cached images will be shown directly, while cloud images will be asynchronously downloaded first
 */
public class GalleryActivity extends SherlockActivity {
    public static final String DEBUG_TAG = "GalleryActivity";

    private ViewPager mViewPager;
    private ImageView animationView;
    private TextView mPageIndex;
    private TextView mPageCount;
    private TextView mPageName;
    private DataManager dataMgr;
    private Account mAccount;
    private String repoName;
    private String repoID;
    private String dirPath;
    private String fileName;
    private ArrayList<String> mThumbnailLinks = Lists.newArrayList();
    private HashMap<String, String> mThumbnailFileNameMap = Maps.newHashMap();

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);
        setContentView(R.layout.gallery_activity_layout);

        /*if (getSupportActionBar() != null)
            getSupportActionBar().hide();*/

        mViewPager = (HackyViewPager) findViewById(R.id.gallery_pager);
        mViewPager.setPageTransformer(true, new ZoomOutPageTransformer());
        mViewPager.setOffscreenPageLimit(1);

        mViewPager.setOnPageChangeListener(new ViewPager.OnPageChangeListener() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                if (mThumbnailLinks == null)
                    return;

                mPageIndex.setText(String.valueOf(position + 1));
                String currentLink = mThumbnailLinks.get(position);
                if (mThumbnailFileNameMap.containsKey(currentLink))
                    mPageName.setText(mThumbnailFileNameMap.get(currentLink));
            }

            @Override
            public void onPageSelected(int position) {}

            @Override
            public void onPageScrollStateChanged(int state) {}
        });

        mPageIndex = (TextView) findViewById(R.id.gallery_page_index);
        mPageCount = (TextView) findViewById(R.id.gallery_page_count);
        mPageName = (TextView) findViewById(R.id.gallery_page_name);
        animationView = (ImageView) findViewById(R.id.gallery_animation);

        repoName = getIntent().getStringExtra("repoName");
        repoID = getIntent().getStringExtra("repoId");
        dirPath = getIntent().getStringExtra("path");
        mAccount = getIntent().getParcelableExtra("account");
        fileName = getIntent().getStringExtra("fileName");
        dataMgr = new DataManager(mAccount);

        requestPhotoInfos(repoName, repoID, dirPath, fileName);
    }

    private void requestPhotoInfos(String repoName, String repoID, String dirPath, String fileName) {
        if (!Utils.isNetworkOn()) {
            ToastUtils.show(this, R.string.network_down);
            // display cached photos in gallery
            for (SeafDirent seafDirent : dataMgr.getCachedDirents(repoID, dirPath)) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) { // only cache image type files
                    //TODO may getrepocachedFile is better
                    final SeafCachedFile scf = dataMgr.getCachedFile(repoName, repoID, Utils.pathJoin(dirPath, seafDirent.name));
                    if (scf != null) {
                        final File cachedFile = dataMgr.getLocalCachedFile(repoName, repoID, Utils.pathJoin(dirPath, seafDirent.name), scf.fileID);
                        if (cachedFile != null) {
                            // Log.d(DEBUG_TAG, "add cached url file://" + cachedFile.getAbsolutePath());
                            String thumbnailLink = "file://" + cachedFile.getAbsolutePath();
                            mThumbnailLinks.add(thumbnailLink);
                            mThumbnailFileNameMap.put(thumbnailLink, cachedFile.getName());
                        }
                    }
                }
            }

            if (mThumbnailLinks.isEmpty())
                return;
            mViewPager.setAdapter(new GalleryAdapter(GalleryActivity.this, mAccount, mThumbnailLinks));

            for (int i = 0; i < mThumbnailLinks.size(); i++) {
                if (Utils.fileNameFromPath(mThumbnailLinks.get(i)).equals(fileName)) {
                    mViewPager.setCurrentItem(i);
                    mPageIndex.setText(String.valueOf(i + 1));
                    mPageName.setText(fileName);
                    break;
                }
            }
            mPageCount.setText(String.valueOf(mThumbnailLinks.size()));
            return;

        }

        ConcurrentAsyncTask.execute(new DownloadPicsByPathTask(), repoName, repoID, dirPath);
    }

    public void animateClose(PhotoView imageView, AnimationRect animationRect) {
        mPageIndex.setVisibility(View.INVISIBLE);
        animationView.setImageDrawable(imageView.getDrawable());

        mViewPager.setVisibility(View.INVISIBLE);

        final Rect startBounds = new Rect(animationRect.scaledBitmapRect);
        final Rect finalBounds = new Rect();
        final Point globalOffset = new Point();

        animationView.getGlobalVisibleRect(finalBounds, globalOffset);

        startBounds.offset(-globalOffset.x, -globalOffset.y);
        finalBounds.offset(-globalOffset.x, -globalOffset.y);

        float startScale;
        if ((float) finalBounds.width() / finalBounds.height()
                > (float) startBounds.width() / startBounds.height()) {
            // Extend start bounds horizontally
            startScale = (float) startBounds.height() / finalBounds.height();
            float startWidth = startScale * finalBounds.width();
            float deltaWidth = (startWidth - startBounds.width()) / 2;
            startBounds.left -= deltaWidth;
            startBounds.right += deltaWidth;
        } else {
            // Extend start bounds vertically
            startScale = (float) startBounds.width() / finalBounds.width();
            float startHeight = startScale * finalBounds.height();
            float deltaHeight = (startHeight - startBounds.height()) / 2;
            startBounds.top -= deltaHeight;
            startBounds.bottom += deltaHeight;
        }

        animationView.setPivotX(0f);
        animationView.setPivotY(0f);

        final float startScaleFinal = startScale;

        animationView.animate().setInterpolator(new DecelerateInterpolator()).x(startBounds.left)
                .y(startBounds.top).scaleY(startScaleFinal).scaleX(startScaleFinal).setDuration(300)
                .setListener(new AnimatorListenerAdapter() {
                    @Override
                    public void onAnimationEnd(Animator animation) {
                        super.onAnimationEnd(animation);
                        GalleryActivity.this.finish();
                        overridePendingTransition(0, 0);
                    }
                }).start();
    }

    private class DownloadPicsByPathTask extends AsyncTask<String, Void, ArrayList<String>> {
        SeafException err = null;

        @Override
        protected ArrayList<String> doInBackground(String... params) {
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

            ArrayList<String> tLinks = Lists.newArrayList();
            for (SeafDirent seafDirent : dirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) { // only cache image type files
                    String thumbnailLink = dataMgr.getThumbnailLink(repoID, Utils.pathJoin(dirPath, seafDirent.name), 800);
                    // Log.d(DEBUG_TAG, "add remote url " + thumbnailLink);
                    if(thumbnailLink != null) {
                        tLinks.add(thumbnailLink);
                        mThumbnailFileNameMap.put(thumbnailLink, seafDirent.name);
                    }
                }
            }
            return tLinks;
        }

        @Override
        protected void onPostExecute(ArrayList<String> thumbnailLinks) {
            if (thumbnailLinks.isEmpty())
                return;

            if (fileName == null)
                return;

            mThumbnailLinks = thumbnailLinks;
            mViewPager.setAdapter(new GalleryAdapter(GalleryActivity.this, mAccount, thumbnailLinks));
            for (int i = 0; i< mThumbnailLinks.size(); i++) {
                String key = mThumbnailLinks.get(i);
                if (mThumbnailFileNameMap.containsKey(key)
                        && mThumbnailFileNameMap.get(key).equals(fileName)) {
                    Log.d(DEBUG_TAG, "current index " + i);
                    Log.d(DEBUG_TAG, "current file name " + fileName);
                    mViewPager.setCurrentItem(i);
                    mPageIndex.setText(String.valueOf(i + 1));
                    mPageName.setText(fileName);
                    break;
                }
            }
            mPageCount.setText(String.valueOf(mThumbnailLinks.size()));
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getSupportMenuInflater();
        inflater.inflate(R.menu.gallery_menu, menu);
        //overFlowMenu = menu;
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

}

