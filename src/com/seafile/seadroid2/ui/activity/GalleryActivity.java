package com.seafile.seadroid2.ui.activity;

import android.animation.Animator;
import android.animation.AnimatorListenerAdapter;
import android.graphics.Point;
import android.graphics.Rect;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;
import android.view.animation.DecelerateInterpolator;
import android.widget.ImageView;
import android.widget.TextView;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.ui.AnimationRect;
import com.seafile.seadroid2.ui.HackyViewPager;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.ZoomOutPageTransformer;
import com.seafile.seadroid2.ui.adapter.GalleryAdapter;
import com.seafile.seadroid2.ui.dialog.DeleteFileDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;
import uk.co.senab.photoview.PhotoView;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

/**
 * A gallery of images with sliding, zooming, multi-touch and single touch support
 * Local cached images will be shown directly, while cloud images will be asynchronously downloaded first
 */
public class GalleryActivity extends SherlockFragmentActivity {
    public static final String DEBUG_TAG = "GalleryActivity";

    private ViewPager mViewPager;
    private ImageView animationView;
    private TextView mPageIndexTextView;
    private TextView mPageCountTextView;
    private TextView mPageNameTextView;
    private DataManager dataMgr;
    private Account mAccount;
    private String repoName;
    private String repoID;
    private String dirPath;
    private String fileName;
    private SeafDirent currentDrient;
    private int mPageIndex;
    private GalleryAdapter mGalleryAdapter;
    private final RequestPhotoLinksTask mLinksTask = new RequestPhotoLinksTask();
    /** mapping thumbnail link to seafDirent in order to display photo name */
    private HashMap<String, SeafDirent> mThumbnailFileNameMap = Maps.newHashMap();

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
                // page index starting from 1 instead of 0 in user interface, so plus one here
                mPageIndexTextView.setText(String.valueOf(position + 1));
                mPageIndex = position;
                String linkKey = mGalleryAdapter.getItem(position);
                if (mThumbnailFileNameMap.containsKey(linkKey)) {
                    currentDrient = mThumbnailFileNameMap.get(linkKey);
                    mPageNameTextView.setText(currentDrient.name);
                }
            }

            @Override
            public void onPageSelected(int position) {}

            @Override
            public void onPageScrollStateChanged(int state) {}
        });

        mPageIndexTextView = (TextView) findViewById(R.id.gallery_page_index);
        mPageCountTextView = (TextView) findViewById(R.id.gallery_page_count);
        mPageNameTextView = (TextView) findViewById(R.id.gallery_page_name);
        animationView = (ImageView) findViewById(R.id.gallery_animation);

        repoName = getIntent().getStringExtra("repoName");
        repoID = getIntent().getStringExtra("repoId");
        dirPath = getIntent().getStringExtra("path");
        mAccount = getIntent().getParcelableExtra("account");
        fileName = getIntent().getStringExtra("fileName");
        dataMgr = new DataManager(mAccount);

        showGallery(repoName, repoID, dirPath, fileName);
    }

    private void showGallery(String repoName, String repoID, String dirPath, String fileName) {
        ConcurrentAsyncTask.execute(mLinksTask, repoName, repoID, dirPath);
    }

    public void animateClose(PhotoView imageView, AnimationRect animationRect) {
        mPageIndexTextView.setVisibility(View.INVISIBLE);
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

    private class RequestPhotoLinksTask extends AsyncTask<String, Void, ArrayList<String>> {
        /** hold thumbnails links which will be used to load thumbnails in gallery */
        private ArrayList<String> mThumbnailLinks = Lists.newArrayList();
        private SeafException err = null;

        public ArrayList<String> getThumbnailLinks() {
            return mThumbnailLinks;
        }

        @Override
        protected ArrayList<String> doInBackground(String... params) {
            if (params.length != 3) {
                Log.e(DEBUG_TAG, "Wrong params to RequestPhotoLinksTask");
                return null;
            }

            repoName = params[0];
            repoID = params[1];
            dirPath = params[2];

            List<SeafDirent> seafDirents;
            try {
                seafDirents = dataMgr.getDirentsFromServer(repoID, dirPath);
            } catch (SeafException e) {
                err = e;
                return null;
            }

            if (seafDirents == null)
                return null;

            ArrayList<String> links = Lists.newArrayList();
            for (SeafDirent seafDirent : seafDirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) { // only cache image type files
                    String link = dataMgr.getThumbnailLink(repoID, Utils.pathJoin(dirPath, seafDirent.name), 800);
                    // Log.d(DEBUG_TAG, "add remote url " + thumbnailLink);
                    if(link != null) {
                        links.add(link);
                        mThumbnailFileNameMap.put(link, seafDirent);
                    }
                }
            }
            return links;
        }

        @Override
        protected void onPostExecute(ArrayList<String> links) {
            if (links == null
                    || links.isEmpty()
                    || fileName == null) {
                if (err != null) {
                    ToastUtils.show(GalleryActivity.this, R.string.gallery_load_photos_error);
                    Log.e(DEBUG_TAG, "error message " + err.getMessage() + " error code " + err.getCode());
                }
                return;
            }

            mThumbnailLinks = links;
            mGalleryAdapter = new GalleryAdapter(GalleryActivity.this, mAccount, links);
            mViewPager.setAdapter(mGalleryAdapter);

            // dynamically navigate to the starting page index selected by user
            // by default the starting page index is 0
            for (int i = 0; i< links.size(); i++) {
                String key = links.get(i);
                if (mThumbnailFileNameMap.containsKey(key)
                        && mThumbnailFileNameMap.get(key).name.equals(fileName)) {
                    Log.d(DEBUG_TAG, "current index " + i);
                    Log.d(DEBUG_TAG, "current file name " + fileName);
                    mViewPager.setCurrentItem(i);
                    mPageIndexTextView.setText(String.valueOf(i + 1));
                    mPageIndex = i;
                    mPageNameTextView.setText(fileName);
                    String linkKey = mGalleryAdapter.getItem(i);
                    if (mThumbnailFileNameMap.containsKey(linkKey)) {
                        currentDrient = mThumbnailFileNameMap.get(linkKey);
                    }
                    break;
                }
            }
            mPageCountTextView.setText(String.valueOf(links.size()));
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
        switch (item.getItemId()) {
            case R.id.gallery_delete:
                if (currentDrient == null)
                    return false;

                Log.d(DEBUG_TAG, "delete " + repoName + Utils.pathJoin(dirPath, currentDrient.name));
                deleteFile(repoID, Utils.pathJoin(dirPath, currentDrient.name));
                return true;
            case R.id.gallery_star:
                return true;
            case R.id.gallery_share:
                return true;
            case R.id.gallery_export:
                return true;
            case R.id.gallery_overflow_menu:
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

    public void deleteFile(String repoID, String path) {
        doDelete(repoID, path, false);
    }

    private void doDelete(String repoID, String path, boolean isDir) {
        final DeleteFileDialog dialog = new DeleteFileDialog();
        dialog.init(repoID, path, isDir, mAccount);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                ToastUtils.show(GalleryActivity.this, R.string.delete_successful);
                slidePage();
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    /**
     * slide to next page if there are pages left on the right side of the current one,
     * slide to previous page if not,
     * quit the gallery if both cases were not met
     */
    private void slidePage() {
        ArrayList<String> links = mLinksTask.getThumbnailLinks();
        links.remove(mPageIndex);
        mGalleryAdapter.setItems(links);
        mGalleryAdapter.notifyDataSetChanged();
        mPageCountTextView.setText(String.valueOf(links.size()));

        if (links.size() == 0) {
            finish();
            return;
        }

        if (mPageIndex > links.size() - 1) {
            // slide to previous page
            mPageIndex = links.size() - 1;
            // page index starting from 1 instead of 0 in user interface, so plus one here
            mPageIndexTextView.setText(String.valueOf(mPageIndex + 1));
        }

        Log.d(DEBUG_TAG, "pageIndex " + mPageIndex);
        String linkKey = mGalleryAdapter.getItem(mPageIndex);
        if (mThumbnailFileNameMap.containsKey(linkKey)) {
            currentDrient = mThumbnailFileNameMap.get(linkKey);
            // update file name in gallery view
            mPageNameTextView.setText(currentDrient.name);
        }

    }
}

