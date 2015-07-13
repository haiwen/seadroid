package com.seafile.seadroid2.ui.activity;

import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;
import android.view.animation.AnimationUtils;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafItem;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.ZoomOutPageTransformer;
import com.seafile.seadroid2.ui.adapter.GalleryAdapter;
import com.seafile.seadroid2.ui.adapter.SeafItemAdapter;
import com.seafile.seadroid2.ui.dialog.DeleteFileDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * A gallery of images with sliding, zooming, multi-touch and single touch support
 * Local cached images will be shown directly, while cloud images will be asynchronously downloaded first
 */
public class GalleryActivity extends SherlockFragmentActivity {
    public static final String DEBUG_TAG = "GalleryActivity";

    private ViewPager mViewPager;
    private LinearLayout mPageIndexContainer;
    private TextView mPageIndexTextView;
    private TextView mPageCountTextView;
    private TextView mPageNameTextView;
    private FrameLayout mProgressView;
    private ImageView mDeleteBtn;
    private ImageView mStarBtn;
    private ImageView mShareBtn;
    private LinearLayout mToolbar;
    private DataManager dataMgr;
    private Account mAccount;
    private String repoName;
    private String repoID;
    private String dirPath;
    private String fileName;
    private String STATE_FILE_NAME;
    private SeafDirent currentDirent;
    private int mPageIndex;
    private GalleryAdapter mGalleryAdapter;
    private final RequestPhotoLinksTask mLinksTask = new RequestPhotoLinksTask();
    /** mapping thumbnail link to seafDirent in order to display photo name */
    private LinkedHashMap<String, SeafDirent> mThumbLinkAndSeafDirentMap = new LinkedHashMap<String, SeafDirent>();

    /** flag to mark if the tool bar was shown */
    private static boolean showToolBar = true;
    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            switch (v.getId()) {
                case R.id.gallery_delete_photo:
                    if (currentDirent == null)
                        return;
                    deleteFile(repoID, Utils.pathJoin(dirPath, currentDirent.name));
                    break;
                case R.id.gallery_star_photo:
                    starFile(repoID, dirPath, currentDirent.name);
                    break;
                case R.id.gallery_share_photo:
                    shareFile(repoID, Utils.pathJoin(dirPath, currentDirent.name));
                    break;
            }
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);
        setContentView(R.layout.gallery_activity_layout);
        if (getSupportActionBar() != null)
            getSupportActionBar().hide();

        mProgressView = (FrameLayout) findViewById(R.id.gallery_progressContainer);
        mDeleteBtn = (ImageView) findViewById(R.id.gallery_delete_photo);
        mStarBtn = (ImageView) findViewById(R.id.gallery_star_photo);
        mShareBtn = (ImageView) findViewById(R.id.gallery_share_photo);
        mToolbar = (LinearLayout) findViewById(R.id.gallery_tool_bar);
        mDeleteBtn.setOnClickListener(onClickListener);
        mStarBtn.setOnClickListener(onClickListener);
        mShareBtn.setOnClickListener(onClickListener);
        mViewPager = (ViewPager) findViewById(R.id.gallery_pager);
        mViewPager.setPageTransformer(true, new ZoomOutPageTransformer());
        mViewPager.setOffscreenPageLimit(1);

        mViewPager.setOnPageChangeListener(new ViewPager.OnPageChangeListener() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                // page index starting from 1 instead of 0 in user interface, so plus one here
                mPageIndexTextView.setText(String.valueOf(position + 1));
                mPageIndex = position;
                if (mGalleryAdapter != null) {
                    String linkKey = mGalleryAdapter.getItem(position);
                    if (mThumbLinkAndSeafDirentMap.containsKey(linkKey)) {
                        currentDirent = mThumbLinkAndSeafDirentMap.get(linkKey);
                        mPageNameTextView.setText(currentDirent.name);
                        fileName = currentDirent.name;
                    }
                }
            }

            @Override
            public void onPageSelected(int position) {}

            @Override
            public void onPageScrollStateChanged(int state) {}
        });

        mPageIndexContainer = (LinearLayout) findViewById(R.id.page_index_container);
        mPageIndexTextView = (TextView) findViewById(R.id.gallery_page_index);
        mPageCountTextView = (TextView) findViewById(R.id.gallery_page_count);
        mPageNameTextView = (TextView) findViewById(R.id.gallery_page_name);

        repoName = getIntent().getStringExtra("repoName");
        repoID = getIntent().getStringExtra("repoId");
        dirPath = getIntent().getStringExtra("path");
        mAccount = getIntent().getParcelableExtra("account");
        fileName = getIntent().getStringExtra("fileName");
        dataMgr = new DataManager(mAccount);

        showGallery(repoName, repoID, dirPath, fileName);
    }

    @Override
    public void onSaveInstanceState(Bundle savedInstanceState) {
        // Save the current image file name
        savedInstanceState.putString(STATE_FILE_NAME, fileName);
        // Log.d(DEBUG_TAG, "onSaveInstanceState " + fileName);

        // Always call the superclass so it can save the view hierarchy state
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        // Always call the superclass so it can restore the view hierarchy
        super.onRestoreInstanceState(savedInstanceState);

        // Restore state members from saved instance
        fileName = savedInstanceState.getString(STATE_FILE_NAME);

        // Log.d(DEBUG_TAG, "onRestoreInstanceState " + fileName);
        navToSelectedPage();
    }

    private void showGallery(String repoName, String repoID, String dirPath, String fileName) {
        if (!Utils.isNetworkOn()) {
            // show cached images
            List<SeafDirent> seafDirents = dataMgr.getCachedDirents(repoID, dirPath);
            if (seafDirents == null)
                return;

            seafDirents = sortFiles(seafDirents,
                    SettingsManager.instance().getSortFilesTypePref(),
                    SettingsManager.instance().getSortFilesOrderPref());
            for (SeafDirent seafDirent : seafDirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) { // only cache image type files
                    String link = dataMgr.getThumbnailLink(repoID, Utils.pathJoin(dirPath, seafDirent.name), 800);
                    // Log.d(DEBUG_TAG, "add remote url " + thumbnailLink);
                    if (link != null) {
                        mThumbLinkAndSeafDirentMap.put(link, seafDirent);
                    }
                }
            }

            mGalleryAdapter = new GalleryAdapter(GalleryActivity.this,
                    mAccount,
                    new ArrayList<String>(mThumbLinkAndSeafDirentMap.keySet()));
            mViewPager.setAdapter(mGalleryAdapter);

            navToSelectedPage();
            return;
        }

        ConcurrentAsyncTask.execute(mLinksTask, repoName, repoID, dirPath);
    }

    private class RequestPhotoLinksTask extends AsyncTask<String, Void, ArrayList<String>> {
        /** hold thumbnails links which will be used to load thumbnails in gallery */
        private ArrayList<String> mThumbnailLinks = Lists.newArrayList();
        private SeafException err = null;

        public ArrayList<String> getThumbnailLinks() {
            return mThumbnailLinks;
        }

        @Override
        protected void onPreExecute() {
            showLoading(true);
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

            seafDirents = sortFiles(seafDirents,
                    SettingsManager.instance().getSortFilesTypePref(),
                    SettingsManager.instance().getSortFilesOrderPref());
            for (SeafDirent seafDirent : seafDirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) { // only cache image type files
                    String link = dataMgr.getThumbnailLink(repoID, Utils.pathJoin(dirPath, seafDirent.name), 800);
                    // Log.d(DEBUG_TAG, "add remote url " + thumbnailLink);
                    if(link != null) {
                        mThumbLinkAndSeafDirentMap.put(link, seafDirent);
                    }
                }
            }
            return new ArrayList<String>(mThumbLinkAndSeafDirentMap.keySet());
        }

        @Override
        protected void onPostExecute(ArrayList<String> links) {
            showLoading(false);
            if (links == null
                    || links.isEmpty()
                    || fileName == null) {
                if (err != null) {
                    ToastUtils.show(GalleryActivity.this, R.string.gallery_load_photos_error);
                    Log.e(DEBUG_TAG, "error message " + err.getMessage() + " error code " + err.getCode());
                }

            }

            mThumbnailLinks = links;
            mGalleryAdapter = new GalleryAdapter(GalleryActivity.this, mAccount, links);
            mViewPager.setAdapter(mGalleryAdapter);

            navToSelectedPage();
        }
    }

    /**
     * Sorts the given list by type of {@link SeafItemAdapter#SORT_BY_NAME} or {@link SeafItemAdapter#SORT_BY_LAST_MODIFIED_TIME},
     * and by order of {@link SeafItemAdapter#SORT_ORDER_ASCENDING} or {@link SeafItemAdapter#SORT_ORDER_DESCENDING}
     *
     * @param dirents
     * @param type
     * @param order
     * @return sorted file list
     */
    public List<SeafDirent> sortFiles(List<SeafDirent> dirents, int type, int order) {
        Log.d(DEBUG_TAG, "sort filey by type " + type + " by order " + order);
        // sort SeafDirents
        if (type == SeafItemAdapter.SORT_BY_NAME) {
            // sort by name, in ascending order
            Collections.sort(dirents, new SeafDirent.DirentNameComparator());
            if (order == SeafItemAdapter.SORT_ORDER_DESCENDING) {
                Collections.reverse(dirents);
            }
        } else if (type == SeafItemAdapter.SORT_BY_LAST_MODIFIED_TIME) {
            // sort by last modified time, in ascending order
            Collections.sort(dirents,   new SeafDirent.DirentLastMTimeComparator());
            if (order == SeafItemAdapter.SORT_ORDER_DESCENDING) {
                Collections.reverse(dirents);
            }
        }
        return dirents;
    }

    /**
     * Dynamically navigate to the starting page index selected by user
     * by default the starting page index is 0
     *
     */
    private void navToSelectedPage() {
        int i = 0;
        for (String key : mThumbLinkAndSeafDirentMap.keySet()) {
            if (mThumbLinkAndSeafDirentMap.get(key).name.equals(fileName)) {
                mViewPager.setCurrentItem(i);
                mPageIndexTextView.setText(String.valueOf(i + 1));
                mPageIndex = i;
                mPageNameTextView.setText(fileName);
                if (mGalleryAdapter == null) return;
                String linkKey = mGalleryAdapter.getItem(i);
                if (mThumbLinkAndSeafDirentMap.containsKey(linkKey)) {
                    currentDirent = mThumbLinkAndSeafDirentMap.get(linkKey);
                }
                break;
            }
            i++;
        }
        mPageCountTextView.setText(String.valueOf(mThumbLinkAndSeafDirentMap.size()));
    }

    /**
     * This method will get called when tapping at the center of a photo,
     * tool bar will auto hide when open the gallery,
     * and will show or hide alternatively when tapping.
     */
    public void hideOrShowToolBar() {
        if (showToolBar) {
            mToolbar.setVisibility(View.VISIBLE);
            mPageIndexContainer.setVisibility(View.VISIBLE);
            mPageNameTextView.setVisibility(View.VISIBLE);
            showToolBar = !showToolBar;
        } else {
            mToolbar.setVisibility(View.GONE);
            mPageIndexContainer.setVisibility(View.GONE);
            mPageNameTextView.setVisibility(View.GONE);
            showToolBar = !showToolBar;
        }

    }

    private void deleteFile(String repoID, String path) {
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

    private void starFile(String repoId, String dir, String fileName) {
        doStarFile(repoId, dir, fileName);
    }

    private void doStarFile(String repoID, String path, String filename) {

        if (!Utils.isNetworkOn()) {
            ToastUtils.show(this, R.string.network_down);
            return;
        }

        String p = Utils.pathJoin(path, filename);
        ConcurrentAsyncTask.execute(new StarFileTask(repoID, p));
    }

    private void shareFile(String repoID, String path) {
        WidgetUtils.chooseShareApp(this, repoID, path, false, mAccount);
    }

    class StarFileTask extends AsyncTask<Void, Void, Void> {
        private String repoId;
        private String path;
        private SeafException err;

        public StarFileTask(String repoId, String path) {
            this.repoId = repoId;
            this.path = path;
        }

        @Override
        protected Void doInBackground(Void... params) {

            if (dataMgr == null)
                return null;

            try {
                dataMgr.star(repoId, path);
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            if (err != null) {
                ToastUtils.show(GalleryActivity.this, R.string.star_file_failed);
                return;
            }

            ToastUtils.show(GalleryActivity.this, R.string.star_file_succeed);
        }
    }

    /**
     * slide to next page if there are pages left on the right side of the current one,
     * slide to previous page if not,
     * quit the gallery if both cases were not met
     */
    private void slidePage() {
        ArrayList<String> links = mLinksTask.getThumbnailLinks();
        links.remove(mPageIndex);
        if (mGalleryAdapter == null) return;

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

        // Log.d(DEBUG_TAG, "pageIndex " + mPageIndex);
        String linkKey = mGalleryAdapter.getItem(mPageIndex);
        if (mThumbLinkAndSeafDirentMap.containsKey(linkKey)) {
            currentDirent = mThumbLinkAndSeafDirentMap.get(linkKey);
            // update file name in gallery view
            mPageNameTextView.setText(currentDirent.name);
            fileName = currentDirent.name;
        }

    }

    private void showLoading(boolean show) {
        if (show) {
            mProgressView.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_in));

            mProgressView.setVisibility(View.VISIBLE);
        } else {
            mProgressView.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_out));
            mProgressView.setVisibility(View.GONE);
        }
    }

}

