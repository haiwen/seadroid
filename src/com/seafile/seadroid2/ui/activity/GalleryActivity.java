package com.seafile.seadroid2.ui.activity;

import android.content.Context;
import android.content.Intent;
import android.content.pm.ResolveInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.view.ViewPager;
import android.text.ClipboardManager;
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
import com.google.common.collect.Maps;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.ZoomOutPageTransformer;
import com.seafile.seadroid2.ui.adapter.GalleryAdapter;
import com.seafile.seadroid2.ui.dialog.AppChoiceDialog;
import com.seafile.seadroid2.ui.dialog.DeleteFileDialog;
import com.seafile.seadroid2.ui.dialog.GetShareLinkDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;

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
    private SeafDirent currentDirent;
    private int mPageIndex;
    private GalleryAdapter mGalleryAdapter;
    private final RequestPhotoLinksTask mLinksTask = new RequestPhotoLinksTask();
    /** mapping thumbnail link to seafDirent in order to display photo name */
    private HashMap<String, SeafDirent> mThumbLinkAndSeafDirentMap = Maps.newHashMap();

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
                String linkKey = mGalleryAdapter.getItem(position);
                if (mThumbLinkAndSeafDirentMap.containsKey(linkKey)) {
                    currentDirent = mThumbLinkAndSeafDirentMap.get(linkKey);
                    mPageNameTextView.setText(currentDirent.name);
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

    private void showGallery(String repoName, String repoID, String dirPath, String fileName) {
        if (!Utils.isNetworkOn()) {
            // show cached images
            List<SeafDirent> seafDirents = dataMgr.getCachedDirents(repoID, dirPath);
            if (seafDirents == null)
                return;

            ArrayList<String> links = Lists.newArrayList();
            for (SeafDirent seafDirent : seafDirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) { // only cache image type files
                    String link = dataMgr.getThumbnailLink(repoID, Utils.pathJoin(dirPath, seafDirent.name), 800);
                    // Log.d(DEBUG_TAG, "add remote url " + thumbnailLink);
                    if (link != null) {
                        links.add(link);
                        mThumbLinkAndSeafDirentMap.put(link, seafDirent);
                    }
                }
            }

            mGalleryAdapter = new GalleryAdapter(GalleryActivity.this, mAccount, links);
            mViewPager.setAdapter(mGalleryAdapter);

            // dynamically navigate to the starting page index selected by user
            // by default the starting page index is 0
            for (int i = 0; i < links.size(); i++) {
                String key = links.get(i);
                if (mThumbLinkAndSeafDirentMap.containsKey(key)
                        && mThumbLinkAndSeafDirentMap.get(key).name.equals(fileName)) {
                    // Log.d(DEBUG_TAG, "current index " + i);
                    // Log.d(DEBUG_TAG, "current file name " + fileName);
                    mViewPager.setCurrentItem(i);
                    mPageIndexTextView.setText(String.valueOf(i + 1));
                    mPageIndex = i;
                    mPageNameTextView.setText(fileName);
                    String linkKey = mGalleryAdapter.getItem(i);
                    if (mThumbLinkAndSeafDirentMap.containsKey(linkKey)) {
                        currentDirent = mThumbLinkAndSeafDirentMap.get(linkKey);
                    }
                    break;
                }
            }
            mPageCountTextView.setText(String.valueOf(links.size()));

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

            ArrayList<String> links = Lists.newArrayList();
            for (SeafDirent seafDirent : seafDirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) { // only cache image type files
                    String link = dataMgr.getThumbnailLink(repoID, Utils.pathJoin(dirPath, seafDirent.name), 800);
                    // Log.d(DEBUG_TAG, "add remote url " + thumbnailLink);
                    if(link != null) {
                        links.add(link);
                        mThumbLinkAndSeafDirentMap.put(link, seafDirent);
                    }
                }
            }
            return links;
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
                return;
            }

            mThumbnailLinks = links;
            mGalleryAdapter = new GalleryAdapter(GalleryActivity.this, mAccount, links);
            mViewPager.setAdapter(mGalleryAdapter);

            // dynamically navigate to the starting page index selected by user
            // by default the starting page index is 0
            for (int i = 0; i< links.size(); i++) {
                String key = links.get(i);
                if (mThumbLinkAndSeafDirentMap.containsKey(key)
                        && mThumbLinkAndSeafDirentMap.get(key).name.equals(fileName)) {
                    // Log.d(DEBUG_TAG, "current index " + i);
                    // Log.d(DEBUG_TAG, "current file name " + fileName);
                    mViewPager.setCurrentItem(i);
                    mPageIndexTextView.setText(String.valueOf(i + 1));
                    mPageIndex = i;
                    mPageNameTextView.setText(fileName);
                    String linkKey = mGalleryAdapter.getItem(i);
                    if (mThumbLinkAndSeafDirentMap.containsKey(linkKey)) {
                        currentDirent = mThumbLinkAndSeafDirentMap.get(linkKey);
                    }
                    break;
                }
            }
            mPageCountTextView.setText(String.valueOf(links.size()));
        }
    }

    public void hideOrShowActionBar() {
        if (!showToolBar) {
            mToolbar.setVisibility(View.GONE);
            mPageIndexContainer.setVisibility(View.GONE);
            mPageNameTextView.setVisibility(View.GONE);
            showToolBar = !showToolBar;
        } else {
            mToolbar.setVisibility(View.VISIBLE);
            mPageIndexContainer.setVisibility(View.VISIBLE);
            mPageNameTextView.setVisibility(View.VISIBLE);
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
        chooseShareApp(repoID, path, false);
    }

    private void chooseShareApp(final String repoID, final String path, final boolean isdir) {
        final Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");

        // Get a list of apps
        List<ResolveInfo> infos = Utils.getAppsByIntent(shareIntent);

        String title = getString(isdir ? R.string.share_dir_link : R.string.share_file_link);

        AppChoiceDialog dialog = new AppChoiceDialog();
        dialog.addCustomAction(0, getResources().getDrawable(R.drawable.copy_link),
                getString(R.string.copy_link));
        dialog.init(title, infos, new AppChoiceDialog.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(AppChoiceDialog.CustomAction action) {
                final GetShareLinkDialog gdialog = new GetShareLinkDialog();
                gdialog.init(repoID, path, isdir, mAccount);
                gdialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
                    @Override
                    @SuppressWarnings("deprecation")
                    public void onTaskSuccess() {
                        ClipboardManager clipboard = (ClipboardManager)
                                getSystemService(Context.CLIPBOARD_SERVICE);
                        clipboard.setText(gdialog.getLink());
                        // ClipData clip = ClipData.newPlainText("seafile shared link", gdialog.getLink());
                        // clipboard.setPrimaryClip(clip);
                        ToastUtils.show(GalleryActivity.this, R.string.link_ready_to_be_pasted);
                    }
                });
                gdialog.show(getSupportFragmentManager(), "DialogFragment");
            }

            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                shareIntent.setClassName(packageName, className);

                final GetShareLinkDialog gdialog = new GetShareLinkDialog();
                gdialog.init(repoID, path, isdir, mAccount);
                gdialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
                    @Override
                    public void onTaskSuccess() {
                        shareIntent.putExtra(Intent.EXTRA_TEXT, gdialog.getLink());
                        startActivity(shareIntent);
                    }
                });
                gdialog.show(getSupportFragmentManager(), "DialogFragment");
            }

        });
        dialog.show(getSupportFragmentManager(), BrowserActivity.CHOOSE_APP_DIALOG_FRAGMENT_TAG);
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

