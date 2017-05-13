package com.seafile.seadroid2.ui.activity;

import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.WindowManager;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.ui.HackyViewPager;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafPhoto;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.ZoomOutPageTransformer;
import com.seafile.seadroid2.ui.adapter.GalleryAdapter;
import com.seafile.seadroid2.ui.adapter.SeafItemAdapter;
import com.seafile.seadroid2.ui.dialog.DeleteFileDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;

import java.util.Collections;
import java.util.List;

/**
 * A gallery of images with sliding, zooming, multi-touch and single touch support
 * Local cached images will be shown directly, while cloud images will be asynchronously downloaded first
 */
public class GalleryActivity extends BaseActivity {
    public static final String DEBUG_TAG = "GalleryActivity";

    private HackyViewPager mViewPager;
    private LinearLayout mPageIndexContainer;
    private TextView mPageIndexTextView;
    private TextView mPageCountTextView;
    private TextView mPageNameTextView;
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
    private int mPageIndex;
    private GalleryAdapter mGalleryAdapter;
    private List<SeafPhoto> mPhotos = Lists.newArrayList();

    /** flag to mark if the tool bar was shown */
    private boolean showToolBar = true;
    private View.OnClickListener onClickListener = new View.OnClickListener() {
        @Override
        public void onClick(View v) {
            switch (v.getId()) {
                case R.id.gallery_delete_photo:
                    deleteFile(repoID, Utils.pathJoin(dirPath, fileName));
                    break;
                case R.id.gallery_star_photo:
                    starFile(repoID, dirPath, fileName);
                    break;
                case R.id.gallery_share_photo:
                    shareFile(repoID, false, Utils.pathJoin(dirPath, fileName));
                    break;
            }
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        getWindow().setFlags(WindowManager.LayoutParams.FLAG_FULLSCREEN, WindowManager.LayoutParams.FLAG_FULLSCREEN);
        setContentView(R.layout.gallery_activity_layout);

        mDeleteBtn = (ImageView) findViewById(R.id.gallery_delete_photo);
        mStarBtn = (ImageView) findViewById(R.id.gallery_star_photo);
        mShareBtn = (ImageView) findViewById(R.id.gallery_share_photo);
        mToolbar = (LinearLayout) findViewById(R.id.gallery_tool_bar);
        mDeleteBtn.setOnClickListener(onClickListener);
        mStarBtn.setOnClickListener(onClickListener);
        mShareBtn.setOnClickListener(onClickListener);
        mViewPager = (HackyViewPager) findViewById(R.id.gallery_pager);
        mViewPager.setPageTransformer(true, new ZoomOutPageTransformer());
        mViewPager.setOffscreenPageLimit(1);

        mViewPager.setOnPageChangeListener(new HackyViewPager.OnPageChangeListener() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                // page index starting from 1 instead of 0 in user interface, so plus one here
                mPageIndexTextView.setText(String.valueOf(position + 1));
                mPageIndex = position;
                // fixed IndexOutOfBoundsException when accessing list
                if (mPageIndex == mPhotos.size()) return;
                fileName = mPhotos.get(mPageIndex).getName();
                mPageNameTextView.setText(fileName);
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

        displayPhotosInGallery(repoName, repoID, dirPath);
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

    /**
     * Load thumbnail urls in order to display them in the gallery.
     * Prior to use caches to calculate those urls.
     * If caches are not available, load them asynchronously.
     *
     * NOTE: When user browsing files in "LIBRARY" tab, he has to navigate into a repo in order to open gallery.
     * Method which get called is {@link com.seafile.seadroid2.ui.fragment.ReposFragment#navToReposView(boolean)} or {@link com.seafile.seadroid2.ui.fragment.ReposFragment#navToDirectory(boolean)},
     * so seafDirents were already cached and it will always use them to calculate thumbnail urls for displaying photos in gallery.
     * But for browsing "STARRED" tab, caches of starred files may or may not cached, that is where the asynchronous loading code segment comes into use.
     * @param repoID
     * @param dirPath
     */
    private void displayPhotosInGallery(String repoName, String repoID, String dirPath) {
        // calculate thumbnail urls by cached dirents
        List<SeafDirent> seafDirents = dataMgr.getCachedDirents(repoID, dirPath);
        if (seafDirents != null) {
            // sort files by type and order
            seafDirents = sortFiles(seafDirents,
                    SettingsManager.instance().getSortFilesTypePref(),
                    SettingsManager.instance().getSortFilesOrderPref());
            for (SeafDirent seafDirent : seafDirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) {
                    mPhotos.add(new SeafPhoto(repoName, repoID, dirPath, seafDirent));
                }
            }

            mGalleryAdapter = new GalleryAdapter(GalleryActivity.this,
                    mAccount,
                    mPhotos,
                    dataMgr);
            mViewPager.setAdapter(mGalleryAdapter);

            navToSelectedPage();
        } else {
            if (!Utils.isNetworkOn()) {
                showShortToast(this, R.string.network_down);
                // data is not available
                finish();
            }

            // load photos asynchronously
            LoadPhotosTask task = new LoadPhotosTask(repoName, repoID, dirPath);
            ConcurrentAsyncTask.execute(task);
        }

    }

    /**
     * Load photos asynchronously, use {@link SeafPhoto} to manage state of each photo instance
     */
    private class LoadPhotosTask extends AsyncTask<String, Void, List<SeafPhoto>> {
        private String repoName, repoID, dirPath;
        private SeafException err = null;

        public LoadPhotosTask(String repoName, String repoID, String dirPath) {
            this.repoName = repoName;
            this.repoID = repoID;
            this.dirPath = dirPath;
        }

        @Override
        protected List<SeafPhoto> doInBackground(String... params) {
            List<SeafPhoto> photos = Lists.newArrayList();
            List<SeafDirent> seafDirents;
            try {
                seafDirents = dataMgr.getDirentsFromServer(repoID, dirPath);
            } catch (SeafException e) {
                err = e;
                return null;
            }

            if (seafDirents == null)
                return null;

            // sort photos according to global sort settings
            seafDirents = sortFiles(seafDirents,
                    SettingsManager.instance().getSortFilesTypePref(),
                    SettingsManager.instance().getSortFilesOrderPref());
            for (SeafDirent seafDirent : seafDirents) {
                if (!seafDirent.isDir()
                        && Utils.isViewableImage(seafDirent.name)) {
                    photos.add(new SeafPhoto(repoName, repoID, dirPath, seafDirent));
                }
            }
            return photos;
        }

        @Override
        protected void onPostExecute(List<SeafPhoto> photos) {
            if (photos.isEmpty()
                    || fileName == null) {
                if (err != null) {
                    showShortToast(GalleryActivity.this, R.string.gallery_load_photos_error);
                    Log.e(DEBUG_TAG, "error message " + err.getMessage() + " error code " + err.getCode());
                }

                return;
            }

            mPhotos = photos;
            mGalleryAdapter = new GalleryAdapter(GalleryActivity.this, mAccount, photos, dataMgr);
            mViewPager.setAdapter(mGalleryAdapter);

            navToSelectedPage();
        }
    }

    /**
     * Sorts the given list by type and order.
     * Sorting type is one of {@link SeafItemAdapter#SORT_BY_NAME} or {@link SeafItemAdapter#SORT_BY_LAST_MODIFIED_TIME}.
     * Sorting order is one of {@link SeafItemAdapter#SORT_ORDER_ASCENDING} or {@link SeafItemAdapter#SORT_ORDER_DESCENDING}.
     *
     * @param dirents
     * @param type
     * @param order
     * @return sorted file list
     */
    public List<SeafDirent> sortFiles(List<SeafDirent> dirents, int type, int order) {
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
        int size = mPhotos.size();
        for (int i = 0; i < size; i++) {
            if (mPhotos.get(i).getName().equals(fileName)) {
                mViewPager.setCurrentItem(i);
                mPageIndexTextView.setText(String.valueOf(i + 1));
                mPageIndex = i;
                mPageNameTextView.setText(fileName);
                break;
            }
        }
        mPageCountTextView.setText(String.valueOf(size));
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
        } else {
            mToolbar.setVisibility(View.GONE);
            mPageIndexContainer.setVisibility(View.GONE);
            mPageNameTextView.setVisibility(View.GONE);
        }
        showToolBar = !showToolBar;

    }

    private void deleteFile(String repoID, String path) {
        final DeleteFileDialog dialog = new DeleteFileDialog();
        dialog.init(repoID, path, false, mAccount);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showShortToast(GalleryActivity.this, R.string.delete_successful);
                removePageAndRefreshView();
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    private void starFile(String repoId, String dir, String fileName) {
        if (!Utils.isNetworkOn()) {
            showShortToast(this, R.string.network_down);
            return;
        }

        String p = Utils.pathJoin(dir, fileName);
        ConcurrentAsyncTask.execute(new StarFileTask(repoId, p));
    }

    private void shareFile(String repoID, boolean isEncrypt, String path) {
        if (isEncrypt) {
            WidgetUtils.inputSharePassword(this, repoID, path, false, mAccount);
        } else {
            WidgetUtils.chooseShareApp(this, repoID, path, false, mAccount, null, null);
        }
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
                showShortToast(GalleryActivity.this, R.string.star_file_failed);
                return;
            }

            showShortToast(GalleryActivity.this, R.string.star_file_succeed);
        }
    }

    /**
     * slide to next page if there are pages on the right side of the current one,
     * slide to previous page if not,
     * quit the gallery if both cases were not met
     */
    private void removePageAndRefreshView() {
        mPhotos.remove(mPageIndex);
        mGalleryAdapter.setItems(mPhotos);
        mGalleryAdapter.notifyDataSetChanged();
        int size = mPhotos.size();
        mPageCountTextView.setText(String.valueOf(size));

        if (size == 0) {
            finish();
            return;
        }

        mPageIndex = mPageIndex > size - 1 ? size -1 : mPageIndex;
        // page index starting from 1 instead of 0 in user interface, so plus one here
        mPageIndexTextView.setText(String.valueOf(mPageIndex + 1));

        // update file name in gallery view
        mPageNameTextView.setText(mPhotos.get(mPageIndex).getName());

    }

}

