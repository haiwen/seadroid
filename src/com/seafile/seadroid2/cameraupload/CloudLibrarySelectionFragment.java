package com.seafile.seadroid2.cameraupload;

import android.content.Intent;
import android.database.Cursor;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.Fragment;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.*;
import android.widget.AdapterView.OnItemClickListener;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.*;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.adapter.DirentsAdapter;
import com.seafile.seadroid2.util.Utils;

import java.net.HttpURLConnection;
import java.util.List;

/**
 * Choose account and library for camera upload
 */
public class CloudLibrarySelectionFragment extends Fragment {
    public static final String DEBUG_TAG = "CloudLibrarySelectionFr";

    private static final int STEP_CHOOSE_ACCOUNT = 1;
    private static final int STEP_CHOOSE_REPO = 2;
    private static final int STEP_CHOOSE_DIR = 3;
    private int mStep = 1;

    private CameraUploadConfigActivity mActivity;
    private CloudLibraryAccountAdapter mAccountAdapter;
    private CloudLibraryAdapter mReposAdapter;
    private DirentsAdapter mDirentsAdapter;
    private AccountManager mAccountManager;
    private DataManager mDataManager;
    private NavContext mNavContext;
    private Account mAccount;
    private LoadAccountsTask mLoadAccountsTask;
    private LoadReposTask mLoadReposTask;
    private LoadDirTask mLoadDirTask;

    private RelativeLayout mUpLayout;
    private TextView mCurrentFolderText;
    private TextView mEmptyText, mErrorText;
    private ImageView mRefreshBtn;
    private View mProgressContainer, mListContainer;
    private ListView mFoldersListView;
    private Cursor mCursor;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        mActivity = (CameraUploadConfigActivity) getActivity();
        Intent intent = mActivity.getIntent();

        View rootView = getActivity().getLayoutInflater().inflate(R.layout.cuc_multi_selection_layout, null);
        mFoldersListView = (ListView) rootView.findViewById(R.id.cuc_multi_selection_lv);
        mFoldersListView.setFastScrollEnabled(true);
        mUpLayout = (RelativeLayout) rootView.findViewById(R.id.cuc_multi_selection_up_layout);
        mCurrentFolderText = (TextView) rootView.findViewById(R.id.cuc_multi_selection_current_directory_txt);
        mEmptyText = (TextView) rootView.findViewById(R.id.cuc_multi_selection_empty_msg);
        mErrorText = (TextView) rootView.findViewById(R.id.cuc_multi_selection_error_msg);
        mRefreshBtn = (ImageView) rootView.findViewById(R.id.cuc_multi_selection_refresh_iv);
        mProgressContainer = rootView.findViewById(R.id.cuc_multi_selection_progress_container);
        mListContainer = rootView.findViewById(R.id.cuc_multi_selection_list_container);
        mRefreshBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                try {
                    refreshList(true);
                } catch (Exception e) {
                    Log.e(DEBUG_TAG, "Unknown error", e);
                }
            }
        });

        mUpLayout.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                try {
                    stepBack();
                } catch (Exception e) {
                    Log.e(DEBUG_TAG, "Unknown error", e);
                }
            }

        });

        mFoldersListView.setOnItemClickListener(new OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                try {
                    onListItemClick(parent, position, id);
                } catch (Exception e) {
                    Log.e(DEBUG_TAG, "Unknown error", e);
                }
            }
        });

        chooseAccount();

        return rootView;
    }

    private void refreshList(final boolean forceRefresh) {
        switch (mStep) {
            case STEP_CHOOSE_ACCOUNT:
                if (mLoadAccountsTask != null && mLoadAccountsTask.getStatus() != AsyncTask.Status.FINISHED) {
                    return;
                } else {
                    chooseAccount(false);
                    break;
                }
            case STEP_CHOOSE_REPO:
                if (mLoadReposTask != null && mLoadReposTask.getStatus() != AsyncTask.Status.FINISHED) {
                    return;
                } else {
                    chooseRepo(forceRefresh);
                    break;
                }
            case STEP_CHOOSE_DIR:
                if (mLoadDirTask != null && mLoadDirTask.getStatus() != AsyncTask.Status.FINISHED) {
                    return;
                } else {
                    chooseDir(forceRefresh);
                    break;
                }
        }
    }

    public void onListItemClick(final View v, final int position, final long id) {
        NavContext nav = getNavContext();
        SeafRepo repo = null;

        if (mStep == STEP_CHOOSE_REPO) {
            repo = getReposAdapter().getItem(position);
        } else if (mStep == STEP_CHOOSE_DIR) {
            repo = getDataManager().getCachedRepoByID(nav.getRepoID());

        }

        switch (mStep) {
            case STEP_CHOOSE_ACCOUNT:
                setAccount(getAccountAdapter().getItem(position));
                setCurrentDirText(mAccount.getDisplayName());
                chooseRepo();
                break;
            case STEP_CHOOSE_REPO:
                nav.setRepoName(repo.name);
                nav.setRepoID(repo.id);
                nav.setDir("/", repo.root);
                chooseDir();

                setCurrentDirText(getString(R.string.settings_cuc_remote_lib_repo, repo.name));
                SeafRepo seafRepo = getReposAdapter().getItem(position);
                onRepoSelected(mAccount, seafRepo, nav.getDirPath());
                break;
            case STEP_CHOOSE_DIR:
                SeafDirent dirent = getDirentsAdapter().getItem(position);
                if (dirent.type == SeafDirent.DirentType.FILE) {
                    return;
                }

                nav.setDir(Utils.pathJoin(nav.getDirPath(), dirent.name), dirent.id);
                setCurrentDirText(getString(R.string.settings_cuc_remote_lib_repo,
                        getNavContext().getRepoName() + nav.getDirPath()));
                onRepoSelected(mAccount, mReposAdapter.selectedRepo, nav.getDirPath());
                refreshDir();
                break;
        }
    }

    private void onRepoSelected(Account account, SeafRepo seafRepo, String directory) {
        mActivity.saveCameraUploadInfo(account, seafRepo, directory);
        getReposAdapter().setSelectedRepo(seafRepo);
        getReposAdapter().notifyDataSetChanged();
    }

    private void stepBack() {
        stepBack(false);
    }

    private void stepBack(boolean cancelIfFirstStep) {
        switch (mStep) {
            case STEP_CHOOSE_ACCOUNT:
                if (cancelIfFirstStep) {
                    mActivity.finish();
                }
                mUpLayout.setVisibility(View.INVISIBLE);
                break;
            case STEP_CHOOSE_REPO:
                setCurrentDirText(getString(R.string.settings_cuc_remote_lib_account));
                chooseAccount(false);
                break;
            case STEP_CHOOSE_DIR:
                if (getNavContext().isRepoRoot()) {
                    setCurrentDirText(getAccountManager().getCurrentAccount().getEmail());
                    chooseRepo();
                } else {
                    String path = getNavContext().getDirPath();
                    setCurrentDirText(getString(R.string.settings_cuc_remote_lib_repo,
                            getNavContext().getRepoName() + Utils.getParentPath(path)));
                    getNavContext().setDir(Utils.getParentPath(path), null);
                    refreshDir();
                }
                break;
        }
    }

    private void chooseDir() {
        chooseDir(false);
    }

    private void chooseDir(boolean forceRefresh) {
        mStep = STEP_CHOOSE_DIR;
        mUpLayout.setVisibility(View.VISIBLE);
        mEmptyText.setText(R.string.dir_empty);

        setListAdapter(getDirentsAdapter());
        refreshDir(forceRefresh);
    }

    private void chooseAccount() {
        chooseAccount(true);
    }

    /**
     * List all accounts
     */
    private void chooseAccount(boolean forwardIfOnlyOneAccount) {
        mStep = STEP_CHOOSE_ACCOUNT;
        mUpLayout.setVisibility(View.INVISIBLE);
        mEmptyText.setText(R.string.no_account);
        setCurrentDirText(getString(R.string.settings_cuc_remote_lib_account));

        mLoadAccountsTask = new LoadAccountsTask(getAccountManager(), forwardIfOnlyOneAccount);

        ConcurrentAsyncTask.execute(mLoadAccountsTask);
        setListAdapter(getAccountAdapter());
    }

    /**
     * List all repos
     */
    private void chooseRepo() {
        chooseRepo(false);
    }

    private void chooseRepo(boolean forceRefresh) {
        mStep = STEP_CHOOSE_REPO;
        mUpLayout.setVisibility(View.VISIBLE);
        setCurrentDirText(mAccount.getDisplayName());

        setListAdapter(getReposAdapter());

        getNavContext().setRepoID(null);

        if (!Utils.isNetworkOn() || !forceRefresh) {
            List<SeafRepo> repos = getDataManager().getReposFromCache();
            if (repos != null) {
                updateAdapterWithRepos(repos);
                return;
            }
        }

        mLoadReposTask = new LoadReposTask(getDataManager());
        ConcurrentAsyncTask.execute(mLoadReposTask);
    }

    private void refreshDir() {
        refreshDir(false);
    }

    private void refreshDir(boolean forceRefresh) {
        String repoID = getNavContext().getRepoID();
        String dirPath = getNavContext().getDirPath();

        if (!Utils.isNetworkOn() || !forceRefresh) {
            List<SeafDirent> dirents = getDataManager().getCachedDirents(
                    getNavContext().getRepoID(), getNavContext().getDirPath());
            if (dirents != null) {
                updateAdapterWithDirents(dirents);
                return;
            }
        }

        mLoadDirTask = new LoadDirTask(repoID, dirPath, getDataManager());
        ConcurrentAsyncTask.execute(mLoadDirTask);
    }

    private void updateAdapterWithDirents(List<SeafDirent> dirents) {
        getDirentsAdapter().setDirents(dirents);
        showListOrEmptyText(dirents.size());
    }

    private void updateAdapterWithRepos(List<SeafRepo> repos) {
        // remove encrypted repos in order to "hide" them in selection list
        List<SeafRepo> filteredRepos = Lists.newArrayList();
        for (SeafRepo repo : repos) {
            if (!repo.encrypted)
                filteredRepos.add(repo);
        }
        getReposAdapter().setRepos(filteredRepos);
        showListOrEmptyText(filteredRepos.size());
    }

    private CloudLibraryAccountAdapter getAccountAdapter() {
        if (mAccountAdapter == null) {
            mAccountAdapter = new CloudLibraryAccountAdapter(mActivity);
        }

        return mAccountAdapter;
    }

    private CloudLibraryAdapter getReposAdapter() {
        if (mReposAdapter == null) {
            mReposAdapter = new CloudLibraryAdapter(true, null);
        }

        return mReposAdapter;
    }

    private DirentsAdapter getDirentsAdapter() {
        if (mDirentsAdapter == null) {
            mDirentsAdapter = new DirentsAdapter();
        }

        return mDirentsAdapter;
    }

    private void showListOrEmptyText(int listSize) {
        if (listSize == 0) {
            mFoldersListView.setVisibility(View.GONE);
            mEmptyText.setVisibility(View.VISIBLE);
        } else {
            mFoldersListView.setVisibility(View.VISIBLE);
            mEmptyText.setVisibility(View.GONE);
        }
    }

    private void setListAdapter(BaseAdapter adapter) {
        mFoldersListView.setAdapter(adapter);
    }

    private DataManager getDataManager() {
        if (mDataManager == null) {
            mDataManager = new DataManager(mAccount);
        }

        return mDataManager;
    }

    private void setAccount(Account account) {
        mAccount = account;
        mDataManager = new DataManager(account);
    }

    private AccountManager getAccountManager() {
        if (mAccountManager == null) {
            mAccountManager = new AccountManager(getActivity());
        }

        return mAccountManager;
    }

    private NavContext getNavContext() {
        if (mNavContext == null) {
            mNavContext = new NavContext();
        }

        return mNavContext;
    }

    /**
     * Sets the current directory's text.
     */
    private void setCurrentDirText(String text) {
        mCurrentFolderText.setText(text);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        getActivity().finish();
    }

    @Override
    public void onPause() {
        super.onPause();
        getActivity().finish();
    }

    @Override
    public void onDestroyView() {
        super.onDestroyView();

        if (isRemoving()) {
            mCursor.close();
            mCursor = null;
        }

    }

    private class LoadAccountsTask extends AsyncTask<Void, Void, Void> {
        private List<Account> accounts;
        private Exception err;
        private AccountManager accountManager;
        private boolean forwardIfOnlyOneAccount;

        public LoadAccountsTask(AccountManager accountManager, boolean forwardIfOnlyOneAccount) {
            this.accountManager = accountManager;
            this.forwardIfOnlyOneAccount = forwardIfOnlyOneAccount;
        }

        @Override
        protected void onPreExecute() {
            showLoading(true);
        }

        @Override
        protected Void doInBackground(Void... params) {
            try {
                accounts = accountManager.getSignedInAccountList();
            } catch (Exception e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            showLoading(false);
            if (err != null || accounts == null) {
                setErrorMessage(R.string.load_accounts_fail);
                if (err != null) {
                    Log.d(DEBUG_TAG, "failed to load accounts: " + err.getMessage());
                }
                return;
            }

            if (accounts.size() == 1 && forwardIfOnlyOneAccount) {
                // Only 1 account. Go to next step.
                setAccount(accounts.get(0));
                chooseRepo();
                return;
            }

            CloudLibraryAccountAdapter adapter = getAccountAdapter();
            adapter.clear();
            for (Account account: accounts) {
                adapter.add(account);
            }
            adapter.notifyDataSetChanged();
            showListOrEmptyText(accounts.size());
        }
    }

    private class LoadReposTask extends AsyncTask<Void, Void, Void> {
        private List<SeafRepo> repos;
        private SeafException err;
        private DataManager dataManager;

        public LoadReposTask(DataManager dataManager) {
            this.dataManager = dataManager;
        }

        @Override
        protected Void doInBackground(Void... params) {
            try {
                repos = dataManager.getReposFromServer();
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPreExecute() {
            showLoading(true);
        }

        @Override
        protected void onPostExecute(Void v) {
            if (mStep != STEP_CHOOSE_REPO) {
                return;
            }

            showLoading(false);
            if (err != null || repos == null) {
                setErrorMessage(R.string.load_libraries_fail);
                Log.d(DEBUG_TAG, "failed to load repos: " + (err != null ? err.getMessage() : " no error present"));
                return;
            }

            updateAdapterWithRepos(repos);
        }
    }

    private class LoadDirTask extends AsyncTask<Void, Void, Void> {
        private String repoID, dirPath;
        private SeafException err;
        private DataManager dataManager;
        private List<SeafDirent> dirents;

        public LoadDirTask(String repoID, String dirPath, DataManager dataManager) {
            this.repoID = repoID;
            this.dirPath = dirPath;
            this.dataManager = dataManager;
        }

        @Override
        protected void onPreExecute() {
            showLoading(true);
        }

        @Override
        protected Void doInBackground(Void... params) {
            try {
                dirents = dataManager.getDirentsFromServer(repoID, dirPath);
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            if (mStep != STEP_CHOOSE_DIR) {
                return;
            }

            getDirentsAdapter().clearDirents();
            showLoading(false);
            if (err != null) {
                int retCode = err.getCode();
                if (retCode == HttpURLConnection.HTTP_NOT_FOUND) {
                    ToastUtils.show(mActivity, String.format("The folder \"%s\" was deleted", dirPath));
                } else {
                    Log.d(DEBUG_TAG, "failed to load dirents: " + err.getMessage());
                    err.printStackTrace();
                    setErrorMessage(R.string.load_dir_fail);
                }
                return;
            }

            if (dirents == null) {
                Log.d(DEBUG_TAG, "failed to load dirents: no error present");
                setErrorMessage(R.string.load_dir_fail);
                return;
            }

            updateAdapterWithDirents(dirents);
        }
    }

    private void setErrorMessage(int resID) {
        //mContentArea.setVisibility(View.GONE);
        mErrorText.setVisibility(View.VISIBLE);
        mErrorText.setText(getString(resID));
    }

    private void clearError() {
        mErrorText.setVisibility(View.GONE);
        //mContentArea.setVisibility(View.VISIBLE);
    }

    private void showLoading(boolean loading) {
        clearError();
        if (loading) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_in));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            mListContainer.setVisibility(View.INVISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_out));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_in));

            mProgressContainer.setVisibility(View.GONE);
            mListContainer.setVisibility(View.VISIBLE);
        }
    }

}

