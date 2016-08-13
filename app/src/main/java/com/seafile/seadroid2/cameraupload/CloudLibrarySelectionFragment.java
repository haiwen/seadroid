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
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.avatar.Avatar;
import com.seafile.seadroid2.avatar.AvatarManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.NavContext;
import com.seafile.seadroid2.ui.adapter.DirentsAdapter;
import com.seafile.seadroid2.ui.dialog.PasswordDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.List;

/**
 * Choose account and library for camera upload
 */
public class CloudLibrarySelectionFragment extends Fragment {
    public static final String DEBUG_TAG = "CloudLibrarySelectionFragment";

    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "passwordDialogFragmentTag";
    public static final String ONLY_SHOW_WRITABLE_REPOS = "onlyShowWritableRepos";
    public static final String SHOW_ENCRYPTED_REPOS = "showEncryptedRepos";
    public static final String ENCRYPTED_REPO_ID = "encryptedRepoId";

    public static final String DATA_REPO_ID = "repoID";
    public static final String DATA_REPO_NAME = "repoNAME";
    public static final String DATA_DIR = "dir";
    public static final String DATA_ACCOUNT = "account";

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
    private AvatarManager avatarManager;

    private RelativeLayout mUpLayout;
    private TextView mCurrentFolderText;
    private TextView mEmptyText, mErrorText;
    private ImageView mRefreshBtn;
    private View mProgressContainer, mListContainer;
    private ListView mFoldersListView;
    private Cursor mCursor;
    private String mCurrentDir;

    private boolean canChooseAccount;
    private boolean onlyShowWritableRepos;
    private String encryptedRepoId;

    /** only show repo list for camera upload */
    private boolean isOnlyChooseRepo;

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        mActivity = (CameraUploadConfigActivity) getActivity();
        Intent intent = mActivity.getIntent();
        avatarManager = new AvatarManager();
        Account account = intent.getParcelableExtra("account");
        if (account == null) {
            canChooseAccount = true;
        } else {
            mAccount = account;
        }
        onlyShowWritableRepos = intent.getBooleanExtra(ONLY_SHOW_WRITABLE_REPOS, true);
        encryptedRepoId = intent.getStringExtra(ENCRYPTED_REPO_ID);
        isOnlyChooseRepo = true;

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
                refreshList(true);
            }
        });

        mUpLayout.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                try {
                    stepBack();
                } catch (Exception e) {
                    e.printStackTrace();
                }

            }

        });

        mFoldersListView.setOnItemClickListener(new OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                onListItemClick(parent, position, id);
            }
        });

        if (canChooseAccount) {
            chooseAccount();
        } else {
            chooseRepo();
        }

        return rootView;
    }

    @Override
    public void onResume() {
        super.onResume();
        loadAvatarUrls(48);
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
                    SeafRepo repo = getDataManager().getCachedRepoByID(getNavContext().getRepoID());
                    if (repo.encrypted && !getDataManager().getRepoPasswordSet(repo.id)) {
                        String password = getDataManager().getRepoPassword(repo.id);
                        showPasswordDialog(repo.name, repo.id,
                                new TaskDialog.TaskDialogListener() {
                                    @Override
                                    public void onTaskSuccess() {
                                        chooseRepo(forceRefresh);
                                    }
                                } , password);
                    }
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
            //mCurrentFolderText.setText(nav.getRepoName());
        } else if (mStep == STEP_CHOOSE_DIR) {
            repo = getDataManager().getCachedRepoByID(nav.getRepoID());

        }

        if (repo != null) {
            if (repo.encrypted && !getDataManager().getRepoPasswordSet(repo.id)) {
                String password = getDataManager().getRepoPassword(repo.id);
                showPasswordDialog(repo.name, repo.id, new TaskDialog.TaskDialogListener() {
                    @Override
                    public void onTaskSuccess() {
                        onListItemClick(v, position, id);
                    }
                }, password);

                return;
            }
        }

        switch (mStep) {
            case STEP_CHOOSE_ACCOUNT:
                setAccount(getAccountAdapter().getItem(position));
                mCurrentDir = mAccount.getDisplayName();
                setCurrentDirText(mCurrentDir);
                chooseRepo();
                break;
            case STEP_CHOOSE_REPO:
                if (!isOnlyChooseRepo) {
                    nav.setRepoName(repo.name);
                    nav.setRepoID(repo.id);
                    nav.setDir("/", repo.root);
                    chooseDir();
                }
                mCurrentDir = getString(R.string.settings_cuc_remote_lib_repo, repo.name);
                setCurrentDirText(mCurrentDir);
                SeafRepo seafRepo = getReposAdapter().getItem(position);
                onRepoSelected(mAccount, seafRepo);
                break;
            case STEP_CHOOSE_DIR:
                SeafDirent dirent = getDirentsAdapter().getItem(position);
                mCurrentDir += "/" + dirent.name;
                setCurrentDirText(mCurrentDir);

                if (dirent.type == SeafDirent.DirentType.FILE) {
                    return;
                }

                nav.setDir(Utils.pathJoin(nav.getDirPath(), dirent.name), dirent.id);
                refreshDir();
                break;
        }
    }

    private void onRepoSelected(Account account, SeafRepo seafRepo) {
        mActivity.saveCameraUploadInfo(account, seafRepo);
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
                if (canChooseAccount) {
                    mCurrentDir = getString(R.string.settings_cuc_remote_lib_account);
                    setCurrentDirText(mCurrentDir);
                    chooseAccount(false);
                } else if (cancelIfFirstStep) {
                    mActivity.finish();
                }
                break;
            case STEP_CHOOSE_DIR:
                if (getNavContext().isRepoRoot()) {
                    mCurrentDir = getAccountManager().getCurrentAccount().getEmail();
                    setCurrentDirText(mCurrentDir);
                    chooseRepo();
                } else {
                    String path = getNavContext().getDirPath();
                    mCurrentDir = getNavContext().getRepoName() + Utils.getParentPath(path);
                    setCurrentDirText(mCurrentDir);
                    getNavContext().setDir(Utils.getParentPath(path), null);
                    refreshDir();
                }
                break;
        }
    }

    private void showPasswordDialog() {
        NavContext nav = getNavContext();
        String repoName = nav.getRepoName();
        String repoID = nav.getRepoID();

        showPasswordDialog(repoName, repoID, new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                refreshDir();
            }
        }, null);
    }

    public void showPasswordDialog(String repoName, String repoID,
                                   TaskDialog.TaskDialogListener listener, String password) {
        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.setRepo(repoName, repoID, mAccount);
        if (password != null) {
            passwordDialog.setPassword(password);
        }
        passwordDialog.setTaskDialogLisenter(listener);
        passwordDialog.show(mActivity.getSupportFragmentManager(), PASSWORD_DIALOG_FRAGMENT_TAG);
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
        mCurrentDir = getString(R.string.settings_cuc_remote_lib_account);
        setCurrentDirText(mCurrentDir);

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
        mCurrentDir = mAccount.getDisplayName();
        setCurrentDirText(mCurrentDir);

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
            mReposAdapter = new CloudLibraryAdapter(onlyShowWritableRepos, encryptedRepoId);
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
                accounts = accountManager.getAccountList();
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
                if (retCode == SeafConnection.HTTP_STATUS_REPO_PASSWORD_REQUIRED) {
                    showPasswordDialog();
                } else if (retCode == HttpURLConnection.HTTP_NOT_FOUND) {
                    final String message = String.format(getString(R.string.op_exception_folder_deleted), dirPath);
                    mActivity.showShortToast(mActivity, message);
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

    /**
     * asynchronously load avatars
     *
     * @param avatarSize set a avatar size in one of 24*24, 32*32, 48*48, 64*64, 72*72, 96*96
     */
    public void loadAvatarUrls(int avatarSize) {
        List<Avatar> avatars;

        if (!Utils.isNetworkOn() || !avatarManager.isNeedToLoadNewAvatars()) {
            // Toast.makeText(AccountsActivity.this, getString(R.string.network_down), Toast.LENGTH_SHORT).show();

            // use cached avatars
            avatars = avatarManager.getAvatarList();

            if (avatars == null) {
                return;
            }

            // set avatars url to adapter
            mAccountAdapter.setAvatars((ArrayList<Avatar>) avatars);

            // notify adapter data changed
            mAccountAdapter.notifyDataSetChanged();

            return;
        }

        LoadAvatarUrlsTask task = new LoadAvatarUrlsTask(avatarSize);

        ConcurrentAsyncTask.execute(task);

    }

    private class LoadAvatarUrlsTask extends AsyncTask<Void, Void, List<Avatar>> {

        private List<Avatar> avatars;
        private int avatarSize;
        private SeafConnection httpConnection;

        public LoadAvatarUrlsTask(int avatarSize) {
            this.avatarSize = avatarSize;
            this.avatars = Lists.newArrayList();
        }

        @Override
        protected List<Avatar> doInBackground(Void... params) {
            // reuse cached avatars
            avatars = avatarManager.getAvatarList();

            // contains accounts who don`t have avatars yet
            List<Account> acts = avatarManager.getAccountsWithoutAvatars();

            // contains new avatars in order to persist them to database
            List<Avatar> newAvatars = new ArrayList<Avatar>(acts.size());

            // load avatars from server
            for (Account account : acts) {
                httpConnection = new SeafConnection(account);

                String avatarRawData = null;
                try {
                    avatarRawData = httpConnection.getAvatar(account.getEmail(), avatarSize);
                } catch (SeafException e) {
                    e.printStackTrace();
                    return avatars;
                }

                Avatar avatar = avatarManager.parseAvatar(avatarRawData);
                if (avatar == null)
                    continue;

                avatar.setSignature(account.getSignature());

                avatars.add(avatar);

                newAvatars.add(avatar);
            }

            // save new added avatars to database
            avatarManager.saveAvatarList(newAvatars);

            return avatars;
        }

        @Override
        protected void onPostExecute(List<Avatar> avatars) {
            if (avatars == null) {
                return;
            }

            // set avatars url to adapter
            mAccountAdapter.setAvatars((ArrayList<Avatar>) avatars);

            // notify adapter data changed
            mAccountAdapter.notifyDataSetChanged();
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

