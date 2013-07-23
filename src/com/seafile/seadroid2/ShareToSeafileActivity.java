package com.seafile.seadroid2;

import java.util.List;

import android.content.ComponentName;
import android.content.ContentResolver;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.database.Cursor;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.provider.MediaStore.Images;
import android.util.Log;
import android.view.View;
import android.view.animation.AnimationUtils;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockListActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.TransferService.TransferBinder;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.DirentsAdapter;
import com.seafile.seadroid2.ui.ReposAdapter;


public class ShareToSeafileActivity extends SherlockListActivity {
    private static final String DEBUG_TAG = "ShareToSeafileActivity";

    public static final String SHARE_SOURCE_URI = "share.src.uri";

    private NavContext mNavContext;

    private TransferService mTxService;
    private Account mAccount;

    private AccountManager mAccountManager;
    private DataManager mDataManager;

    private AccountAdapter mAccountAdapter;
    private ReposAdapter mReposAdapter;
    private DirentsAdapter mDirentsAdapter;

    private LoadDirTask mLoadDirTask;
    private LoadReposTask mLoadReposTask;
    private LoadAccountsTask mLoadAccountsTask;

    private View mProgressContainer, mListContainer, mContentArea;
    private Button mOkButton, mCancelButton;
    private TextView mEmptyText, mErrorText;
    private ListView mListView;

    private static final int STEP_CHOOSE_ACCOUNT = 1;
    private static final int STEP_CHOOSE_REPO = 2;
    private static final int STEP_CHOOSE_DIR = 3;
    private int mStep = 1;

    private ServiceConnection mConnection;

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.share_to_seafile);

        Intent intent = getIntent();

        Uri uri = (Uri)intent.getExtras().get(Intent.EXTRA_STREAM);

        if (uri == null) {
            findViewById(R.id.main).setVisibility(View.GONE);
            return;
        }

        findViewById(R.id.not_supported_text).setVisibility(View.GONE);

        final String localPath = getSharedFilePath(uri);
        Log.d(DEBUG_TAG, "share " + localPath);

        ActionBar bar = getSupportActionBar();
        bar.setNavigationMode(ActionBar.NAVIGATION_MODE_STANDARD);
        bar.setDisplayHomeAsUpEnabled(false);

        mOkButton = (Button)findViewById(R.id.ok);
        mCancelButton = (Button)findViewById(R.id.cancel);

        mListView = (ListView) findViewById(android.R.id.list);
        mEmptyText = (TextView) findViewById(android.R.id.empty);
        mErrorText = (TextView) findViewById(R.id.error_message);
        mListContainer = findViewById(R.id.listContainer);
        mProgressContainer = findViewById(R.id.progressContainer);
        mContentArea = findViewById(R.id.content);

        mOkButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String repoName = mNavContext.getRepoName();
                String repoID = mNavContext.getRepoID();
                String dir = mNavContext.getDirPath();
                addUploadTask(repoName, repoID, dir, localPath);
            }
        });

        mCancelButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                finish();
            }
        });

        mListView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);

        chooseAccount();
    }

    private String getSharedFilePath(Uri uri) {
        if (uri.getScheme().equals("file")) {
            return uri.getPath();
        } else {
            ContentResolver contentResolver = getContentResolver();
            Cursor cursor = contentResolver.query(uri, null, null, null, null);
            cursor.moveToFirst();
            String filePath = cursor.getString(cursor.getColumnIndex(Images.Media.DATA));
            return filePath;
        }
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");
        if (mTxService != null) {
            unbindService(mConnection);
            mTxService = null;
        }

        if (mLoadReposTask != null
            && mLoadReposTask.getStatus() != AsyncTask.Status.FINISHED) {
            mLoadReposTask.cancel(true);
        }

        if (mLoadDirTask != null
            && mLoadDirTask.getStatus() != AsyncTask.Status.FINISHED) {
            mLoadDirTask.cancel(true);
        }

        if (mLoadAccountsTask != null
            && mLoadAccountsTask.getStatus() != AsyncTask.Status.FINISHED) {
            mLoadAccountsTask.cancel(true);
        }

        super.onDestroy();
    }

    public void onListItemClick(ListView l, View v, int position, long id) {
        NavContext nav = getNavContext();
        switch (mStep) {
        case STEP_CHOOSE_ACCOUNT:
            mAccount = getAccountAdapter().getItem(position);
            chooseRepo();
            break;
        case STEP_CHOOSE_REPO:
            SeafRepo repo = getReposAdapter().getItem(position);
            nav.setRepoName(repo.name);
            nav.setRepoID(repo.id);
            nav.setDir("/", repo.root);
            chooseDir();
            break;
        case STEP_CHOOSE_DIR:
            SeafDirent dirent = getDirentsAdapter().getItem(position);
            if (dirent.type == SeafDirent.DirentType.FILE) {
                return;
            }

            nav.setDir(Utils.pathJoin(nav.getDirPath(), dirent.name), dirent.id);
            refreshDir();
            break;
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getSupportMenuInflater();
        inflater.inflate(R.menu.share_to_seafile_menu, menu);
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        MenuItem menuRefresh = menu.findItem(R.id.refresh);
        menuRefresh.setVisible(true);
        menuRefresh.setEnabled(true);
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
        case android.R.id.home:
            stepBack();
            return true;
        case R.id.refresh:
            refreshList();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void refreshList() {
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
                chooseRepo();
                break;
            }
        case STEP_CHOOSE_DIR:
            if (mLoadDirTask != null && mLoadDirTask.getStatus() != AsyncTask.Status.FINISHED) {
                return;
            } else {
                chooseDir();
                break;
            }
        }
    }

    private void stepBack() {
        switch (mStep) {
        case STEP_CHOOSE_ACCOUNT:
            break;
        case STEP_CHOOSE_REPO:
            chooseAccount(false);
            break;
        case STEP_CHOOSE_DIR:
            if (getNavContext().isRepoRoot()) {
                chooseRepo();
            } else {
                String path = getNavContext().getDirPath();
                getNavContext().setDir(Utils.getParentPath(path), null);
                refreshDir();
            }
            break;
        }
    }

    /**
     * List all accounts
     */
    private void chooseAccount(boolean forwardIfOnlyOneAccount) {
        mStep = STEP_CHOOSE_ACCOUNT;
        mEmptyText.setText(R.string.no_account);

        mLoadAccountsTask = new LoadAccountsTask(getAccountManager(), forwardIfOnlyOneAccount);

        ConcurrentAsyncTask.execute(mLoadAccountsTask);
        setListAdapter(getAccountAdapter());
        mOkButton.setVisibility(View.GONE);

        // update action bar
        ActionBar bar = getSupportActionBar();
        bar.setDisplayHomeAsUpEnabled(false);
        bar.setTitle(R.string.choose_an_account);
    }

    private void chooseAccount() {
        chooseAccount(true);
    }

    /**
     * List all repos
     */
    private void chooseRepo() {
        mStep = STEP_CHOOSE_REPO;
        mEmptyText.setText(R.string.no_library);

        setListAdapter(getReposAdapter());
        mOkButton.setVisibility(View.GONE);

        getNavContext().setRepoID(null);

        showLoading(true);
        mLoadReposTask = new LoadReposTask(getDataManager());
        ConcurrentAsyncTask.execute(mLoadReposTask);

        // update action bar
        ActionBar bar = getSupportActionBar();
        bar.setDisplayHomeAsUpEnabled(true);
        bar.setTitle(R.string.choose_a_library);
    }

    private void chooseDir() {
        mStep = STEP_CHOOSE_DIR;
        mEmptyText.setText(R.string.dir_empty);

        // update action bar
        setListAdapter(getDirentsAdapter());
        mOkButton.setVisibility(View.VISIBLE);
        refreshDir();
    }

    private void refreshDir() {
        showLoading(true);

        String repoID = getNavContext().getRepoID();
        String dirPath = getNavContext().getDirPath();

        mLoadDirTask = new LoadDirTask(repoID, dirPath, getDataManager());
        ConcurrentAsyncTask.execute(mLoadDirTask);

        // update action bar
        ActionBar bar = getSupportActionBar();
        bar.setDisplayHomeAsUpEnabled(true);
        bar.setTitle(R.string.choose_a_folder);
    }

    private void addUploadTask(String repoName, String repoID, String targetDir, String localFilePath) {
        bindTransferService(repoName, repoID, targetDir, localFilePath);
    }

    private void bindTransferService(final String repoName, final String repoID,
                                        final String targetDir, final String localPath) {
        // start transfer service
        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);

        mConnection = new ServiceConnection() {
            @Override
            public void onServiceConnected(ComponentName className, IBinder service) {
                TransferBinder binder = (TransferBinder) service;
                mTxService = binder.getService();
                mTxService.addUploadTask(mAccount, repoID, repoName, targetDir,
                                         localPath, false);
                finish();
            }

            @Override
            public void onServiceDisconnected(ComponentName arg0) {
                mTxService = null;
            }
        };
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");
    }

    private void showLoading(boolean loading) {
        clearError();
        if (loading) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_in));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            mListContainer.setVisibility(View.INVISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                        this, android.R.anim.fade_out));
                mListContainer.startAnimation(AnimationUtils.loadAnimation(
                        this, android.R.anim.fade_in));

            mProgressContainer.setVisibility(View.GONE);
            mListContainer.setVisibility(View.VISIBLE);
        }
    }

    private void setErrorMessage(int resID) {
        mContentArea.setVisibility(View.GONE);
        mErrorText.setVisibility(View.VISIBLE);
        mErrorText.setText(getString(resID));
    }

    private void clearError() {
        mErrorText.setVisibility(View.GONE);
        mContentArea.setVisibility(View.VISIBLE);
    }

    private DataManager getDataManager() {
        if (mDataManager == null) {
            mDataManager = new DataManager(mAccount);
        }

        return mDataManager;
    }

    private AccountManager getAccountManager() {
        if (mAccountManager == null) {
            mAccountManager = new AccountManager(this);
        }

        return mAccountManager;
    }

    private NavContext getNavContext() {
        if (mNavContext == null) {
            mNavContext = new NavContext();
        }

        return mNavContext;
    }

    private AccountAdapter getAccountAdapter() {
        if (mAccountAdapter == null) {
            mAccountAdapter = new AccountAdapter(this);
        }

        return mAccountAdapter;
    }

    private ReposAdapter getReposAdapter() {
        if (mReposAdapter == null) {
            mReposAdapter = new ReposAdapter();
        }

        return mReposAdapter;
    }

    private DirentsAdapter getDirentsAdapter() {
        if (mDirentsAdapter == null) {
            mDirentsAdapter = new DirentsAdapter();
        }

        return mDirentsAdapter;
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
                // Only 1 account. Go to the next next step.
                mAccount = accounts.get(0);
                chooseRepo();
                return;
            }

            AccountAdapter adapter = getAccountAdapter();
            adapter.clear();
            for (Account account: accounts) {
                adapter.add(account);
            }
            adapter.notifyDataSetChanged();
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
                repos = dataManager.getRepos();
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            showLoading(false);
            if (err != null || repos == null) {
                setErrorMessage(R.string.load_libraries_fail);
                if (err != null) {
                    Log.d(DEBUG_TAG, "failed to load repos: " + err.getMessage());
                }
                return;
            }

            if (mStep != STEP_CHOOSE_REPO) {
                return;
            }

            if (repos != null) {
                getReposAdapter().setRepos(repos);
            } else {
                Log.d(DEBUG_TAG, "failed to load repos");
            }
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
        protected Void doInBackground(Void... params) {
            try {
                dirents = dataManager.getDirents(repoID, dirPath);
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            showLoading(false);
            if (err != null || dirents == null) {
                setErrorMessage(R.string.load_dir_fail);
                if (err != null) {
                    Log.d(DEBUG_TAG, "failed to load dir: " + err.getMessage());
                }
                return;
            }

            if (mStep != STEP_CHOOSE_DIR) {
                return;
            }

            if (dirents != null) {
                getDirentsAdapter().setDirents(dirents);
            } else {
                Log.d(DEBUG_TAG, "failed to load dir");
            }
        }
    }
}