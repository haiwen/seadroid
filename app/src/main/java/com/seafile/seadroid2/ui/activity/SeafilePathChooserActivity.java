package com.seafile.seadroid2.ui.activity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v7.app.ActionBar;
import android.support.v7.widget.Toolbar;
import android.text.TextUtils;
import android.util.Log;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.animation.AnimationUtils;
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.NavContext;
import com.seafile.seadroid2.ui.adapter.AccountAdapter;
import com.seafile.seadroid2.ui.adapter.DirentsAdapter;
import com.seafile.seadroid2.ui.adapter.SeafAccountAdapter;
import com.seafile.seadroid2.ui.adapter.SeafReposAdapter;
import com.seafile.seadroid2.ui.dialog.PasswordDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.ui.fragment.SettingsFragment;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import java.net.HttpURLConnection;
import java.util.List;

/**
 * Path chooser - Let the user choose a target path (account, repo, dir)
 */
public class SeafilePathChooserActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "SeafilePathChooserActivity";

    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "password_dialog_fragment_tag";

    private NavContext mNavContext;

    private Account mAccount;

    private AccountManager mAccountManager;
    private DataManager mDataManager;

    private AccountAdapter mAccountAdapter;
    private SeafReposAdapter mReposAdapter;
    private DirentsAdapter mDirentsAdapter;

    private LoadDirTask mLoadDirTask;
    private LoadReposTask mLoadReposTask;
    private LoadAccountsTask mLoadAccountsTask;

    private boolean canChooseAccount;
    private boolean onlyShowWritableRepos;
    private String encryptedRepoId;

    private boolean isOnlyChooseRepo;

    private View mProgressContainer, mListContainer, mContentArea;
    private Button mOkButton, mCancelButton;
    private TextView mEmptyText, mErrorText;
    private ListView mListView;

    private static final int STEP_CHOOSE_ACCOUNT = 1;
    private static final int STEP_CHOOSE_REPO = 2;
    private static final int STEP_CHOOSE_DIR = 3;
    private int mStep = 1;

    public static final String DATA_REPO_ID = "repoID";
    public static final String DATA_REPO_NAME = "repoNAME";
    public static final String DATA_DIRECTORY_PATH = "dirPath";
    public static final String DATA_DIR = "dir";
    public static final String DATA_ACCOUNT = "account";

    public static final String ONLY_SHOW_WRITABLE_REPOS = "onlyShowWritableRepos";
    public static final String SHOW_ENCRYPTED_REPOS = "showEncryptedRepos";
    public static final String ENCRYPTED_REPO_ID = "encryptedRepoId";
    private boolean showEncryptedRepos;

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.seafile_path_chooser);
        Intent intent = getIntent();
        Account account = (Account)intent.getParcelableExtra("account");
        if (account == null) {
            canChooseAccount = true;
        } else {
            mAccount = account;
        }
        onlyShowWritableRepos = intent.getBooleanExtra(ONLY_SHOW_WRITABLE_REPOS, true);
        showEncryptedRepos = intent.getBooleanExtra(SHOW_ENCRYPTED_REPOS, true);
        encryptedRepoId = intent.getStringExtra(ENCRYPTED_REPO_ID);

        mOkButton = (Button) findViewById(R.id.ok);
        mCancelButton = (Button) findViewById(R.id.cancel);
        mListView = (ListView) findViewById(android.R.id.list);
        mEmptyText = (TextView) findViewById(android.R.id.empty);
        mErrorText = (TextView) findViewById(R.id.error_message);
        mListContainer = findViewById(R.id.listContainer);
        mProgressContainer = findViewById(R.id.progressContainer);
        mContentArea = findViewById(R.id.content);
        isOnlyChooseRepo = intent.getBooleanExtra(SettingsFragment.CAMERA_UPLOAD_BOTH_PAGES, false);
        if (isOnlyChooseRepo) {
            mOkButton.setVisibility(View.GONE);
        }
        mListView.setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        mListView.setOnItemClickListener(new ListView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> l, View view, int position, long id) {
                onListItemClick(view, position, id);
            }
        });

        mOkButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String repoName = mNavContext.getRepoName();
                String repoID = mNavContext.getRepoID();
                String dir = mNavContext.getDirPath();
                Intent intent = new Intent();
                intent.putExtra(DATA_REPO_NAME, repoName);
                intent.putExtra(DATA_REPO_ID, repoID);
                intent.putExtra(DATA_DIR, dir);
                intent.putExtra(DATA_ACCOUNT, mAccount);
                setResult(RESULT_OK, intent);
                finish();
            }
        });

        mCancelButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                setResult(RESULT_CANCELED);
                finish();
            }
        });

        if (canChooseAccount) {
            chooseAccount();
        } else {
            chooseRepo();
        }

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.app_name);
    }

    @Override
    protected void onStart() {
        super.onStart();
        if (android.os.Build.VERSION.SDK_INT < 14
                && SettingsManager.instance().isGestureLockRequired()) {
            Intent newIntent = new Intent(this, UnlockGesturePasswordActivity.class);
            startActivity(newIntent);
        }
    }

    @SuppressLint("LongLogTag")
    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");

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

    public void onListItemClick(final View v, final int position, final long id) {
        NavContext nav = getNavContext();
        SeafRepo repo = null;

        if (mStep == STEP_CHOOSE_REPO) {
            repo = getReposAdapter().getItem(position);
        } else if (mStep == STEP_CHOOSE_DIR) {
            repo = getDataManager().getCachedRepoByID(nav.getRepoID());
        }

        if (repo != null) {
            if (repo.encrypted && !mDataManager.getRepoPasswordSet(repo.id)) {
                String password = mDataManager.getRepoPassword(repo.id);
                showPasswordDialog(repo.name, repo.id,
                        new TaskDialog.TaskDialogListener() {
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
            chooseRepo();
            break;
        case STEP_CHOOSE_REPO:
            if (!isOnlyChooseRepo) {
                nav.setRepoName(repo.name);
                nav.setRepoID(repo.id);
                nav.setDir("/", repo.root);
                chooseDir();
            } else {
                Intent intent = new Intent();
                intent.putExtra(DATA_REPO_NAME, repo.name);
                intent.putExtra(DATA_REPO_ID, repo.id);
                intent.putExtra(DATA_DIR, repo.root);
                intent.putExtra(DATA_ACCOUNT, mAccount);
                setResult(RESULT_OK, intent);
                finish();
            }
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
        Toolbar toolbar = getActionBarToolbar();
        toolbar.inflateMenu(R.menu.seafile_path_chooser_menu);
        toolbar.setOnMenuItemClickListener(this);
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
        if (item.getItemId() == android.R.id.home) {
            stepBack();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.refresh:
                refreshList(true);
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public void onBackPressed() {
        stepBack(true);
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
                if (repo.encrypted && !mDataManager.getRepoPasswordSet(repo.id)) {
                    String password = mDataManager.getRepoPassword(repo.id);
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

    private void stepBack() {
        stepBack(false);
    }

    private void stepBack(boolean cancelIfFirstStep) {
        switch (mStep) {
        case STEP_CHOOSE_ACCOUNT:
            if (cancelIfFirstStep) {
                finish();
            }
            break;
        case STEP_CHOOSE_REPO:
            if (canChooseAccount) {
                chooseAccount(false);
            } else if (cancelIfFirstStep) {
                finish();
            }
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

    private void setListAdapter(BaseAdapter adapter) {
        mListView.setAdapter(adapter);
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
        chooseRepo(false);
    }

    private void chooseRepo(boolean forceRefresh) {
        mStep = STEP_CHOOSE_REPO;
        mEmptyText.setText(R.string.no_repo);

        setListAdapter(getReposAdapter());
        mOkButton.setVisibility(View.GONE);

        getNavContext().setRepoID(null);

        if (!Utils.isNetworkOn() || !forceRefresh) {
            List<SeafRepo> repos = getDataManager().getReposFromCache();
            if (repos != null) {
                updateAdapterWithRepos(repos);
                // update action bar
                ActionBar bar = getSupportActionBar();
                bar.setDisplayHomeAsUpEnabled(true);
                bar.setTitle(R.string.choose_a_library);
                return;
            }
        }

        showLoading(true);
        mLoadReposTask = new LoadReposTask(getDataManager());
        ConcurrentAsyncTask.execute(mLoadReposTask);

        // update action bar
        ActionBar bar = getSupportActionBar();
        bar.setDisplayHomeAsUpEnabled(true);
        bar.setTitle(R.string.choose_a_library);
    }

    private void chooseDir() {
        chooseDir(false);
    }

    private void chooseDir(boolean forceRefresh) {
        mStep = STEP_CHOOSE_DIR;
        mEmptyText.setText(R.string.dir_empty);

        // update action bar
        setListAdapter(getDirentsAdapter());
        mOkButton.setVisibility(View.VISIBLE);
        refreshDir(forceRefresh);
    }

    private void refreshDir() {
        refreshDir(false);
    }

    private void updateAdapterWithDirents(List<SeafDirent> dirents) {
        DirentsAdapter adapter=getDirentsAdapter();
        if (dirents.size() > 0) {
            adapter.clearDirents();
            for (SeafDirent dirent : dirents) {
                adapter.add(dirent);
            }
            int sort_type = SettingsManager.instance().getSortFilesTypePref();
            int sort_order = SettingsManager.instance().getSortFilesOrderPref();
            adapter.sortFiles(sort_type, sort_order);
            adapter.notifyDataSetChanged();
        }
        showListOrEmptyText(dirents.size());
    }

    private void updateAdapterWithRepos(List<SeafRepo> repos) {
        SeafReposAdapter adapter = getReposAdapter();
        if (repos.size() > 0) {
            adapter.clearRepos();
            for (SeafRepo item : repos) {
                boolean isContains = false;
                for (SeafRepo data : adapter.getData()) {
                    if (TextUtils.equals(data.getID(), item.getID())) {
                        isContains = true;
                        break;
                    }
                }
                if (onlyShowWritableRepos && !item.hasWritePermission()) {
                    // Read only dir need not  show in list
                    continue;
                }

                if (item.encrypted && !showEncryptedRepos) {
                    // encrypted dir need not show in list
                    continue;
                }

                if (item.encrypted && TextUtils.equals(item.id, encryptedRepoId)) {
                    NavContext nav = getNavContext();
                    nav.setRepoName(item.name);
                    nav.setRepoID(item.id);
                    nav.setDir("/", item.root);
                    chooseDir();
                    mStep = STEP_CHOOSE_REPO;
                    break;
                }
                if (!isContains) {
                    adapter.add(item);
                }
            }
            int sort_type = SettingsManager.instance().getSortFilesTypePref();
            int sort_order = SettingsManager.instance().getSortFilesOrderPref();
            adapter.sortFiles(sort_type, sort_order);
            adapter.notifyDataSetChanged();
        }
        showListOrEmptyText(repos.size());
    }

    private void refreshDir(boolean forceRefresh) {
        String repoID = getNavContext().getRepoID();
        String dirPath = getNavContext().getDirPath();

        if (!Utils.isNetworkOn() || !forceRefresh) {
            List<SeafDirent> dirents = getDataManager().getCachedDirents(
                getNavContext().getRepoID(), getNavContext().getDirPath());
            if (dirents != null) {
                updateAdapterWithDirents(dirents);
                // update action bar
                ActionBar bar = getSupportActionBar();
                bar.setDisplayHomeAsUpEnabled(true);
                bar.setTitle(R.string.choose_a_folder);
                return;
            }
        }

        showLoading(true);
        mLoadDirTask = new LoadDirTask(repoID, dirPath, getDataManager());
        ConcurrentAsyncTask.execute(mLoadDirTask);

        // update action bar
        ActionBar bar = getSupportActionBar();
        bar.setDisplayHomeAsUpEnabled(true);
        bar.setTitle(R.string.choose_a_folder);
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
        passwordDialog.show(getSupportFragmentManager(), PASSWORD_DIALOG_FRAGMENT_TAG);
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

    private void showListOrEmptyText(int listSize) {
        if (listSize == 0) {
            mListView.setVisibility(View.GONE);
            mEmptyText.setVisibility(View.VISIBLE);
        } else {
            mListView.setVisibility(View.VISIBLE);
            mEmptyText.setVisibility(View.GONE);
        }
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
            mAccountAdapter = new SeafAccountAdapter(this);
        }

        return mAccountAdapter;
    }

    private SeafReposAdapter getReposAdapter() {
        if (mReposAdapter == null) {
            mReposAdapter = new SeafReposAdapter(onlyShowWritableRepos, encryptedRepoId);
        }

        return mReposAdapter;
    }

    private DirentsAdapter getDirentsAdapter() {
        if (mDirentsAdapter == null) {
            mDirentsAdapter = new DirentsAdapter();
        }

        return mDirentsAdapter;
    }

    private void setAccount(Account account) {
        mAccount = account;
        mDataManager = new DataManager(account);
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
                accounts = accountManager.getSignedInAccountList();
            } catch (Exception e) {
                err = e;
            }

            return null;
        }

        @SuppressLint("LongLogTag")
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
                setAccount(accounts.get(0));
                chooseRepo();
                return;
            }

            AccountAdapter adapter = getAccountAdapter();
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

        @SuppressLint("LongLogTag")
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
        protected Void doInBackground(Void... params) {
            try {
                dirents = dataManager.getDirentsFromServer(repoID, dirPath);
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @SuppressLint("LongLogTag")
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
                    showShortToast(SeafilePathChooserActivity.this, message);
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
}
