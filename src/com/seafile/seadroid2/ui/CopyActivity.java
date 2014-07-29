package com.seafile.seadroid2.ui;

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
import android.widget.AdapterView;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.AccountAdapter;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.ui.DirentsAdapter;
import com.seafile.seadroid2.ui.PasswordDialog;
import com.seafile.seadroid2.ui.ReposAdapter;
import com.seafile.seadroid2.ui.TaskDialog;

/*class CopyAndMoveTask extends TaskDialog.Task {
    String repoID;
    String filenames;
    String dst_repoID;
    String dst_dir;
    String path;
    boolean isdir;
    boolean isCopy;
    DataManager dataManager;

    public CopyAndMoveTask(String repoID, String filenames, String dst_repoID, String dst_dir, String path,
            boolean isdir, boolean isCopy, DataManager dataManager) {
        this.repoID = repoID;
        this.filenames = filenames;
        this.dst_repoID = dst_repoID;
        this.dst_dir = dst_dir;
        this.path = path;
        this.isdir = isdir;
        this.isCopy = isCopy;
        this.dataManager = dataManager;
    }
    
    @Override
    protected void runTask() {
    // TODO Auto-generated method stub
        try {
            if (isCopy) {
                //dataManager.copy
            }
            else dataManager.move(repoID, filenames, dst_repoID, dst_dir, path, isdir);;
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}*/

public class CopyActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "CopyActivity";

    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "password_dialog_fragment_tag";

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
    
    private String repoID;
    private String path;
    private String filenames;
    private boolean isdir;
    private String repoName;
    private boolean isCopy;

    /*public void init(String repoID, String repoName, String path, String filenames, Account mAccount,
    		         boolean isdir, boolean isCopy){
    	this.repoID = repoID;
    	this.repoName = repoName;
        this.path = path;
        this.filenames = filenames;
        this.mAccount = mAccount;
        this.isdir = isdir;
        this.isCopy = isCopy;
    }*/
    
    protected void onCreate(Bundle savedInstanceState) {
    	// TODO Auto-generated method stub
        super.onCreate(savedInstanceState);
        setContentView(R.layout.share_to_seafile);

        /*Intent intent = getIntent();

        Uri uri = (Uri)intent.getExtras().get(Intent.EXTRA_STREAM);
        final String localPath = getSharedFilePath(uri);

        if (localPath == null) {
            findViewById(R.id.main).setVisibility(View.GONE);
            return;
        }

        findViewById(R.id.not_supported_text).setVisibility(View.GONE);*/

        //Log.d(DEBUG_TAG, "copy " + path);

        Intent intent = getIntent();
        this.repoID = intent.getStringExtra("repoID");
        this.repoName = intent.getStringExtra("repoName");
        this.path = intent.getStringExtra("path");
        this.filenames = intent.getStringExtra("filenames");
        this.mAccount = (Account)intent.getParcelableExtra("mAccount");
        this.isdir = intent.getExtras().getBoolean("isdir");
        this.isCopy = intent.getExtras().getBoolean("isCopy");
        
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
                String dst_repoName = mNavContext.getRepoName();
                String dst_repoID = mNavContext.getRepoID();
                String dst_dir = mNavContext.getDirPath();
                doTask(dst_repoName, dst_repoID, dst_dir, isCopy);
            }
        });

        mCancelButton.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                finish();
            }
        });

        chooseAccount();
    }

    /*private String getSharedFilePath(Uri uri) {
    	// TODO Auto-generated method stub
        if (uri == null) {
            return null;
        }

        if (uri.getScheme().equals("file")) {
            return uri.getPath();
        } else {
            ContentResolver contentResolver = getContentResolver();
            Cursor cursor = contentResolver.query(uri, null, null, null, null);
            if (!cursor.moveToFirst()) {
                return null;
            }
            String filePath = cursor.getString(cursor.getColumnIndex(Images.Media.DATA));
            return filePath;
        }
    }*/

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

    public void onListItemClick(final View v, final int position, final long id) {
        NavContext nav = getNavContext();
        SeafRepo repo = null;

        if (mStep == STEP_CHOOSE_REPO) {
            repo = getReposAdapter().getItem(position);
        } else if (mStep == STEP_CHOOSE_DIR) {
            repo = getDataManager().getCachedRepoByID(nav.getRepoID());
        }

        if (repo != null) {
            if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
                String password = DataManager.getRepoPassword(repo.id);
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
                if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
                    String password = DataManager.getRepoPassword(repo.id);
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
        mEmptyText.setText(R.string.no_library);

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
        getDirentsAdapter().setDirents(dirents);
        showListOrEmptyText(dirents.size());
    }

    private void updateAdapterWithRepos(List<SeafRepo> repos) {
        getReposAdapter().setRepos(repos);
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

        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.setRepo(repoName, repoID, mAccount);
        passwordDialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                refreshDir();
            }
        });
        passwordDialog.show(getSupportFragmentManager(), PASSWORD_DIALOG_FRAGMENT_TAG);
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

    private void doTask(String dst_repoName, String dst_repoID, String dst_dir, boolean isCopy) {
    	// TODO Auto-generated method stub
        CopyAndMoveTask task = new CopyAndMoveTask();
        task.execute(dst_repoName, dst_repoID, dst_dir);
        return;
    }
    
    /*private void doCopyTask(String dst_repoName, String dst_repoID, String dst_dir){
    	
    }
    
    private void doMoveTask(String dst_repoName, String dst_repoID, String dst_dir){
    	try{
    		mDataManager.move(repoID, filenames, dst_repoID, dst_dir, path, isdir);
    	} catch (SeafException e) {
    	 // TODO Auto-generated method stub
        }	
    }*/

    /*private void bindTransferService(final String repoName, final String repoID,
                                        final String targetDir, final String localPath) {
    	// TODO Auto-generated method stub
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
    }*/

    public void showToast(CharSequence msg) {
        Context context = getApplicationContext();
        Toast toast = Toast.makeText(context, msg, Toast.LENGTH_SHORT);
        toast.show();
    }

    public void showToast(int id) {
        showToast(getString(id));
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

        @Override
        protected void onPostExecute(Void v) {
            if (mStep != STEP_CHOOSE_REPO) {
                return;
            }

            showLoading(false);
            if (err != null || repos == null) {
                setErrorMessage(R.string.load_libraries_fail);
                if (err != null) {
                    Log.d(DEBUG_TAG, "failed to load repos: " + err.getMessage());
                }
                return;
            }

            if (repos != null) {
                updateAdapterWithRepos(repos);
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
                if (retCode == 440) {
                    showPasswordDialog();
                } else if (retCode == 404) {
                    showToast(String.format("The folder \"%s\" was deleted", dirPath));
                } else {
                    Log.d(DEBUG_TAG, "failed to load dirents: " + err.getMessage());
                    err.printStackTrace();
                    setErrorMessage(R.string.load_dir_fail);
                }
                return;
            }

            if (dirents == null) {
                Log.d(DEBUG_TAG, "failed to load dirents: " + err.getMessage());
                setErrorMessage(R.string.load_dir_fail);
                return;
            }

            if (dirents != null) {
                updateAdapterWithDirents(dirents);
            } else {
                Log.d(DEBUG_TAG, "failed to load dir");
            }
        }
    }
    
    private class CopyAndMoveTask extends AsyncTask<String, Long , Void>{
        
        protected Void doInBackground (String ... params) {
            
            String dst_repoID = params[1];
            String dst_dir = params[2];
            
            try {
                if (isCopy) {
                    //dataManager.copy
                }
                else getDataManager().move(repoID, filenames, dst_repoID, dst_dir, path, isdir);
            } catch (SeafException e) {
                //setTaskException(e);
            }
            return null;
        }
        
    }
    
}
