package com.seafile.seadroid2.ui.activity;

import android.Manifest;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.provider.MediaStore;
import android.support.design.widget.Snackbar;
import android.support.design.widget.TabLayout;
import android.support.v4.app.ActivityCompat;
import android.support.v4.app.DialogFragment;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentPagerAdapter;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.content.ContextCompat;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v4.view.ViewPager;
import android.support.v7.app.AlertDialog;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;
import android.widget.FrameLayout;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.cameraupload.MediaObserverService;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SeafStarredFile;
import com.seafile.seadroid2.data.ServerInfo;
import com.seafile.seadroid2.data.StorageManager;
import com.seafile.seadroid2.fileschooser.MultiFileChooserActivity;
import com.seafile.seadroid2.monitor.FileMonitorService;
import com.seafile.seadroid2.notification.DownloadNotificationProvider;
import com.seafile.seadroid2.notification.UploadNotificationProvider;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.DownloadTaskManager;
import com.seafile.seadroid2.transfer.PendingUploadInfo;
import com.seafile.seadroid2.transfer.TransferManager;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.transfer.UploadTaskManager;
import com.seafile.seadroid2.ui.CopyMoveContext;
import com.seafile.seadroid2.ui.NavContext;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.adapter.SeafItemAdapter;
import com.seafile.seadroid2.ui.dialog.AppChoiceDialog;
import com.seafile.seadroid2.ui.dialog.AppChoiceDialog.CustomAction;
import com.seafile.seadroid2.ui.dialog.CopyMoveDialog;
import com.seafile.seadroid2.ui.dialog.DeleteFileDialog;
import com.seafile.seadroid2.ui.dialog.DeleteRepoDialog;
import com.seafile.seadroid2.ui.dialog.FetchFileDialog;
import com.seafile.seadroid2.ui.dialog.NewDirDialog;
import com.seafile.seadroid2.ui.dialog.NewFileDialog;
import com.seafile.seadroid2.ui.dialog.NewRepoDialog;
import com.seafile.seadroid2.ui.dialog.PasswordDialog;
import com.seafile.seadroid2.ui.dialog.RenameFileDialog;
import com.seafile.seadroid2.ui.dialog.RenameRepoDialog;
import com.seafile.seadroid2.ui.dialog.SortFilesDialogFragment;
import com.seafile.seadroid2.ui.dialog.SslConfirmDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.ui.dialog.UploadChoiceDialog;
import com.seafile.seadroid2.ui.fragment.ActivitiesFragment;
import com.seafile.seadroid2.ui.fragment.ReposFragment;
import com.seafile.seadroid2.ui.fragment.StarredFragment;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.util.UtilsJellyBean;
import com.viewpagerindicator.IconPagerAdapter;

import org.apache.commons.io.IOUtils;
import org.json.JSONException;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class BrowserActivity extends BaseActivity
        implements ReposFragment.OnFileSelectedListener, StarredFragment.OnStarredFileSelectedListener,
        FragmentManager.OnBackStackChangedListener, Toolbar.OnMenuItemClickListener,
        SortFilesDialogFragment.SortItemClickListener {
    private static final String DEBUG_TAG = "BrowserActivity";
    public static final String ACTIONBAR_PARENT_PATH = "/";

    public static final String OPEN_FILE_DIALOG_FRAGMENT_TAG = "openfile_fragment";
    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "password_fragment";
    public static final String CHOOSE_APP_DIALOG_FRAGMENT_TAG = "choose_app_fragment";
    public static final String PICK_FILE_DIALOG_FRAGMENT_TAG = "pick_file_fragment";
    public static final int REQUEST_PERMISSIONS_WRITE_EXTERNAL_STORAGE = 1;

    public static final String TAG_NEW_REPO_DIALOG_FRAGMENT = "NewRepoDialogFragment";
    public static final String TAG_DELETE_REPO_DIALOG_FRAGMENT = "DeleteRepoDialogFragment";
    public static final String TAG_DELETE_FILE_DIALOG_FRAGMENT = "DeleteFileDialogFragment";
    public static final String TAG_DELETE_FILES_DIALOG_FRAGMENT = "DeleteFilesDialogFragment";
    public static final String TAG_RENAME_REPO_DIALOG_FRAGMENT = "RenameRepoDialogFragment";
    public static final String TAG_RENAME_FILE_DIALOG_FRAGMENT = "RenameFileDialogFragment";
    public static final String TAG_COPY_MOVE_DIALOG_FRAGMENT = "CopyMoveDialogFragment";
    public static final String TAG_SORT_FILES_DIALOG_FRAGMENT = "SortFilesDialogFragment";

    public static final int INDEX_LIBRARY_TAB = 0;
    public static final int INDEX_STARRED_TAB = 1;
    public static final int INDEX_ACTIVITIES_TAB = 2;

    private static final int[] ICONS = new int[] {
            R.drawable.tab_library, R.drawable.tab_starred,
            R.drawable.tab_activity
    };

    private FetchFileDialog fetchFileDialog = null;
    private SeafileTabsAdapter adapter;
    private View mLayout;
    private FrameLayout container;
    private TabLayout mTabLayout;
    private ViewPager pager;
    private NavContext navContext = new NavContext();
    private CopyMoveContext copyMoveContext;
    private Menu overFlowMenu;
    private MenuItem menuSearch;

    private DataManager dataManager = null;
    private TransferService txService = null;
    private TransferReceiver mTransferReceiver;
    private AccountManager accountManager;
    private int currentPosition = 0;
    private Intent copyMoveIntent;
    private Account account;

    public DataManager getDataManager() {
        return dataManager;
    }

    public void addUpdateTask(String repoID, String repoName, String targetDir, String localFilePath) {
        if (txService != null) {
            txService.addTaskToUploadQue(account, repoID, repoName, targetDir, localFilePath, true, true);
        } else {
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, true, true);
            pendingUploads.add(info);
        }
    }

    public void addUpdateBlocksTask(String repoID, String repoName, String targetDir, String localFilePath, int version) {
        if (txService != null) {
            txService.addTaskToUploadQue(account, repoID, repoName, targetDir, localFilePath, true, true, version);
        } else {
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, true, true, version);
            pendingUploads.add(info);
        }
    }

    private int addUploadTask(String repoID, String repoName, String targetDir, String localFilePath) {
        if (txService != null) {
            return txService.addTaskToUploadQue(account, repoID, repoName, targetDir, localFilePath, false, true);
        } else {
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false, true);
            pendingUploads.add(info);
            return 0;
        }
    }

    private int addUploadBlocksTask(String repoID, String repoName, String targetDir, String localFilePath, int version) {
        if (txService != null) {
            return txService.addTaskToUploadQue(account, repoID, repoName, targetDir, localFilePath, false, true, version);
        } else {
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false, true, version);
            pendingUploads.add(info);
            return 0;
        }
    }

    private ArrayList<PendingUploadInfo> pendingUploads = Lists.newArrayList();

    public TransferService getTransferService() {
        return txService;
    }

    public Account getAccount() {
        return account;
    }

    public NavContext getNavContext() {
        return navContext;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        supportRequestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        super.onCreate(savedInstanceState);

        accountManager = new AccountManager(this);

        // restart service should it have been stopped for some reason
        Intent mediaObserver = new Intent(this, MediaObserverService.class);
        startService(mediaObserver);

        if (!isTaskRoot()) {
            final Intent intent = getIntent();
            final String intentAction = getIntent().getAction();
            if (intent.hasCategory(Intent.CATEGORY_LAUNCHER)
                    && intentAction != null
                    && intentAction.equals(Intent.ACTION_MAIN)) {
                finish();
                return;
            }
        }
        setContentView(R.layout.tabs_main);
        mLayout = findViewById(R.id.main_layout);
        container = (FrameLayout) findViewById(R.id.bottom_sheet_container);
        setSupportActionBar(getActionBarToolbar());
        // enable ActionBar app icon to behave as action back
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        findViewById(R.id.view_toolbar_bottom_line).setVisibility(View.GONE);
        // Get the message from the intent
        Intent intent = getIntent();

        account = accountManager.getCurrentAccount();
        if (account == null || !account.hasValidToken()) {
            finishAndStartAccountsActivity();
            return;
        }

        // Log.d(DEBUG_TAG, "browser activity onCreate " + account.server + " " + account.email);
        dataManager = new DataManager(account);

        getSupportFragmentManager().addOnBackStackChangedListener(this);

        unsetRefreshing();
        disableUpButton();

        pager = (ViewPager) findViewById(R.id.pager);
        adapter = new SeafileTabsAdapter(getSupportFragmentManager());
        pager.setAdapter(adapter);
        mTabLayout = (TabLayout) findViewById(R.id.sliding_tabs);
        mTabLayout.setupWithViewPager(pager);
        mTabLayout.setOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                currentPosition = tab.getPosition();
                supportInvalidateOptionsMenu();
                pager.setCurrentItem(tab.getPosition(), true);
                if (currentPosition != INDEX_LIBRARY_TAB) {
                    disableUpButton();
                } else if (navContext.inRepo()) {
                    enableUpButton();
                }

                setUpButtonTitleOnSlideTabs(tab.getPosition());
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });

        pager.addOnPageChangeListener(new ViewPager.OnPageChangeListener() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {

            }

            @Override
            public void onPageSelected(int position) {
                currentPosition = position;
                supportInvalidateOptionsMenu();
                pager.setCurrentItem(position, true);
                if (currentPosition != INDEX_LIBRARY_TAB) {
                    disableUpButton();
                } else if (navContext.inRepo()) {
                    enableUpButton();
                }

                setUpButtonTitleOnSlideTabs(position);
            }

            @Override
            public void onPageScrollStateChanged(int state) {

            }
        });

        pager.setOffscreenPageLimit(3);

        if (savedInstanceState != null) {
            Log.d(DEBUG_TAG, "savedInstanceState is not null");
            fetchFileDialog = (FetchFileDialog)
                    getSupportFragmentManager().findFragmentByTag(OPEN_FILE_DIALOG_FRAGMENT_TAG);

            AppChoiceDialog appChoiceDialog = (AppChoiceDialog)
                    getSupportFragmentManager().findFragmentByTag(CHOOSE_APP_DIALOG_FRAGMENT_TAG);

            if (appChoiceDialog != null) {
                FragmentTransaction ft = getSupportFragmentManager().beginTransaction();
                ft.detach(appChoiceDialog);
                ft.commit();
            }

            SslConfirmDialog sslConfirmDlg = (SslConfirmDialog)
                    getSupportFragmentManager().findFragmentByTag(SslConfirmDialog.FRAGMENT_TAG);

            if (sslConfirmDlg != null) {
                Log.d(DEBUG_TAG, "sslConfirmDlg is not null");
                FragmentTransaction ft = getSupportFragmentManager().beginTransaction();
                ft.detach(sslConfirmDlg);
                ft.commit();
            } else {
                Log.d(DEBUG_TAG, "sslConfirmDlg is null");
            }

            String repoID = savedInstanceState.getString("repoID");
            String repoName = savedInstanceState.getString("repoName");
            String path = savedInstanceState.getString("path");
            String dirID = savedInstanceState.getString("dirID");
            String permission = savedInstanceState.getString("permission");
            if (repoID != null) {
                navContext.setRepoID(repoID);
                navContext.setRepoName(repoName);
                navContext.setDir(path, dirID);
                navContext.setDirPermission(permission);
            }
        }

        String repoID = intent.getStringExtra("repoID");
        String repoName = intent.getStringExtra("repoName");
        String path = intent.getStringExtra("path");
        String dirID = intent.getStringExtra("dirID");
        String permission = intent.getStringExtra("permission");
        if (repoID != null) {
            navContext.setRepoID(repoID);
            navContext.setRepoName(repoName);
            navContext.setDir(path, dirID);
            navContext.setDirPermission(permission);
        }

        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");

        Intent monitorIntent = new Intent(this, FileMonitorService.class);
        startService(monitorIntent);

        requestServerInfo();

        requestReadExternalStoragePermission();

    }

    public FrameLayout getContainer() {
        return container;
    }

    private void finishAndStartAccountsActivity() {
        Intent newIntent = new Intent(this, AccountsActivity.class);
        newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        finish();
        startActivity(newIntent);
    }

    private void requestServerInfo() {
        if(!checkServerProEdition()) {
            // hide Activity tab
            adapter.hideActivityTab();
            adapter.notifyDataSetChanged();
            mTabLayout.setupWithViewPager(pager);
        }

        if (!checkSearchEnabled()) {
            // hide search menu
            if (menuSearch != null)
                menuSearch.setVisible(false);
        }

        if (!Utils.isNetworkOn())
            return;

        ConcurrentAsyncTask.execute(new RequestServerInfoTask());
    }

    public void completeRemoteWipe() {
        ConcurrentAsyncTask.execute(new AsyncTask<Void, Void, Void>() {
            @Override
            protected Void doInBackground(Void... objects) {
                // clear local caches
                StorageManager storageManager = StorageManager.getInstance();
                storageManager.clearCache();

                // clear cached data from database
                DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
                dbHelper.delCaches();

                try {
                    // response to server when finished cleaning caches
                    getDataManager().completeRemoteWipe();
                } catch (SeafException e) {
                    e.printStackTrace();
                }
                return null;
            }

            @Override
            protected void onPostExecute(Void o) {
                // sign out current account
                logoutWhenTokenExpired();

            }
        });
    }

    /**
     * Token expired, clear current authorized info and redirect user to login page
     */
    public void logoutWhenTokenExpired() {
        AccountManager accountMgr = new AccountManager(this);

        // sign out current account
        Account account = accountMgr.getCurrentAccount();
        accountMgr.signOutAccount(account);

        // then redirect to AccountsActivity
        Intent intent = new Intent(this, AccountsActivity.class);
        startActivity(intent);

        // finish current Activity
        finish();
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                if (navContext.inRepo() && currentPosition == INDEX_LIBRARY_TAB) {
                    onBackPressed();
                }
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        switch (item.getItemId()) {
            case R.id.sort:
                showSortFilesDialog();
                return true;
            case R.id.search:
                Intent searchIntent = new Intent(this, SearchActivity.class);
                startActivity(searchIntent);
                return true;
            case R.id.create_repo:
                showNewRepoDialog();
                return true;
            case R.id.add:
                addFile();
                return true;
            case R.id.transfer_tasks:
                Intent newIntent = new Intent(this, TransferActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(newIntent);
                return true;
            case R.id.accounts:
                newIntent = new Intent(this, AccountsActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(newIntent);
                return true;
            case R.id.edit:
                // start action mode for selecting multiple files/folders

                if (!Utils.isNetworkOn()) {
                    showShortToast(this, R.string.network_down);
                    return true;
                }
                if (currentPosition == INDEX_LIBRARY_TAB) {
                    if (navContext.inRepo()) {
                        SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                        if (repo.encrypted && !dataManager.getRepoPasswordSet(repo.id)) {
                            String password = dataManager.getRepoPassword(repo.id);
                            showPasswordDialog(repo.name, repo.id,
                                    new TaskDialog.TaskDialogListener() {
                                        @Override
                                        public void onTaskSuccess() {
                                            getReposFragment().startContextualActionMode();
                                        }
                                    } , password);

                            return true;
                        }
                    }

                    getReposFragment().startContextualActionMode();
                }

                return true;
            case R.id.settings:
                Intent settingsIntent = new Intent(BrowserActivity.this,SettingsActivity.class);
                settingsIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(settingsIntent);
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

    /**
     * If the user is running Android 6.0 (API level 23) or later, the user has to grant your app its permissions while they are running the app
     *
     * Requests the WRITE_EXTERNAL_STORAGE permission.
     * If the permission has been denied previously, a SnackBar will prompt the user to grant the
     * permission, otherwise it is requested directly.
     *
     */
    private void requestReadExternalStoragePermission() {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.WRITE_EXTERNAL_STORAGE) != PackageManager.PERMISSION_GRANTED) {
            // Should we show an explanation?
            if (ActivityCompat.shouldShowRequestPermissionRationale(this,
                    Manifest.permission.WRITE_EXTERNAL_STORAGE)) {

                Snackbar.make(mLayout,
                        R.string.permission_read_exteral_storage_rationale,
                        Snackbar.LENGTH_INDEFINITE)
                        .setAction(R.string.settings, new View.OnClickListener() {
                            @Override
                            public void onClick(View view) {
                                ActivityCompat.requestPermissions(BrowserActivity.this,
                                        new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE},
                                        REQUEST_PERMISSIONS_WRITE_EXTERNAL_STORAGE);
                            }
                        })
                        .show();

            } else {

                // No explanation needed, we can request the permission.

                // WRITE_EXTERNAL_STORAGE permission has not been granted yet. Request it directly.
                ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.WRITE_EXTERNAL_STORAGE},
                        REQUEST_PERMISSIONS_WRITE_EXTERNAL_STORAGE);
            }
        }
    }

    /**
     * Callback received when a permissions request has been completed.
     */
    @Override
    public void onRequestPermissionsResult(int requestCode, String permissions[], int[] grantResults) {
        // Log.i(DEBUG_TAG, "Received response for permission request.");
        switch (requestCode) {
            case REQUEST_PERMISSIONS_WRITE_EXTERNAL_STORAGE: {
                // Check if the only required permission has been granted
                // If request is cancelled, the result arrays are empty.
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    // permission was granted
                } else {
                    // permission denied
                }
            }
        }
    }

    class RequestServerInfoTask extends AsyncTask<Void, Void, ServerInfo> {
        private SeafException err;

        @Override
        protected ServerInfo doInBackground(Void... params) {
            try {
                return dataManager.getServerInfo();
            } catch (SeafException e) {
                err = e;
            } catch (JSONException e) {
                Log.e(DEBUG_TAG, "JSONException " + e.getMessage());
            }
            return null;
        }

        @Override
        protected void onPostExecute(ServerInfo serverInfo) {
            // Check to see whether this activity is in the process of finishing
            // to avoid IllegalStateException when AsyncTasks continue to run after the activity has been destroyed
            // http://stackoverflow.com/a/35729068/3962551
            if (isFinishing()) return;

            if (serverInfo == null) {
                if (err != null)
                    showShortToast(BrowserActivity.this, err.getMessage());
                return;
            }

            if (serverInfo.isProEdition()) {
                // show Activity tab
                adapter.unHideActivityTab();
                adapter.notifyDataSetChanged();
                mTabLayout.setupWithViewPager(pager);
            }

            if (serverInfo.isSearchEnabled()) {
                // show search menu
                if (menuSearch != null)
                    menuSearch.setVisible(true);
            }

            accountManager.setServerInfo(account, serverInfo);
        }
    }

    /**
     * check if server is pro edition
     *
     * @return
     *          true, if server is pro edition
     *          false, otherwise.
     */
    private boolean checkServerProEdition() {
        if (account == null)
            return false;

        ServerInfo serverInfo = accountManager.getServerInfo(account);

        return serverInfo.isProEdition();
    }

    /**
     * check if server supports searching feature
     *
     * @return
     *          true, if search enabled
     *          false, otherwise.
     */
    private boolean checkSearchEnabled() {
        if (account == null)
            return false;

        ServerInfo serverInfo = accountManager.getServerInfo(account);

        return serverInfo.isSearchEnabled();
    }

    class SeafileTabsAdapter extends FragmentPagerAdapter implements
            IconPagerAdapter {
        public SeafileTabsAdapter(FragmentManager fm) {
            super(fm);
        }

        private ReposFragment reposFragment = null;
        private ActivitiesFragment activitieFragment = null;
        private StarredFragment starredFragment = null;
        private boolean isHideActivityTab;

        public void hideActivityTab() {
            this.isHideActivityTab = true;
        }

        public void unHideActivityTab() {
            this.isHideActivityTab = false;
        }

        @Override
        public Fragment getItem(int position) {
            switch (position) {
                case 0:

                    if (reposFragment == null) {
                        reposFragment = new ReposFragment();
                    }
                    return reposFragment;
                case 1:
                    if (starredFragment == null) {
                        starredFragment = new StarredFragment();
                    }
                    return starredFragment;
                case 2:
                    if (activitieFragment == null) {
                        activitieFragment = new ActivitiesFragment();
                    }
                    return activitieFragment;
                default:
                    return new Fragment();
            }
        }

        @Override
        public CharSequence getPageTitle(int position) {
            switch (position) {
                case 0:
                    return getString(R.string.tabs_library).toUpperCase();
                case 1:
                    return getString(R.string.tabs_starred).toUpperCase();
                case 2:
                    return getString(R.string.tabs_activity).toUpperCase();

                default:
                    return null;
            }
        }

        @Override
        public int getIconResId(int index) {
            return ICONS[index];
        }

        @Override
        public int getCount() {
            if (!isHideActivityTab)
                return ICONS.length;
            else
                return 2;
        }
    }

    public int getCurrentPosition() {
        return currentPosition;
    }

    public void setCurrentPosition(int currentPosition) {
        this.currentPosition = currentPosition;
        pager.setCurrentItem(currentPosition);
        mTabLayout.setScrollPosition(currentPosition, 0, true);
        setUpButtonTitleOnSlideTabs(currentPosition);
        refreshViewOnSlideTabs(currentPosition);
    }

    public Fragment getFragment(int index) {
        return getSupportFragmentManager().findFragmentByTag(makeFragmentName(index));
    }

    private String makeFragmentName(int index) {
        return "android:switcher:" + R.id.pager + ":" + index;
    }

    public ReposFragment getReposFragment() {
        return (ReposFragment) getFragment(0);
    }

    public StarredFragment getStarredFragment() {
        return (StarredFragment)getFragment(1);
    }

    public ActivitiesFragment getActivitiesFragment() {
        return (ActivitiesFragment)getFragment(2);
    }

    ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferBinder binder = (TransferBinder) service;
            txService = binder.getService();
            Log.d(DEBUG_TAG, "bind TransferService");

            for (PendingUploadInfo info : pendingUploads) {
                if (info.enckVersion != -1) {
                    txService.addTaskToUploadQue(account,
                            info.repoID,
                            info.repoName,
                            info.targetDir,
                            info.localFilePath,
                            info.isUpdate,
                            info.isCopyToLocal,
                            info.enckVersion);
                } else {
                    txService.addTaskToUploadQue(account,
                            info.repoID,
                            info.repoName,
                            info.targetDir,
                            info.localFilePath,
                            info.isUpdate,
                            info.isCopyToLocal);
                }
            }
            pendingUploads.clear();
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };

    @Override
    public void onStart() {
        Log.d(DEBUG_TAG, "onStart");
        super.onStart();
        if (android.os.Build.VERSION.SDK_INT < 14
                && SettingsManager.instance().isGestureLockRequired()) {
            Intent intent = new Intent(this, UnlockGesturePasswordActivity.class);
            startActivity(intent);
        }

        if (mTransferReceiver == null) {
            mTransferReceiver = new TransferReceiver();
        }

        IntentFilter filter = new IntentFilter(TransferManager.BROADCAST_ACTION);
        LocalBroadcastManager.getInstance(this).registerReceiver(mTransferReceiver, filter);
    }

    @Override
    protected void onPause() {
        Log.d(DEBUG_TAG, "onPause");
        super.onPause();
    }

    @Override
    public void onRestart() {
        Log.d(DEBUG_TAG, "onRestart");
        super.onRestart();

        if (accountManager.getCurrentAccount() == null
                || !accountManager.getCurrentAccount().equals(this.account)
                || !accountManager.getCurrentAccount().getToken().equals(this.account.getToken())) {
            finishAndStartAccountsActivity();
        }
    }

    @Override
    protected void onNewIntent(Intent intent) {
        Log.d(DEBUG_TAG, "onNewIntent");

        // if the user started the Seadroid app from the Launcher, keep the old Activity
        final String intentAction = intent.getAction();
        if (intent.hasCategory(Intent.CATEGORY_LAUNCHER)
                && intentAction != null
                && intentAction.equals(Intent.ACTION_MAIN)) {
            return;
        }

        Account selectedAccount = accountManager.getCurrentAccount();
        Log.d(DEBUG_TAG,"Current account: "+selectedAccount);
        if (selectedAccount == null
                || !account.equals(selectedAccount)
                || !account.getToken().equals(selectedAccount.getToken())) {
            Log.d(DEBUG_TAG,"Account switched, restarting activity.");
            finish();
            startActivity(intent);
        }
    }

    @Override
    protected void onStop() {
        Log.d(DEBUG_TAG, "onStop");
        super.onStop();

        if (mTransferReceiver != null) {
            LocalBroadcastManager.getInstance(this).unregisterReceiver(mTransferReceiver);
        }
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }
        super.onDestroy();
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        Log.d(DEBUG_TAG, "onSaveInstanceState");
        super.onSaveInstanceState(outState);
        //outState.putInt("tab", getActionBarToolbar().getSelectedNavigationIndex());
        if (navContext.getRepoID() != null) {
            outState.putString("repoID", navContext.getRepoID());
            outState.putString("repoName", navContext.getRepoName());
            outState.putString("path", navContext.getDirPath());
            outState.putString("dirID", navContext.getDirID());
            outState.putString("permission", navContext.getDirPermission());
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        overFlowMenu = menu;
        Toolbar toolbar = getActionBarToolbar();
        toolbar.inflateMenu(R.menu.browser_menu);
        toolbar.setOnMenuItemClickListener(this);
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        menuSearch = menu.findItem(R.id.search);
        MenuItem menuSort = menu.findItem(R.id.sort);
        MenuItem menuAdd = menu.findItem(R.id.add);
        MenuItem menuCreateRepo = menu.findItem(R.id.create_repo);
        MenuItem menuEdit = menu.findItem(R.id.edit);

        // Libraries Tab
        if (currentPosition == 0) {
            if (navContext.inRepo()) {
                menuCreateRepo.setVisible(false);
                menuAdd.setVisible(true);
                menuEdit.setVisible(true);
                if (hasRepoWritePermission()) {
                    menuAdd.setEnabled(true);
                    menuEdit.setEnabled(true);
                } else {
                    menuAdd.setEnabled(false);
                    menuEdit.setEnabled(false);
                }

            } else {
                menuCreateRepo.setVisible(true);
                menuAdd.setVisible(false);
                menuEdit.setVisible(false);
            }

            menuSort.setVisible(true);
        } else {
            menuSort.setVisible(false);
            menuCreateRepo.setVisible(false);
            menuAdd.setVisible(false);
            menuEdit.setVisible(false);
        }

        // Global menus, e.g. Accounts, TransferTasks, Settings, are visible by default.
        // So nothing need to be done here.

        // Though search menu is also a global menu, its state was maintained dynamically at runtime.
        if (!checkServerProEdition())
            menuSearch.setVisible(false);

        return true;
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);
    }

    @Override
    protected void onPostResume() {
        super.onPostResume();
        // We can't show the CopyMoveDialog in onActivityResult, this is a
        // workaround found in
        // http://stackoverflow.com/questions/16265733/failure-delivering-result-onactivityforresult/18345899#18345899
        if (copyMoveIntent != null) {
            String dstRepoId, dstDir;
            dstRepoId = copyMoveIntent.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
            dstDir = copyMoveIntent.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
            copyMoveContext.setDest(dstRepoId, dstDir);
            doCopyMove();
            copyMoveIntent = null;
        }
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
    }

    private void showSortFilesDialog() {
        SortFilesDialogFragment dialog = new SortFilesDialogFragment();
        dialog.show(getSupportFragmentManager(), TAG_SORT_FILES_DIALOG_FRAGMENT);
    }

    @Override
    public void onSortFileItemClick(DialogFragment dialog, int position) {
        switch (position) {
            case 0: // sort by name, ascending
                sortFiles(SeafItemAdapter.SORT_BY_NAME, SeafItemAdapter.SORT_ORDER_ASCENDING);
                break;
            case 1: // sort by name, descending
                sortFiles(SeafItemAdapter.SORT_BY_NAME, SeafItemAdapter.SORT_ORDER_DESCENDING);
                break;
            case 2: // sort by last modified time, ascending
                sortFiles(SeafItemAdapter.SORT_BY_LAST_MODIFIED_TIME, SeafItemAdapter.SORT_ORDER_ASCENDING);
                break;
            case 3: // sort by last modified time, descending
                sortFiles(SeafItemAdapter.SORT_BY_LAST_MODIFIED_TIME, SeafItemAdapter.SORT_ORDER_DESCENDING);
                break;
            default:
                return;
        }
    }

    /**
     * Sort files by type and order
     *
     * @param type
     */
    private void sortFiles(final int type, final int order) {
        if (currentPosition == INDEX_LIBRARY_TAB) {
            if (navContext.inRepo()) {
                SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                if (repo.encrypted && !dataManager.getRepoPasswordSet(repo.id)) {
                    String password = dataManager.getRepoPassword(repo.id);
                    showPasswordDialog(repo.name, repo.id,
                            new TaskDialog.TaskDialogListener() {
                                @Override
                                public void onTaskSuccess() {
                                    getReposFragment().sortFiles(type, order);
                                }
                            }, password);
                }
            }
            getReposFragment().sortFiles(type, order);
        }
    }

    /**
     * create a new repo
     */
    private void showNewRepoDialog() {
        final NewRepoDialog dialog = new NewRepoDialog();
        dialog.init(account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess(){
                showShortToast(
                        BrowserActivity.this,
                        String.format(getResources().getString(R.string.create_new_repo_success), dialog.getRepoName())
                );
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == INDEX_LIBRARY_TAB && reposFragment != null) {
                    reposFragment.refreshView(true, true);
                }
            }
        });
        dialog.show(getSupportFragmentManager(), TAG_NEW_REPO_DIALOG_FRAGMENT);
    }

    /**
     * add new file/files
     */
    private void addFile() {
        final AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(getString(R.string.add_file));
        builder.setItems(R.array.add_file_options_array, new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                if (which == 0) // create file
                    showNewFileDialog();
                else if (which == 1) // create folder
                    showNewDirDialog();
                else if (which == 2) // upload file
                    pickFile();
                else if (which == 3) // take a photo
                    CameraTakePhoto();
            }
        }).show();
    }

    private void showNewDirDialog() {
        if (!hasRepoWritePermission()) {
            showShortToast(this, R.string.library_read_only);
            return;
        }

        final NewDirDialog dialog = new NewDirDialog();
        dialog.init(navContext.getRepoID(), navContext.getDirPath(), account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                final String message = String.format(getString(R.string.create_new_folder_success), dialog.getNewDirName());
                showShortToast(BrowserActivity.this, message);
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == INDEX_LIBRARY_TAB && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "NewDirDialogFragment");
    }

    private void showNewFileDialog() {
        if (!hasRepoWritePermission()) {
            showShortToast(this, R.string.library_read_only);
            return;
        }

        final NewFileDialog dialog = new NewFileDialog();
        dialog.init(navContext.getRepoID(), navContext.getDirPath(), account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                final String message = String.format(getString(R.string.create_new_file_success), dialog.getNewFileName());
                showShortToast(BrowserActivity.this, message);
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == INDEX_LIBRARY_TAB && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "NewFileDialogFragment");
    }

    public void setRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.TRUE);
    }

    public void unsetRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.FALSE);
    }

    private File takeCameraPhotoTempFile;

    private void CameraTakePhoto() {
        Intent imageCaptureIntent = new Intent("android.media.action.IMAGE_CAPTURE");

        try {
            File ImgDir = DataManager.createTempDir();

            String fileName = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()) + ".jpg";
            takeCameraPhotoTempFile = new File(ImgDir, fileName);

            Uri photo = Uri.fromFile(takeCameraPhotoTempFile);
            imageCaptureIntent.putExtra(MediaStore.EXTRA_OUTPUT, photo);
            startActivityForResult(imageCaptureIntent, TAKE_PHOTO_REQUEST);

        } catch (IOException e) {
            showShortToast(BrowserActivity.this, R.string.unknow_error);
        }
    }

    public void enableUpButton() {
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        setSupportActionBar(getActionBarToolbar());
        //getActionBarToolbar().setLogo(getResources().getDrawable(R.color.transparent));
    }

    public void disableUpButton() {
        getSupportActionBar().setDisplayHomeAsUpEnabled(false);
        //getActionBarToolbar().setEnabled(false);
        //getActionBarToolbar().setLogo(R.drawable.icon);
    }

    public void setUpButtonTitle(String title){
        getActionBarToolbar().setTitle(title);
    }

    /**
     * update up button title when sliding among tabs
     *
     * @param position
     */
    private void setUpButtonTitleOnSlideTabs(int position) {
        if (navContext == null)
            return;

        if (position == INDEX_LIBRARY_TAB) {
            if (navContext.inRepo()) {
                if (navContext.getDirPath().equals(BrowserActivity.ACTIONBAR_PARENT_PATH)) {
                    setUpButtonTitle(navContext.getRepoName());
                } else {
                    setUpButtonTitle(navContext.getDirPath().substring(
                            navContext.getDirPath().lastIndexOf(BrowserActivity.ACTIONBAR_PARENT_PATH) + 1));
                }
            } else
                setUpButtonTitle(getString(R.string.tabs_library).toUpperCase());
        } else {
            setUpButtonTitle(currentPosition == 1 ? getString(R.string.tabs_starred).toUpperCase() : getString(R.string.tabs_activity).toUpperCase());
        }

    }

    /**
     * refresh view when sliding among tabs
     *
     * @param position
     */
    private void refreshViewOnSlideTabs(int position) {
        if (navContext == null)
            return;

        if (position == INDEX_LIBRARY_TAB) {
            if (navContext.inRepo()) {
                getReposFragment().refreshView();
            }
        }

    }

    /***********  Start other activity  ***************/

    public static final int PICK_FILES_REQUEST = 1;
    public static final int PICK_PHOTOS_VIDEOS_REQUEST = 2;
    public static final int PICK_FILE_REQUEST = 3;
    public static final int TAKE_PHOTO_REQUEST = 4;
    public static final int CHOOSE_COPY_MOVE_DEST_REQUEST = 5;
    public static final int DOWNLOAD_FILE_REQUEST = 6;

    public boolean hasRepoWritePermission() {
        if (navContext == null) {
            return false;
        }
        if (navContext.getDirPermission().indexOf('w') == -1){
            return false;
        }
        return true;
    }

    void pickFile() {
        if (!hasRepoWritePermission()) {
            showShortToast(this, R.string.library_read_only);
            return;
        }

        // Starting with kitkat (or earlier?), the document picker has integrated image and local file support
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.KITKAT) {
            UploadChoiceDialog dialog = new UploadChoiceDialog();
            dialog.show(getSupportFragmentManager(), PICK_FILE_DIALOG_FRAGMENT_TAG);
        } else {
            Intent target = Utils.createGetContentIntent();
            Intent intent = Intent.createChooser(target, getString(R.string.choose_file));
            startActivityForResult(intent, BrowserActivity.PICK_FILE_REQUEST);
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {
        case PICK_FILES_REQUEST:
            if (resultCode == RESULT_OK) {
                String[] paths = data.getStringArrayExtra(MultiFileChooserActivity.MULTI_FILES_PATHS);
                if (paths == null)
                    return;
                showShortToast(this, getString(R.string.added_to_upload_tasks));

                List<SeafDirent> list = dataManager.getCachedDirents(navContext.getRepoID(), navContext.getDirPath());
                if (list == null) return;

                for (String path : paths) {
                    boolean duplicate = false;
                    for (SeafDirent dirent : list) {
                        if (dirent.name.equals(Utils.fileNameFromPath(path))) {
                            duplicate = true;
                            break;
                        }
                    }
                    if (!duplicate) {
                        showShortToast(BrowserActivity.this, getString(R.string.added_to_upload_tasks));
                        final SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                        if (repo != null && repo.canLocalDecrypt()) {
                            addUploadBlocksTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), path, repo.encVersion);
                        } else {
                            addUploadTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), path);
                        }
                    } else {
                        showFileExistDialog(path);
                    }
                }
            }
            break;
        case PICK_PHOTOS_VIDEOS_REQUEST:
            if (resultCode == RESULT_OK) {
                ArrayList<String> paths = data.getStringArrayListExtra("photos");
                if (paths == null)
                    return;
                showShortToast(this, getString(R.string.added_to_upload_tasks));

                List<SeafDirent> list = dataManager.getCachedDirents(navContext.getRepoID(), navContext.getDirPath());
                if (list == null) return;

                for (String path : paths) {
                    boolean duplicate = false;
                    for (SeafDirent dirent : list) {
                        if (dirent.name.equals(Utils.fileNameFromPath(path))) {
                            duplicate = true;
                            break;
                        }
                    }
                    if (!duplicate) {
                        showShortToast(BrowserActivity.this, getString(R.string.added_to_upload_tasks));
                        final SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                        if (repo != null && repo.canLocalDecrypt()) {
                            addUploadBlocksTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), path,repo.encVersion);
                        }else {
                            addUploadTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), path);
                        }
                    } else {
                        showFileExistDialog(path);
                    }
                }
            }
            break;
        case PICK_FILE_REQUEST:
            if (resultCode == RESULT_OK) {
                if (!Utils.isNetworkOn()) {
                    showShortToast(this, R.string.network_down);
                    return;
                }

                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                    List<Uri> uriList = UtilsJellyBean.extractUriListFromIntent(data);
                    if (uriList.size() > 0) {
                        ConcurrentAsyncTask.execute(new SAFLoadRemoteFileTask(), uriList.toArray(new Uri[]{}));
                    } else {
                        showShortToast(BrowserActivity.this, R.string.saf_upload_path_not_available);
                    }
                } else {
                    Uri uri = data.getData();
                    if (uri != null) {
                        ConcurrentAsyncTask.execute(new SAFLoadRemoteFileTask(), uri);
                    } else {
                        showShortToast(BrowserActivity.this, R.string.saf_upload_path_not_available);
                    }
                }
            }
            break;
        case CHOOSE_COPY_MOVE_DEST_REQUEST:
            if (resultCode == RESULT_OK) {
                if (!Utils.isNetworkOn()) {
                    showShortToast(this, R.string.network_down);
                    return;
                }

                copyMoveIntent = data;
            }
            break;
        case TAKE_PHOTO_REQUEST:
            if (resultCode == RESULT_OK) {
                showShortToast(this, getString(R.string.take_photo_successfully));
                if (!Utils.isNetworkOn()) {
                    showShortToast(this, R.string.network_down);
                    return;
                }

                if(takeCameraPhotoTempFile == null) {
                    showShortToast(this, getString(R.string.saf_upload_path_not_available));
                    Log.i(DEBUG_TAG, "Pick file request did not return a path");
                    return;
                }
                showShortToast(this, getString(R.string.added_to_upload_tasks));
                final SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                if (repo != null && repo.canLocalDecrypt()) {
                    addUploadBlocksTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), takeCameraPhotoTempFile.getAbsolutePath(), repo.encVersion);
                } else {
                    addUploadTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), takeCameraPhotoTempFile.getAbsolutePath());
                }

            }
            break;
            case DOWNLOAD_FILE_REQUEST:
                if (resultCode == RESULT_OK) {
                    File file = new File(data.getStringExtra("path"));
                    WidgetUtils.showFile(BrowserActivity.this, file);
                }
        default:
             break;
        }
    }

    class SAFLoadRemoteFileTask extends AsyncTask<Uri, Void, File[]> {

        @Override
        protected File[] doInBackground(Uri... uriList) {
            if (uriList == null)
                return null;

            List<File> fileList = new ArrayList<File>();
            for (Uri uri: uriList) {
                // Log.d(DEBUG_TAG, "Uploading file from uri: " + uri);

                InputStream in = null;
                OutputStream out = null;

                try {
                    File tempDir = DataManager.createTempDir();
                    File tempFile = new File(tempDir, Utils.getFilenamefromUri(BrowserActivity.this, uri));

                    if (!tempFile.createNewFile()) {
                        throw new RuntimeException("could not create temporary file");
                    }

                    in = getContentResolver().openInputStream(uri);
                    out = new FileOutputStream(tempFile);
                    IOUtils.copy(in, out);

                    fileList.add(tempFile);

                } catch (IOException e) {
                    Log.d(DEBUG_TAG, "Could not open requested document", e);
                } catch (RuntimeException e) {
                    Log.d(DEBUG_TAG, "Could not open requested document", e);
                } finally {
                    IOUtils.closeQuietly(in);
                    IOUtils.closeQuietly(out);
                }
            }
            return fileList.toArray(new File[]{});
        }

        @Override
        protected void onPostExecute(File... fileList) {
            if (fileList == null) return;

            List<SeafDirent> list = dataManager.getCachedDirents(navContext.getRepoID(), navContext.getDirPath());

            for (final File file: fileList) {
                if (file == null) {
                    showShortToast(BrowserActivity.this, R.string.saf_upload_path_not_available);
                } else {
                    if (list == null) {
                        Log.e(DEBUG_TAG, "Seadroid dirent cache is empty in uploadFile. Should not happen, aborting.");
                        return;
                    }

                    boolean duplicate = false;
                    for (SeafDirent dirent : list) {
                        if (dirent.name.equals(file.getName())) {
                            duplicate = true;
                            break;
                        }
                    }

                    if (!duplicate) {
                        final SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                        showShortToast(BrowserActivity.this, getString(R.string.added_to_upload_tasks));
                        if (repo != null && repo.canLocalDecrypt()) {
                            addUploadBlocksTask(repo.id, repo.name, navContext.getDirPath(), file.getAbsolutePath(), repo.encVersion);
                        } else {
                            addUploadTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), file.getAbsolutePath());
                        }
                    } else {
                        showFileExistDialog(file);
                    }
                }
            }

            if (txService == null)
                return;

            if (!txService.hasUploadNotifProvider()) {
                UploadNotificationProvider provider = new UploadNotificationProvider(
                        txService.getUploadTaskManager(),
                        txService);
                txService.saveUploadNotifProvider(provider);
            }
        }
    }

    private void showFileExistDialog(final String filePath){
        showFileExistDialog(new File(filePath));
    }

    private void showFileExistDialog(final File file) {
        final SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
        AlertDialog.Builder builder = new AlertDialog.Builder(this);
        builder.setTitle(getString(R.string.upload_file_exist));
        builder.setMessage(String.format(getString(R.string.upload_duplicate_found), file.getName()));
        builder.setPositiveButton(getString(R.string.upload_replace), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                showShortToast(BrowserActivity.this, getString(R.string.added_to_upload_tasks));
                if (repo != null && repo.canLocalDecrypt()) {
                    addUpdateBlocksTask(repo.id, repo.name, navContext.getDirPath(), file.getAbsolutePath(), repo.encVersion);
                } else {
                    addUpdateTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), file.getAbsolutePath());
                }
            }
        });
        builder.setNeutralButton(getString(R.string.cancel), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
            }
        });
        builder.setNegativeButton(getString(R.string.upload_keep_both), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                if (repo != null && repo.canLocalDecrypt()) {
                    addUploadBlocksTask(repo.id, repo.name, navContext.getDirPath(), file.getAbsolutePath(), repo.encVersion);
                } else {
                    addUploadTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), file.getAbsolutePath());
                }
            }
        });
        builder.show();
    }

    public void onItemSelected() {
        // update contextual action bar (CAB) title
        getReposFragment().updateContextualActionBar();
    }

    /***************  Navigation *************/

    @Override
    public void onFileSelected(SeafDirent dirent) {
        final String fileName= dirent.name;
        final long fileSize = dirent.size;
        final String repoName = navContext.getRepoName();
        final String repoID = navContext.getRepoID();
        final String dirPath = navContext.getDirPath();
        final String filePath = Utils.pathJoin(navContext.getDirPath(), fileName);
        final SeafRepo repo = dataManager.getCachedRepoByID(repoID);

        // Encrypted repo doesn\`t support gallery,
        // because pic thumbnail under encrypted repo was not supported at the server side
        if (Utils.isViewableImage(fileName)
                && repo != null && !repo.encrypted) {
            WidgetUtils.startGalleryActivity(this, repoName, repoID, dirPath, fileName, account);
            return;
        }

        final File localFile = dataManager.getLocalCachedFile(repoName, repoID, filePath, dirent.id);
        if (localFile != null) {
            WidgetUtils.showFile(this, localFile);
            return;
        }

        startFileActivity(repoName, repoID, filePath, fileSize);
    }

    /**
     * Download a file
     *
     * @param dir
     * @param fileName
     */
    public void downloadFile(String dir, String fileName) {
        String filePath = Utils.pathJoin(dir, fileName);
        txService.addDownloadTask(account,
                navContext.getRepoName(),
                navContext.getRepoID(),
                filePath);

        if (!txService.hasDownloadNotifProvider()) {
            DownloadNotificationProvider provider = new DownloadNotificationProvider(txService.getDownloadTaskManager(),
                    txService);
            txService.saveDownloadNotifProvider(provider);
        }

        SeafItemAdapter adapter = getReposFragment().getAdapter();
        List<DownloadTaskInfo> infos = txService.getDownloadTaskInfosByPath(navContext.getRepoID(), dir);
        // update downloading progress
        adapter.setDownloadTaskList(infos);
    }

    /**
     * Download all files (folders) under a given folder
     *
     * @param dirPath
     * @param fileName name of the download folder
     * @param recurse
     */
    public void downloadDir(String dirPath, String fileName, boolean recurse) {
        if (!Utils.isNetworkOn()) {
            showShortToast(this, R.string.network_down);
            return;
        }

        ConcurrentAsyncTask.execute(new DownloadDirTask(),
                navContext.getRepoName(),
                navContext.getRepoID(),
                dirPath,
                String.valueOf(recurse),
                fileName);
    }

    private class DownloadDirTask extends AsyncTask<String, Void, List<SeafDirent> > {

        private String repoName;
        private String repoID;
        private String fileName;
        private String dirPath;
        private int fileCount;
        private boolean recurse;
        private ArrayList<String> dirPaths = Lists.newArrayList();
        private SeafException err = null;

        @Override
        protected List<SeafDirent> doInBackground(String... params) {
            if (params.length != 5) {
                Log.d(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }

            repoName = params[0];
            repoID = params[1];
            dirPath = params[2];
            recurse = Boolean.valueOf(params[3]);
            fileName = params[4];


            ArrayList<SeafDirent> dirents = Lists.newArrayList();

            dirPaths.add(Utils.pathJoin(dirPath, fileName));

            // don`t use for each loop here
            for (int i = 0; i < dirPaths.size(); i++) {

                List<SeafDirent> currentDirents;
                try {
                    currentDirents = dataManager.getDirentsFromServer(repoID, dirPaths.get(i));
                } catch (SeafException e) {
                    err = e;
                    e.printStackTrace();
                    return null;
                }

                if (currentDirents == null)
                    continue;

                for (SeafDirent seafDirent : currentDirents) {
                    if (seafDirent.isDir()) {
                        if (recurse) {
                            dirPaths.add(Utils.pathJoin(dirPaths.get(i), seafDirent.name));
                        }
                    } else {
                        File localCachedFile = dataManager.getLocalCachedFile(repoName,
                                repoID,
                                Utils.pathJoin(dirPaths.get(i),
                                        seafDirent.name),
                                seafDirent.id);
                        if (localCachedFile != null) {
                            continue;
                        }

                        // txService maybe null if layout orientation has changed
                        // e.g. landscape and portrait switch
                        if (txService == null)
                            return null;

                        txService.addTaskToDownloadQue(account,
                                repoName,
                                repoID,
                                Utils.pathJoin(dirPaths.get(i),
                                        seafDirent.name));

                        fileCount++;
                    }
                }
            }

            return dirents;
        }

        @Override
        protected void onPostExecute(List<SeafDirent> dirents) {
            if (dirents == null) {
                if (err != null) {
                    showShortToast(BrowserActivity.this, R.string.transfer_list_network_error);
                }
                return;
            }

            if (fileCount == 0)
                showShortToast(BrowserActivity.this, R.string.transfer_download_no_task);
            else {
                showShortToast(BrowserActivity.this, getResources().getQuantityString(R.plurals.transfer_download_started, fileCount, fileCount));
                if (!txService.hasDownloadNotifProvider()) {
                    DownloadNotificationProvider provider = new DownloadNotificationProvider(txService.getDownloadTaskManager(),
                            txService);
                    txService.saveDownloadNotifProvider(provider);
                }
            }

            // set download tasks info to adapter in order to update download progress in UI thread
            getReposFragment().getAdapter().setDownloadTaskList(txService.getDownloadTaskInfosByPath(repoID, dirPath));
        }
    }

    private void startFileActivity(String repoName, String repoID, String filePath, long fileSize) {
        int taskID = txService.addDownloadTask(account, repoName, repoID, filePath, fileSize);
        Intent intent = new Intent(this, FileActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", account);
        intent.putExtra("taskID", taskID);
        startActivityForResult(intent, DOWNLOAD_FILE_REQUEST);
    }

    @Override
    public void onStarredFileSelected(final SeafStarredFile starredFile) {
        final long fileSize = starredFile.getSize();
        final String repoID = starredFile.getRepoID();
        final SeafRepo repo = dataManager.getCachedRepoByID(repoID);
        if (repo == null)
            return;

        if (repo.encrypted && !dataManager.getRepoPasswordSet(repo.id)) {
            String password = dataManager.getRepoPassword(repo.id);
            showPasswordDialog(repo.name, repo.id,
                    new TaskDialog.TaskDialogListener() {
                        @Override
                        public void onTaskSuccess() {
                            onStarredFileSelected(starredFile);
                        }
                    }, password);

            return;
        }

        final String repoName = repo.getName();
        final String filePath = starredFile.getPath();
        final String dirPath = Utils.getParentPath(filePath);

        // Encrypted repo doesn\`t support gallery,
        // because pic thumbnail under encrypted repo was not supported at the server side
        if (Utils.isViewableImage(starredFile.getTitle()) && !repo.encrypted) {
            WidgetUtils.startGalleryActivity(this, repoName, repoID, dirPath, starredFile.getTitle(), account);
            return;
        }

        final File localFile = dataManager.getLocalCachedFile(repoName, repoID, filePath, null);
        if (localFile != null) {
            WidgetUtils.showFile(this, localFile);
            return;
        }

        startFileActivity(repoName, repoID, filePath, fileSize);
    }

    @Override
    public void onBackPressed() {
        if (getSupportFragmentManager().getBackStackEntryCount() != 0) {
            getSupportFragmentManager().popBackStack();
            return;
        }

        if (currentPosition == INDEX_LIBRARY_TAB) {
            if (navContext.inRepo()) {
                if (navContext.isRepoRoot()) {
                    navContext.setRepoID(null);
                    getActionBarToolbar().setTitle(R.string.app_name);
                } else {
                    String parentPath = Utils.getParentPath(navContext
                            .getDirPath());
                    navContext.setDir(parentPath, null);
                    if (parentPath.equals(ACTIONBAR_PARENT_PATH)) {
                        getActionBarToolbar().setTitle(navContext.getRepoName());
                    }else {
                        getActionBarToolbar().setTitle(parentPath.substring(parentPath.lastIndexOf(ACTIONBAR_PARENT_PATH) + 1));
                    }
                }
                getReposFragment().refreshView(true);

            } else
                super.onBackPressed();
        } else if (currentPosition == INDEX_ACTIVITIES_TAB) {
            if (getActivitiesFragment().isBottomSheetShown()) {
                getActivitiesFragment().hideBottomSheet();
            } else
                super.onBackPressed();
        } else
            super.onBackPressed();
    }

    @Override
    public void onBackStackChanged() {
    }


    /************  Files ************/

    /**
     * Export a file.
     * 1. first ask the user to choose an app
     * 2. then download the latest version of the file
     * 3. start the choosen app
     *
     * @param fileName The name of the file to share in the current navcontext
     */
    public void exportFile(String fileName, long fileSize) {
        String repoName = navContext.getRepoName();
        String repoID = navContext.getRepoID();
        String dirPath = navContext.getDirPath();
        String fullPath = Utils.pathJoin(dirPath, fileName);
        chooseExportApp(repoName, repoID, fullPath, fileSize);
    }

    private void chooseExportApp(final String repoName, final String repoID, final String path, final long fileSize) {
        final File file = dataManager.getLocalRepoFile(repoName, repoID, path);
        Uri uri = Uri.fromFile(file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        // Get a list of apps
        List<ResolveInfo> infos = Utils.getAppsByIntent(sendIntent);

        if (infos.isEmpty()) {
            showShortToast(this, R.string.no_app_available);
            return;
        }

        AppChoiceDialog dialog = new AppChoiceDialog();
        dialog.init(getString(R.string.export_file), infos, new AppChoiceDialog.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(CustomAction action) {
            }
            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                sendIntent.setClassName(packageName, className);

                if (!Utils.isNetworkOn() && file.exists()) {
                    startActivity(sendIntent);
                    return;
                }
                fetchFileAndExport(appInfo, sendIntent, repoName, repoID, path, fileSize);
            }

        });
        dialog.show(getSupportFragmentManager(), CHOOSE_APP_DIALOG_FRAGMENT_TAG);
    }

    private void fetchFileAndExport(final ResolveInfo appInfo, final Intent intent,
                                    final String repoName, final String repoID, final String path, final long fileSize) {

        fetchFileDialog = new FetchFileDialog();
        fetchFileDialog.init(repoName, repoID, path, fileSize, new FetchFileDialog.FetchFileListener() {
            @Override
            public void onSuccess() {
                startActivity(intent);
            }

            @Override
            public void onDismiss() {
                fetchFileDialog = null;
            }

            @Override
            public void onFailure(SeafException err) {
            }
        });
        fetchFileDialog.show(getSupportFragmentManager(), OPEN_FILE_DIALOG_FRAGMENT_TAG);
    }

    public void renameRepo(String repoID, String repoName) {
        final RenameRepoDialog dialog = new RenameRepoDialog();
        dialog.init(repoID, repoName, account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showShortToast(BrowserActivity.this, R.string.rename_successful);
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == INDEX_LIBRARY_TAB && reposFragment != null) {
                    reposFragment.refreshView(true, true);
                }
            }
        });
        dialog.show(getSupportFragmentManager(), TAG_RENAME_REPO_DIALOG_FRAGMENT);
    }

    public void deleteRepo(String repoID) {
        final DeleteRepoDialog dialog = new DeleteRepoDialog();
        dialog.init(repoID, account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showShortToast(BrowserActivity.this, R.string.delete_successful);
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == INDEX_LIBRARY_TAB && reposFragment != null) {
                    reposFragment.refreshView(true, true);
                }
            }
        });
        dialog.show(getSupportFragmentManager(), TAG_DELETE_REPO_DIALOG_FRAGMENT);
    }

    /**
     * Share a file. Generating a file share link and send the link to someone
     * through some app.
     * @param repoID
     * @param path
     */
    public void shareFile(final String repoID, final String path, boolean isEncrypt) {
        WidgetUtils.chooseShareApp(BrowserActivity.this, repoID, path, isEncrypt, false, account);
    }

    public void shareDir(String repoID, String path, boolean isEncrypt) {
        WidgetUtils.chooseShareApp(this, repoID, path, isEncrypt, true, account);
    }

    public void renameFile(String repoID, String repoName, String path) {
        doRename(repoID, repoName, path, false);
    }

    public void renameDir(String repoID, String repoName, String path) {
        doRename(repoID, repoName, path, true);
    }

    private void doRename(String repoID, String repoName, String path, boolean isdir) {
        final RenameFileDialog dialog = new RenameFileDialog();
        dialog.init(repoID, path, isdir, account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showShortToast(BrowserActivity.this, R.string.rename_successful);
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == INDEX_LIBRARY_TAB && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), TAG_RENAME_FILE_DIALOG_FRAGMENT);
    }

    public void deleteFile(String repoID, String repoName, String path) {
        doDelete(repoID, repoName, path, false);
    }

    public void deleteDir(String repoID, String repoName, String path) {
        doDelete(repoID, repoName, path, true);
    }

    private void doDelete(String repoID, String repoName, String path, boolean isdir) {
        final DeleteFileDialog dialog = new DeleteFileDialog();
        dialog.init(repoID, path, isdir, account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showShortToast(BrowserActivity.this, R.string.delete_successful);
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == INDEX_LIBRARY_TAB && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), TAG_DELETE_FILE_DIALOG_FRAGMENT);
    }

    public void copyFile(String srcRepoId, String srcRepoName, String srcDir, String srcFn, boolean isdir) {
        chooseCopyMoveDest(srcRepoId, srcRepoName, srcDir, srcFn, isdir, CopyMoveContext.OP.COPY);
    }

    public void moveFile(String srcRepoId, String srcRepoName, String srcDir, String srcFn, boolean isdir) {
        chooseCopyMoveDest(srcRepoId, srcRepoName, srcDir, srcFn, isdir, CopyMoveContext.OP.MOVE);
    }

    public void starFile(String srcRepoId, String srcDir, String srcFn) {
        getStarredFragment().doStarFile(srcRepoId, srcDir, srcFn);
    }

    private void chooseCopyMoveDest(String repoID, String repoName, String path,
                                    String filename, boolean isdir, CopyMoveContext.OP op) {
        copyMoveContext = new CopyMoveContext(repoID, repoName, path, filename,
                                              isdir, op);
        Intent intent = new Intent(this, SeafilePathChooserActivity.class);
        intent.putExtra(SeafilePathChooserActivity.DATA_ACCOUNT, account);
        SeafRepo repo = dataManager.getCachedRepoByID(repoID);
        if (repo.encrypted) {
            intent.putExtra(SeafilePathChooserActivity.ENCRYPTED_REPO_ID, repoID);
        }
        startActivityForResult(intent, CHOOSE_COPY_MOVE_DEST_REQUEST);
        return;
    }

    private void doCopyMove() {
        if (!copyMoveContext.checkCopyMoveToSubfolder()) {
            showShortToast(this, copyMoveContext.isCopy()
                    ? R.string.cannot_copy_folder_to_subfolder
                    : R.string.cannot_move_folder_to_subfolder);
            return;
        }
        final CopyMoveDialog dialog = new CopyMoveDialog();
        dialog.init(account, copyMoveContext);
        dialog.setCancelable(false);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showShortToast(BrowserActivity.this, copyMoveContext.isCopy()
                        ? R.string.copied_successfully
                        : R.string.moved_successfully);
                if (copyMoveContext.batch) {
                    List<SeafDirent> cachedDirents = getDataManager().getCachedDirents(getNavContext().getRepoID(),
                            getNavContext().getDirPath());

                    // refresh view
                    if (getReposFragment().getAdapter() != null) {
                        getReposFragment().getAdapter().setItems(cachedDirents);
                        getReposFragment().getAdapter().notifyDataSetChanged();
                    }

                    if (cachedDirents.size() == 0)
                        getReposFragment().getEmptyView().setVisibility(View.VISIBLE);
                    return;
                }

                if (copyMoveContext.isMove()) {
                    ReposFragment reposFragment = getReposFragment();
                    if (currentPosition == INDEX_LIBRARY_TAB && reposFragment != null) {
                        reposFragment.refreshView();
                    }
                }
            }
        });
        dialog.show(getSupportFragmentManager(), TAG_COPY_MOVE_DIALOG_FRAGMENT);
    }

    private void onFileDownloadFailed(int taskID) {
        if (txService == null) {
            return;
        }

        DownloadTaskInfo info = txService.getDownloadTaskInfo(taskID);
        if (info == null)
            return;

        final SeafException err = info.err;
        final String repoName = info.repoName;
        final String repoID = info.repoID;
        final String path = info.pathInRepo;

        if (err != null && err.getCode() == SeafConnection.HTTP_STATUS_REPO_PASSWORD_REQUIRED) {
            if (currentPosition == INDEX_LIBRARY_TAB
                    && repoID.equals(navContext.getRepoID())
                    && Utils.getParentPath(path).equals(navContext.getDirPath())) {
                showPasswordDialog(repoName, repoID,
                        new TaskDialog.TaskDialogListener() {
                            @Override
                            public void onTaskSuccess() {
                                txService.addDownloadTask(account,
                                                          repoName,
                                                          repoID,
                                                          path);
                            }
                        });
                return;
            }
        }

        showShortToast(this, getString(R.string.download_failed));
    }

    private void onFileUploaded(int taskID) {
        if (txService == null) {
            return;
        }

        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);

        if (info == null) {
            return;
        }

        String repoID = info.repoID;
        String dir = info.parentDir;
        if (currentPosition == INDEX_LIBRARY_TAB
                && repoID.equals(navContext.getRepoID())
                && dir.equals(navContext.getDirPath())) {
            getReposFragment().refreshView(true, true);
            String verb = getString(info.isUpdate ? R.string.updated : R.string.uploaded);
            showShortToast(this, verb + " " + Utils.fileNameFromPath(info.localFilePath));
        }
    }

    private int intShowErrorTime;
    private void onFileUploadFailed(int taskID) {
        if (++ intShowErrorTime <= 1)
            showShortToast(this, getString(R.string.upload_failed));
    }

    public PasswordDialog showPasswordDialog(String repoName, String repoID,
                                             TaskDialog.TaskDialogListener listener) {
        return showPasswordDialog(repoName, repoID, listener, null);
    }

    public PasswordDialog showPasswordDialog(String repoName, String repoID,
                                             TaskDialog.TaskDialogListener listener, String password) {
        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.setRepo(repoName, repoID, account);
        if (password != null) {
            passwordDialog.setPassword(password);
        }
        passwordDialog.setTaskDialogLisenter(listener);
        passwordDialog.show(getSupportFragmentManager(), PASSWORD_DIALOG_FRAGMENT_TAG);
        return passwordDialog;
    }

    /************  Multiple Files ************/

    /**
     * Delete multiple fiels
     *
     * @param repoID
     * @param path
     * @param dirents
     */
    public void deleteFiles(final String repoID, String path, List<SeafDirent> dirents) {
        final DeleteFileDialog dialog = new DeleteFileDialog();
        dialog.init(repoID, path, dirents, account);
        dialog.setCancelable(false);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showShortToast(BrowserActivity.this, R.string.delete_successful);
                if (getDataManager() != null) {
                    List<SeafDirent> cachedDirents = getDataManager().getCachedDirents(repoID,
                            getNavContext().getDirPath());
                    getReposFragment().getAdapter().setItems(cachedDirents);
                    getReposFragment().getAdapter().notifyDataSetChanged();
                    // update contextual action bar (CAB) title
                    getReposFragment().updateContextualActionBar();
                    if (cachedDirents.size() == 0)
                        getReposFragment().getEmptyView().setVisibility(View.VISIBLE);
                }
            }
        });
        dialog.show(getSupportFragmentManager(), TAG_DELETE_FILES_DIALOG_FRAGMENT);
    }

    /**
     * Copy multiple files
     *
     * @param srcRepoId
     * @param srcRepoName
     * @param srcDir
     * @param dirents
     */
    public void copyFiles(String srcRepoId, String srcRepoName, String srcDir, List<SeafDirent> dirents) {
        chooseCopyMoveDestForMultiFiles(srcRepoId, srcRepoName, srcDir, dirents, CopyMoveContext.OP.COPY);
    }

    /**
     * Move multiple files
     *
     * @param srcRepoId
     * @param srcRepoName
     * @param srcDir
     * @param dirents
     */
    public void moveFiles(String srcRepoId, String srcRepoName, String srcDir, List<SeafDirent> dirents) {
        chooseCopyMoveDestForMultiFiles(srcRepoId, srcRepoName, srcDir, dirents, CopyMoveContext.OP.MOVE);
    }

    /**
     * Choose copy/move destination for multiple files
     *
     * @param repoID
     * @param repoName
     * @param dirPath
     * @param dirents
     * @param op
     */
    private void chooseCopyMoveDestForMultiFiles(String repoID, String repoName, String dirPath, List<SeafDirent> dirents, CopyMoveContext.OP op) {
        copyMoveContext = new CopyMoveContext(repoID, repoName, dirPath, dirents, op);
        Intent intent = new Intent(this, SeafilePathChooserActivity.class);
        intent.putExtra(SeafilePathChooserActivity.DATA_ACCOUNT, account);
        SeafRepo repo = getDataManager().getCachedRepoByID(repoID);
        if (repo.encrypted) {
            intent.putExtra(SeafilePathChooserActivity.ENCRYPTED_REPO_ID, repoID);
        }
        startActivityForResult(intent, BrowserActivity.CHOOSE_COPY_MOVE_DEST_REQUEST);
    }

    /**
     * Add selected files (folders) to downloading queue,
     * folders with subfolder will be downloaded recursively.
     *
     * @param repoID
     * @param repoName
     * @param dirPath
     * @param dirents
     */
    public void downloadFiles(String repoID, String repoName, String dirPath, List<SeafDirent> dirents) {
        if (!Utils.isNetworkOn()) {
            showShortToast(this, R.string.network_down);
            return;
        }

        DownloadFilesTask task = new DownloadFilesTask(repoID, repoName, dirPath, dirents);
        ConcurrentAsyncTask.execute(task);
    }

    /**
     * Task for asynchronously downloading selected files (folders),
     * files wont be added to downloading queue if they have already been cached locally.
     */
    class DownloadFilesTask extends AsyncTask<Void, Void, Void> {
        private String repoID, repoName, dirPath;
        private List<SeafDirent> dirents;
        private SeafException err;
        private int fileCount;

        public DownloadFilesTask(String repoID, String repoName, String dirPath, List<SeafDirent> dirents) {
            this.repoID = repoID;
            this.repoName = repoName;
            this.dirPath = dirPath;
            this.dirents = dirents;
        }

        @Override
        protected void onPreExecute() {
            getReposFragment().showLoading(true);
        }

        @Override
        protected Void doInBackground(Void... params) {
            ArrayList<String> dirPaths = Lists.newArrayList(dirPath);
            for (int i = 0; i < dirPaths.size(); i++) {
                if (i > 0) {
                    try {
                        dirents = getDataManager().getDirentsFromServer(repoID, dirPaths.get(i));
                    } catch (SeafException e) {
                        err = e;
                        Log.e(DEBUG_TAG, e.getMessage() + e.getCode());
                    }
                }

                if (dirents == null)
                    continue;

                for (SeafDirent seafDirent : dirents) {
                    if (seafDirent.isDir()) {
                        // download files recursively
                        dirPaths.add(Utils.pathJoin(dirPaths.get(i), seafDirent.name));
                    } else {
                        File localCachedFile = getDataManager().getLocalCachedFile(repoName,
                                repoID,
                                Utils.pathJoin(dirPaths.get(i),
                                        seafDirent.name),
                                seafDirent.id);
                        if (localCachedFile != null) {
                            continue;
                        }

                        // txService maybe null if layout orientation has changed
                        // e.g. landscape and portrait switch
                        if (txService == null)
                            return null;

                        txService.addTaskToDownloadQue(account,
                                repoName,
                                repoID,
                                Utils.pathJoin(dirPaths.get(i),
                                        seafDirent.name));

                        fileCount++;
                    }

                }
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void aVoid) {
            // update ui
            getReposFragment().showLoading(false);

            if (err != null) {
                showShortToast(BrowserActivity.this, R.string.transfer_list_network_error);
                return;
            }

            if (fileCount == 0)
                showShortToast(BrowserActivity.this, R.string.transfer_download_no_task);
            else {
                showShortToast(BrowserActivity.this,
                        getResources().getQuantityString(R.plurals.transfer_download_started,
                                fileCount,
                                fileCount));

                if (!txService.hasDownloadNotifProvider()) {
                    DownloadNotificationProvider provider =
                            new DownloadNotificationProvider(txService.getDownloadTaskManager(),
                            txService);
                    txService.saveDownloadNotifProvider(provider);
                }

            }

        }
    }

    @Override
    public boolean onKeyUp(int keycode, KeyEvent e) {
        switch(keycode) {
        case KeyEvent.KEYCODE_MENU:
            if (overFlowMenu !=null) {
                overFlowMenu.performIdentifierAction(R.id.menu_overflow, 0);
            }
        }

        return super.onKeyUp(keycode, e);
    }

    // for receive broadcast from TransferService
    private class TransferReceiver extends BroadcastReceiver {

        private TransferReceiver() {}

        public void onReceive(Context context, Intent intent) {
            String type = intent.getStringExtra("type");
            if (type.equals(DownloadTaskManager.BROADCAST_FILE_DOWNLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileDownloadFailed(taskID);
            } else if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploaded(taskID);
            } else if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadFailed(taskID);
            }
        }

    } // TransferReceiver


    public void showRepoBottomSheet(SeafRepo repo) {
        getReposFragment().showRepoBottomSheet(repo);
    }

    public void showFileBottomSheet(String title, final SeafDirent dirent) {
        getReposFragment().showFileBottomSheet(title, dirent);
    }

    public void showDirBottomSheet(String title, final SeafDirent dirent) {
        getReposFragment().showDirBottomSheet(title, dirent);
    }

}
