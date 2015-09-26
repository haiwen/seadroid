package com.seafile.seadroid2.ui.activity;

import android.app.ActivityManager;
import android.app.ActivityManager.RunningServiceInfo;
import android.app.Dialog;
import android.content.*;
import android.content.pm.ResolveInfo;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.provider.MediaStore;
import android.support.v4.app.*;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.content.LocalBroadcastManager;
import android.support.v4.view.ViewPager;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.LinearLayout;
import android.widget.TextView;
import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;
import com.astuetz.PagerSlidingTabStrip;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.*;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountDBHelper;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.cameraupload.CameraUploadService;
import com.seafile.seadroid2.data.*;
import com.seafile.seadroid2.fileschooser.MultiFileChooserActivity;
import com.seafile.seadroid2.monitor.FileMonitorService;
import com.seafile.seadroid2.notification.DownloadNotificationProvider;
import com.seafile.seadroid2.notification.UploadNotificationProvider;
import com.seafile.seadroid2.transfer.*;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.ui.CopyMoveContext;
import com.seafile.seadroid2.ui.SeafileStyleDialogBuilder;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.adapter.SeafItemAdapter;
import com.seafile.seadroid2.ui.dialog.*;
import com.seafile.seadroid2.ui.dialog.AppChoiceDialog.CustomAction;
import com.seafile.seadroid2.ui.fragment.ActivitiesFragment;
import com.seafile.seadroid2.ui.fragment.ReposFragment;
import com.seafile.seadroid2.ui.fragment.StarredFragment;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.util.UtilsJellyBean;
import com.viewpagerindicator.IconPagerAdapter;
import org.apache.commons.io.IOUtils;
import org.json.JSONException;

import java.io.*;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class BrowserActivity extends SherlockFragmentActivity
        implements ReposFragment.OnFileSelectedListener, StarredFragment.OnStarredFileSelectedListener, OnBackStackChangedListener {
    public static final String PKG_NAME = "com.seafile.seadroid2";
    public static final String EXTRA_REPO_NAME = PKG_NAME + ".repoName";
    public static final String EXTRA_REPO_ID = PKG_NAME + ".repoID";
    public static final String EXTRA_FILE_PATH = PKG_NAME + ".filePath";
    public static final String EXTRA_ACCOUT = PKG_NAME + ".account";
    private static final String DEBUG_TAG = "BrowserActivity";
    public static final String ACTIONBAR_PARENT_PATH = "/";
    private static final String UPLOAD_TASKS_VIEW = "UploadTasks";
    private static final String FILES_VIEW = "Files";

    public static final String OPEN_FILE_DIALOG_FRAGMENT_TAG = "openfile_fragment";
    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "password_fragment";
    public static final String CHOOSE_APP_DIALOG_FRAGMENT_TAG = "choose_app_fragment";
    public static final String PICK_FILE_DIALOG_FRAGMENT_TAG = "pick_file_fragment";

    private static final int[] ICONS = new int[] {
        R.drawable.tab_library, R.drawable.tab_starred,
        R.drawable.tab_activity
    };
    private int currentPosition = 0;
    private SeafileTabsAdapter adapter;
    private ViewPager pager;
    private PagerSlidingTabStrip tabStrip;
    private LinearLayout mTabsLinearLayout;

    private Account account;
    NavContext navContext = new NavContext();
    DataManager dataManager = null;
    TransferService txService = null;
    TransferReceiver mTransferReceiver;
    SettingsManager settingsMgr;
    private String currentSelectedItem = FILES_VIEW;

    FetchFileDialog fetchFileDialog = null;

    AppChoiceDialog appChoiceDialog = null;

    private Menu overFlowMenu;
    private MenuItem menuSearch;

    private String mCurrentRepoName, mCurrentRepoID, mCurrentDir;

    private Intent copyMoveIntent;

    private CopyMoveContext copyMoveContext;

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

    private int addUploadTask(String repoID, String repoName, String targetDir, String localFilePath) {
        if (txService != null) {
            return txService.addTaskToUploadQue(account, repoID, repoName, targetDir, localFilePath, false, true);
        } else {
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false, true);
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

    public void disableActionBarTitle() {
        getSupportActionBar().setDisplayShowTitleEnabled(false);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        super.onCreate(savedInstanceState);
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

        // Get the message from the intent
        Intent intent = getIntent();
        String server = intent.getStringExtra(AccountManager.SHARED_PREF_SERVER_KEY);
        String email = intent.getStringExtra(AccountManager.SHARED_PREF_EMAIL_KEY);
        String token = intent.getStringExtra(AccountManager.SHARED_PREF_TOKEN_KEY);
        account = new Account(server, email, null, token);
        Log.d(DEBUG_TAG, "browser activity onCreate " + server + " " + email);

        if (server == null) {
            AccountManager accountManager = new AccountManager(this);
            // get current Account from SharedPreference
            // "current" means the Account is in using at foreground if multiple accounts exist
            Account act = accountManager.getCurrentAccount();
            if (act != null) {
                account = act;
            } else {
                Intent newIntent = new Intent(this, AccountsActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(newIntent);
                finish();
                return;
            }

        }

        dataManager = new DataManager(account);

        getSupportFragmentManager().addOnBackStackChangedListener(this);

        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayShowTitleEnabled(true);
        unsetRefreshing();
        disableUpButton();

        tabStrip = (PagerSlidingTabStrip) findViewById(R.id.tab_strip);
        pager = (ViewPager) findViewById(R.id.pager);
        adapter = new SeafileTabsAdapter(getSupportFragmentManager());
        pager.setAdapter(adapter);
        tabStrip.setViewPager(pager);

        mTabsLinearLayout = ((LinearLayout) tabStrip.getChildAt(0));
        for (int i = 0; i < mTabsLinearLayout.getChildCount(); i++) {
            TextView tv = (TextView) mTabsLinearLayout.getChildAt(i);

            if (i == currentPosition) {
                tv.setTextColor(getResources().getColor(R.color.fancy_orange));
                setUpButtonTitleOnSlideTabs(i);
            } else {
                tv.setTextColor(getResources().getColor(R.color.fancy_gray));
            }
        }

        tabStrip.setOnPageChangeListener(new ViewPager.OnPageChangeListener() {

            @Override
            public void onPageSelected(final int position) {
                currentPosition = position;
                supportInvalidateOptionsMenu();
                if (currentPosition != 0) {
                    disableUpButton();
                } else if (navContext.inRepo()) {
                    enableUpButton();
                }

                for(int i=0; i < mTabsLinearLayout.getChildCount(); i++){
                    TextView tv = (TextView) mTabsLinearLayout.getChildAt(i);
                    if(i == position){
                        setUpButtonTitleOnSlideTabs(i);
                        tv.setTextColor(getResources().getColor(R.color.fancy_orange));
                    } else {
                        tv.setTextColor(getResources().getColor(R.color.fancy_gray));
                    }
                }
            }

            @Override
            public void onPageScrollStateChanged(int arg0) {
                // TODO Auto-generated method stub
            }

            @Override
            public void onPageScrolled(int arg0, float arg1, int arg2) {
                // TODO Auto-generated method stub
            }
        });

        if (savedInstanceState != null) {
            Log.d(DEBUG_TAG, "savedInstanceState is not null");
            fetchFileDialog = (FetchFileDialog)
                    getSupportFragmentManager().findFragmentByTag(OPEN_FILE_DIALOG_FRAGMENT_TAG);

            appChoiceDialog = (AppChoiceDialog)
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
            if (repoID != null) {
                navContext.setRepoID(repoID);
                navContext.setRepoName(repoName);
                navContext.setDir(path, dirID);
            }
        }

        String repoID = intent.getStringExtra("repoID");
        String repoName = intent.getStringExtra("repoName");
        String path = intent.getStringExtra("path");
        String dirID = intent.getStringExtra("dirID");
        if (repoID != null) {
            navContext.setRepoID(repoID);
            navContext.setRepoName(repoName);
            navContext.setDir(path, dirID);
        }

        // enable ActionBar app icon to behave as action back
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

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
    }

    private void requestServerInfo() {
        if(!checkServerProEdition()) {
            // hide Activity tab
            adapter.hideActivityTab();
            tabStrip.notifyDataSetChanged();
            adapter.notifyDataSetChanged();
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
            if (serverInfo == null) {
                if (err != null)
                    ToastUtils.show(BrowserActivity.this, err.getMessage());
                return;
            }

            if (serverInfo.proEdition()) {
                // show Activity tab
                adapter.unHideActivityTab();
                adapter.notifyDataSetChanged();
                tabStrip.notifyDataSetChanged();
                // highlight font color of the active tab
                for (int i = 0; i < mTabsLinearLayout.getChildCount(); i++) {
                    TextView tv = (TextView) mTabsLinearLayout.getChildAt(i);

                    if (i == currentPosition) {
                        tv.setTextColor(getResources().getColor(R.color.fancy_orange));
                        setUpButtonTitleOnSlideTabs(i);
                    } else {
                        tv.setTextColor(getResources().getColor(R.color.fancy_gray));
                    }
                }
            }

            if (serverInfo.searchEnabled()) {
                // show search menu
                if (menuSearch != null)
                    menuSearch.setVisible(true);
            }

            serverInfo.setUrl(account.getServer());
            saveServerInfo(serverInfo);
        }
    }

    private void saveServerInfo(ServerInfo serverInfo) {
        AccountDBHelper.getDatabaseHelper(this).saveServerInfo(serverInfo);
    }

    /**
     * check if server is pro edition
     *
     * @return
     *          true, if server is pro edition
     *          false, otherwise.
     */
    private boolean checkServerProEdition() {
        if (account.getServer() == null)
            return false;

        ServerInfo serverInfo = AccountDBHelper.getDatabaseHelper(this).getServerInfo(account.getServer());
        if (serverInfo == null)
            return false;

        return serverInfo.proEdition();
    }

    /**
     * check if server supports searching feature
     *
     * @return
     *          true, if search enabled
     *          false, otherwise.
     */
    private boolean checkSearchEnabled() {
        if (account.getServer() == null)
            return false;

        ServerInfo serverInfo = AccountDBHelper.getDatabaseHelper(this).getServerInfo(account.getServer());
        if (serverInfo == null)
            return false;

        return serverInfo.searchEnabled();
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

    @Override
    protected void onResume() {
        super.onResume();

        SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this);
        boolean isUploadStart = settings.getBoolean(SettingsManager.CAMERA_UPLOAD_SWITCH_KEY, false);
        if (!isUploadStart) {
            return;
        }
        if (isCameraUploadServiceRunning("com.seafile.seadroid2.sync.CameraUploadService")) {
            Log.d(DEBUG_TAG, "service running...");
            // even camera upload service is running, still can`t return.
            // because running state does not guarantee UploadFragment to only show uploading progress, it may show unexpected info like "no upload tasks".
            // 1. when OS under memory pressure, nothing upload even service state is running
            // 2. OS will restore upload, of course service state is running as well

            // return;
        }
        Log.d(DEBUG_TAG, "start service explicitly on Resume method");
        Intent cameraUploadIntent = new Intent(this, CameraUploadService.class);
        startService(cameraUploadIntent);
    }

    private boolean isCameraUploadServiceRunning(String serviceClassName) {
        final ActivityManager activityManager =
                (ActivityManager) getSystemService(Context.ACTIVITY_SERVICE);
        final List<RunningServiceInfo> services = activityManager
                .getRunningServices(Integer.MAX_VALUE);

        for (RunningServiceInfo runningServiceInfo : services) {
            if (runningServiceInfo.service.getClassName().equals(
                    serviceClassName)) {
                return true;
            }
        }
        return false;
    }

    public int getCurrentPosition() {
        return currentPosition;
    }

    public void setCurrentPosition(int currentPosition) {
        this.currentPosition = currentPosition;
        pager.setCurrentItem(currentPosition);
    }

    public Fragment getFragment(int index) {
        return getSupportFragmentManager().findFragmentByTag(makeFragmentName(index));
    }

    private String makeFragmentName(int index) {
        return "android:switcher:" + R.id.pager + ":" + index;
    }

    public ReposFragment getReposFragment() {
        return (ReposFragment)getFragment(0);
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
                txService.addTaskToUploadQue(account,
                                            info.repoID,
                                            info.repoName,
                                            info.targetDir,
                                            info.localFilePath,
                                            info.isUpdate,
                                            info.isCopyToLocal);
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
    }

    @Override
    protected void onNewIntent(Intent intent) {
        Log.d(DEBUG_TAG, "onNewIntent");
        String server = intent.getStringExtra(AccountManager.SHARED_PREF_SERVER_KEY);
        String email = intent.getStringExtra(AccountManager.SHARED_PREF_EMAIL_KEY);

        // if the user started the Seadroid app from the Launcher, keep the old Activity
        final String intentAction = intent.getAction();
        if (intent.hasCategory(Intent.CATEGORY_LAUNCHER)
                && intentAction != null
                && intentAction.equals(Intent.ACTION_MAIN)) {
            return;
        }

        Account selectedAccount = new Account(server, email);
        if (!account.equals(selectedAccount)) {
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
        //outState.putInt("tab", getSupportActionBar().getSelectedNavigationIndex());
        if (navContext.getRepoID() != null) {
            outState.putString("repoID", navContext.getRepoID());
            outState.putString("repoName", navContext.getRepoName());
            outState.putString("path", navContext.getDirPath());
            outState.putString("dirID", navContext.getDirID());
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getSupportMenuInflater();
        inflater.inflate(R.menu.browser_menu, menu);
        overFlowMenu = menu;
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        menuSearch = menu.findItem(R.id.search);
        MenuItem menuSort = menu.findItem(R.id.sort);
        MenuItem menuAdd = menu.findItem(R.id.add);
        MenuItem menuEdit = menu.findItem(R.id.edit);

        // Libraries Tab
        if (currentPosition == 0) {
            if (navContext.inRepo()) {
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
                menuAdd.setVisible(false);
                menuEdit.setVisible(false);
            }

            menuSort.setVisible(true);
        } else {
            menuSort.setVisible(false);
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
    public boolean onOptionsItemSelected(MenuItem item) {

        switch (item.getItemId()) {
        case android.R.id.home:
            if (navContext.inRepo()) {
                onBackPressed();
            }
            return true;
        case R.id.sort:
            showSortFilesDialog();
            return true;
        case R.id.search:
            Intent searchIntent = new Intent(this, SearchActivity.class);
            startActivity(searchIntent);
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
        case R.id.refresh:
            if (!Utils.isNetworkOn()) {
                ToastUtils.show(this, R.string.network_down);
                return true;
            }
            if (currentPosition == 0) {
                if (navContext.inRepo()) {
                    SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                    if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
                        String password = DataManager.getRepoPassword(repo.id);
                        showPasswordDialog(repo.name, repo.id,
                            new TaskDialog.TaskDialogListener() {
                                @Override
                                public void onTaskSuccess() {
                                    getReposFragment().refresh();
                                }
                            } , password);

                        return true;
                    }
                }

                getReposFragment().refresh();
            } else if (currentPosition == 2) {
                getActivitiesFragment().refreshView();
            } else if (currentPosition == 1) {
                getStarredFragment().refresh();
            }
            return true;
        case R.id.edit:
            // start action mode for selecting multiple files/folders

            if (!Utils.isNetworkOn()) {
                ToastUtils.show(this, R.string.network_down);
                return true;
            }
            if (currentPosition == 0) {
                if (navContext.inRepo()) {
                    SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                    if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
                        String password = DataManager.getRepoPassword(repo.id);
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
        new SortFilesDialog().show(getSupportFragmentManager(), "sort files");
    }

    public class SortFilesDialog extends DialogFragment {
        @Override
        public Dialog onCreateDialog(Bundle savedInstanceState) {
            ArrayAdapter<CharSequence> adapter = new ArrayAdapter<CharSequence>(
                    getActivity(),
                    R.layout.list_item_single_choice,
                    android.R.id.text1,
                    getResources().getStringArray(R.array.sort_files_options_array));

            SeafileStyleDialogBuilder builder =
                    (SeafileStyleDialogBuilder) new SeafileStyleDialogBuilder(getActivity())
                            .setTitle(getString(R.string.sort_files))
                            .setSingleChoiceItems(adapter,
                                    calculateCheckedItem(),
                                    new AdapterView.OnItemClickListener() {
                                        @Override
                                        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
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
                                            dismiss();
                                        }
                                    });
            return builder.show();
        }
    }

    /**
     * Sort files by type and order
     *
     * @param type
     */
    private void sortFiles(final int type, final int order) {
        if (currentPosition == 0) {
            if (navContext.inRepo()) {
                SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
                    String password = DataManager.getRepoPassword(repo.id);
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

    private int calculateCheckedItem() {
        switch (SettingsManager.instance().getSortFilesTypePref()) {
            case SeafItemAdapter.SORT_BY_NAME:
                if (SettingsManager.instance().getSortFilesOrderPref()
                        == SeafItemAdapter.SORT_ORDER_ASCENDING)
                    return 0;
                else if (SettingsManager.instance().getSortFilesOrderPref()
                        == SeafItemAdapter.SORT_ORDER_DESCENDING)
                    return 1;

                break;
            case SeafItemAdapter.SORT_BY_LAST_MODIFIED_TIME:
                if (SettingsManager.instance().getSortFilesOrderPref()
                        == SeafItemAdapter.SORT_ORDER_ASCENDING)
                    return 2;
                else if (SettingsManager.instance().getSortFilesOrderPref()
                        == SeafItemAdapter.SORT_ORDER_DESCENDING)
                    return 3;

                break;
        }
        return 0;
    }

    /**
     * add new file/files
     */
    private void addFile() {
        final SeafileStyleDialogBuilder builder = new SeafileStyleDialogBuilder(this);
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
            ToastUtils.show(this, R.string.library_read_only);
            return;
        }

        final NewDirDialog dialog = new NewDirDialog();
        dialog.init(navContext.getRepoID(), navContext.getDirPath(), account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                ToastUtils.show(BrowserActivity.this, "Sucessfully created folder " + dialog.getNewDirName());
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == 0 && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    private void showNewFileDialog() {
        if (!hasRepoWritePermission()) {
            ToastUtils.show(this, R.string.library_read_only);
            return;
        }

        final NewFileDialog dialog = new NewFileDialog();
        dialog.init(navContext.getRepoID(), navContext.getDirPath(), account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                ToastUtils.show(BrowserActivity.this, "Sucessfully created file " + dialog.getNewFileName());
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == 0 && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    public void setRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.TRUE);
    }

    public void unsetRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.FALSE);
    }

    private String strImgPath;

    private void CameraTakePhoto() {
        Intent imageCaptureIntent = new Intent("android.media.action.IMAGE_CAPTURE");

        String strImgDirPath = DataManager.getExternalRootDirectory() + "/myPhotos/";
        String fileName = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()) + ".jpg";
        strImgPath = strImgDirPath + fileName;

        File ImgDir = new File(strImgDirPath);
        if (!ImgDir.exists()) {
            ImgDir.mkdirs();
        }
        File Img = new File(strImgDirPath, fileName);
        while (Img.exists()) {
            fileName = fileName + "(1)";
            strImgPath = strImgDirPath + fileName;
        }
        Uri photo = Uri.fromFile(Img);
        imageCaptureIntent.putExtra(MediaStore.EXTRA_OUTPUT, photo);
        startActivityForResult(imageCaptureIntent, TAKE_PHOTO_REQUEST);
    }

    public void enableUpButton() {
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setLogo(getResources().getDrawable(R.color.transparent));
    }

    public void disableUpButton() {
        getSupportActionBar().setDisplayHomeAsUpEnabled(false);
        getSupportActionBar().setLogo(R.drawable.icon);
    }

    public void setUpButtonTitle(String title){
        getSupportActionBar().setTitle(title);
    }

    /**
     * update up button title when sliding among tabs
     *
     * @param position
     */
    private void setUpButtonTitleOnSlideTabs(int position) {
        if (navContext == null)
            return;

        if (position == 0) {
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

    /***********  Start other activity  ***************/

    public static final int PICK_FILES_REQUEST = 1;
    public static final int PICK_PHOTOS_VIDEOS_REQUEST = 2;
    public static final int PICK_FILE_REQUEST = 3;
    public static final int TAKE_PHOTO_REQUEST = 4;
    public static final int CHOOSE_COPY_MOVE_DEST_REQUEST = 5;
    public static final int DOWNLOAD_FILE_REQUEST = 6;

    public boolean hasRepoWritePermission() {
        SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
        if (repo == null) {
            return false;
        }

        if (repo.permission.indexOf('w') == -1) {
            return false;
        }
        return true;
    }

    void pickFile() {
        if (!hasRepoWritePermission()) {
            ToastUtils.show(this, R.string.library_read_only);
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
                ToastUtils.show(this, getString(R.string.added_to_upload_tasks));
                for (String path : paths) {
                    addUploadTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), path);
                }
            }
            break;
        case PICK_PHOTOS_VIDEOS_REQUEST:
            if (resultCode == RESULT_OK) {
                ArrayList<String> paths = data.getStringArrayListExtra("photos");
                if (paths == null)
                    return;
                ToastUtils.show(this, getString(R.string.added_to_upload_tasks));
                for (String path : paths) {
                    addUploadTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), path);
                }
            }
            break;
        case PICK_FILE_REQUEST:
            if (resultCode == RESULT_OK) {
                if (!Utils.isNetworkOn()) {
                    ToastUtils.show(this, R.string.network_down);
                    return;
                }

                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                    List<Uri> uriList = UtilsJellyBean.extractUriListFromIntent(data);
                    if (uriList.size() > 0) {
                        ConcurrentAsyncTask.execute(new SAFLoadRemoteFileTask(), uriList.toArray(new Uri[]{}));
                    } else {
                        ToastUtils.show(BrowserActivity.this, R.string.saf_upload_path_not_available);
                    }
                } else {
                    Uri uri = data.getData();
                    if (uri != null) {
                        ConcurrentAsyncTask.execute(new SAFLoadRemoteFileTask(), uri);
                    } else {
                        ToastUtils.show(BrowserActivity.this, R.string.saf_upload_path_not_available);
                    }
                }
            }
            break;
        case CHOOSE_COPY_MOVE_DEST_REQUEST:
            if (resultCode == RESULT_OK) {
                if (!Utils.isNetworkOn()) {
                    ToastUtils.show(this, R.string.network_down);
                    return;
                }

                copyMoveIntent = data;
            }
            break;
        case TAKE_PHOTO_REQUEST:
            if (resultCode == RESULT_OK) {
                ToastUtils.show(this, getString(R.string.take_photo_successfully));
                if (!Utils.isNetworkOn()) {
                    ToastUtils.show(this, R.string.network_down);
                    return;
                }

                if(strImgPath == null) {
                    ToastUtils.show(this, "Unable to upload, no path available");
                    Log.i(DEBUG_TAG, "Pick file request did not return a path");
                    return;
                }
                ToastUtils.show(this, getString(R.string.added_to_upload_tasks));
                addUploadTask(navContext.getRepoID(), navContext.getRepoName(), navContext.getDirPath(), strImgPath);

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
                File tempDir = new File(DataManager.getExternalTempDirectory(), "saf_temp" + "/" + "upload-"+System.currentTimeMillis());
                File tempFile = new File(tempDir, Utils.getFilenamefromUri(BrowserActivity.this, uri));

                // Log.d(DEBUG_TAG, "Uploading file from uri: " + uri);

                InputStream in = null;
                OutputStream out = null;

                try {
                    tempDir.mkdirs();

                    if (!tempFile.createNewFile()) {
                        throw new RuntimeException("could not create temporary file");
                    }

                    in = getContentResolver().openInputStream(uri);
                    out = new FileOutputStream(tempFile);
                    IOUtils.copy(in, out);

                } catch (IOException e) {
                    Log.d(DEBUG_TAG, "Could not open requested document", e);
                    tempFile = null;
                } catch (RuntimeException e) {
                    Log.d(DEBUG_TAG, "Could not open requested document", e);
                    tempFile = null;
                } finally {
                    IOUtils.closeQuietly(in);
                    IOUtils.closeQuietly(out);
                }
                fileList.add(tempFile);
            }
            return fileList.toArray(new File[]{});
        }

        @Override
        protected void onPostExecute(File... fileList) {
            ArrayList<Integer> taskIDList = Lists.newArrayList();

            for (File file: fileList) {
                if (file == null) {
                    ToastUtils.show(BrowserActivity.this, R.string.saf_upload_path_not_available);
                } else {
                    int taskID = addUploadTask(navContext.getRepoID(),
                            navContext.getRepoName(), navContext.getDirPath(), file.getAbsolutePath());
                    taskIDList.add(taskID);
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

    public void onItemSelected() {
        // update contextual action bar (CAB) title
        getReposFragment().updateContextualActionBar();
    }

    /***************  Navigation *************/

    @Override
    public void onFileSelected(SeafDirent dirent) {
        final String fileName= dirent.name;
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

        startFileActivity(repoName, repoID, filePath);
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
            ToastUtils.show(this, R.string.network_down);
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
                    ToastUtils.show(BrowserActivity.this, R.string.transfer_list_network_error);
                }
                return;
            }

            if (fileCount == 0)
                ToastUtils.show(BrowserActivity.this, R.string.transfer_download_no_task);
            else {
                ToastUtils.show(BrowserActivity.this, getResources().getQuantityString(R.plurals.transfer_download_started, fileCount, fileCount));
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

    private void startFileActivity(String repoName, String repoID, String filePath) {
        final int taskID = txService.addDownloadTask(account, repoName, repoID, filePath);
        Intent intent = new Intent(this, FileActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", account);
        intent.putExtra("taskID", taskID);
        startActivityForResult(intent, DOWNLOAD_FILE_REQUEST);
    }

    @Override
    public void onStarredFileSelected(SeafStarredFile starredFile) {

        final String repoID = starredFile.getRepoID();
        final SeafRepo repo = dataManager.getCachedRepoByID(repoID);
        final String repoName = repo.getName();
        final String filePath = starredFile.getPath();
        final String dirPath = Utils.getParentPath(filePath);

        // Encrypted repo doesn\`t support gallery,
        // because pic thumbnail under encrypted repo was not supported at the server side
        if (Utils.isViewableImage(starredFile.getTitle())
                && repo != null && !repo.encrypted) {
            WidgetUtils.startGalleryActivity(this, repoName, repoID, dirPath, starredFile.getTitle(), account);
            return;
        }

        final File localFile = dataManager.getLocalCachedFile(repoName, repoID, filePath, null);
        if (localFile != null) {
            WidgetUtils.showFile(this, localFile);
            return;
        }

        startFileActivity(repoName, repoID, filePath);
    }

    @Override
    public void onBackPressed() {
        if (getSupportFragmentManager().getBackStackEntryCount() != 0) {
            getSupportFragmentManager().popBackStack();
            return;
        }

        if (currentSelectedItem == FILES_VIEW && currentPosition == 0) {
            if (navContext.inRepo()) {
                if (navContext.isRepoRoot()) {
                    navContext.setRepoID(null);
                    getSupportActionBar().setTitle(R.string.app_name);
                } else {
                    String parentPath = Utils.getParentPath(navContext
                            .getDirPath());
                    navContext.setDir(parentPath, null);
                    if (parentPath.equals(ACTIONBAR_PARENT_PATH)) {
                        getSupportActionBar().setTitle(navContext.getRepoName());
                    }else {
                        getSupportActionBar().setTitle(parentPath.substring(parentPath.lastIndexOf(ACTIONBAR_PARENT_PATH) + 1));
                    }
                }
                getReposFragment().refreshView();

            } else
                super.onBackPressed();
        } else {
            super.onBackPressed();
        }
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
    public void exportFile(String fileName) {
        String repoName = navContext.getRepoName();
        String repoID = navContext.getRepoID();
        String dirPath = navContext.getDirPath();
        String fullPath = Utils.pathJoin(dirPath, fileName);
        chooseExportApp(repoName, repoID, fullPath);
    }

    private void chooseExportApp(final String repoName, final String repoID, final String path) {
        final File file = dataManager.getLocalRepoFile(repoName, repoID, path);
        Uri uri = Uri.fromFile(file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        // Get a list of apps
        List<ResolveInfo> infos = Utils.getAppsByIntent(sendIntent);

        if (infos.isEmpty()) {
            ToastUtils.show(this, R.string.no_app_available);
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
                fetchFileAndExport(appInfo, sendIntent, repoName, repoID, path);
            }

        });
        dialog.show(getSupportFragmentManager(), CHOOSE_APP_DIALOG_FRAGMENT_TAG);
    }

    private void fetchFileAndExport(final ResolveInfo appInfo, final Intent intent,
                                    final String repoName, final String repoID, final String path) {

        fetchFileDialog = new FetchFileDialog();
        fetchFileDialog.init(repoName, repoID, path, new FetchFileDialog.FetchFileListener() {
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

    /**
     * Share a file. Generating a file share link and send the link to someone
     * through some app.
     * @param repoID
     * @param path
     */
    public void shareFile(String repoID, String path) {
        WidgetUtils.chooseShareApp(this, repoID, path, false, account);
    }

    public void shareDir(String repoID, String path) {
        WidgetUtils.chooseShareApp(this, repoID, path, true, account);
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
                ToastUtils.show(BrowserActivity.this, R.string.rename_successful);
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == 0 && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
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
                ToastUtils.show(BrowserActivity.this, R.string.delete_successful);
                ReposFragment reposFragment = getReposFragment();
                if (currentPosition == 0 && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
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
            ToastUtils.show(this, copyMoveContext.isCopy()
                    ? R.string.cannot_copy_folder_to_subfolder
                    : R.string.cannot_move_folder_to_subfolder);
            return;
        }
        final CopyMoveDialog dialog = new CopyMoveDialog();
        dialog.init(account, copyMoveContext);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                ToastUtils.show(BrowserActivity.this, copyMoveContext.isCopy()
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
                    if (currentPosition == 0 && reposFragment != null) {
                        reposFragment.refreshView();
                    }
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
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

        if (err != null
                && err.getCode() == SeafConnection.HTTP_STATUS_REPO_PASSWORD_REQUIRED) {
            if (currentPosition == 0
                    && repoID.equals(navContext.getRepoID())
                    && Utils.getParentPath(path)
                            .equals(navContext.getDirPath())) {
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

        ToastUtils.show(this, getString(R.string.download_failed));
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
        if (currentPosition == 0
                && repoID.equals(navContext.getRepoID())
                && dir.equals(navContext.getDirPath())) {
            getReposFragment().refreshView(true);
            String verb = getString(info.isUpdate ? R.string.updated : R.string.uploaded);
            ToastUtils.show(this, verb + " " + Utils.fileNameFromPath(info.localFilePath));
        }
    }

    private int intShowErrorTime;
    private void onFileUploadFailed(int taskID) {
        if (++ intShowErrorTime <= 1)
            ToastUtils.show(this, getString(R.string.upload_failed));
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
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                ToastUtils.show(BrowserActivity.this, R.string.delete_successful);
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
        dialog.show(getSupportFragmentManager(), "DialogFragment");
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
            ToastUtils.show(this, R.string.network_down);
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
                ToastUtils.show(BrowserActivity.this, R.string.transfer_list_network_error);
                return;
            }

            if (fileCount == 0)
                ToastUtils.show(BrowserActivity.this, R.string.transfer_download_no_task);
            else {
                ToastUtils.show(BrowserActivity.this,
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

}
