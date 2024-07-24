package com.seafile.seadroid2.ui.main;

import android.Manifest;
import android.app.Dialog;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.appcompat.widget.Toolbar;
import androidx.fragment.app.DialogFragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.viewpager2.widget.ViewPager2;
import androidx.work.Data;
import androidx.work.WorkInfo;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.navigation.NavigationBarView;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.google.firebase.crashlytics.FirebaseCrashlytics;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.ActivityMainBinding;
import com.seafile.seadroid2.framework.data.ServerInfo;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.data.db.entities.FileTransferEntity;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.util.PermissionUtil;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.TakeCameras;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.framework.file_monitor.FileSyncService;
import com.seafile.seadroid2.framework.worker.upload.FolderBackupScannerWorker;
import com.seafile.seadroid2.framework.worker.SupportWorkManager;
import com.seafile.seadroid2.framework.worker.TransferEvent;
import com.seafile.seadroid2.framework.worker.TransferWorker;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.dialog_fragment.NewDirFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.NewRepoDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.SortFilesDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnSortItemClickListener;
import com.seafile.seadroid2.ui.repo.RepoQuickFragment;
import com.seafile.seadroid2.ui.search.Search2Activity;
import com.seafile.seadroid2.ui.settings.SettingsActivity;
import com.seafile.seadroid2.ui.transfer_list.TransferActivity;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.Map;

import io.reactivex.functions.Consumer;
import kotlin.Pair;

public class MainActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {

    private ActivityMainBinding binding;
    public static final int INDEX_LIBRARY_TAB = 0;

    private Intent monitorIntent;
    private FileSyncService syncService;
    private MainViewModel mainViewModel;

    private Menu overFlowMenu;
    private MenuItem menuSearch;

    private Account curAccount;

    private NavContext getNavContext() {
        return mainViewModel.getNavContext();
    }

    private RepoQuickFragment getReposFragment() {
        return (RepoQuickFragment) mainViewModel.getFragments().get(0);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (!isTaskRoot()) {
            final Intent intent = getIntent();
            final String intentAction = getIntent().getAction();
            if (intent.hasCategory(Intent.CATEGORY_LAUNCHER) && intentAction != null && intentAction.equals(Intent.ACTION_MAIN)) {
                finish();
                return;
            }
        }

        mainViewModel = new ViewModelProvider(this).get(MainViewModel.class);

        binding = ActivityMainBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        setSupportActionBar(getActionBarToolbar());
        // enable ActionBar app icon to behave as action back
        enableUpButton(false);

        curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (curAccount == null || !curAccount.hasValidToken()) {
            finishAndStartAccountsActivity();
            return;
        }

        initFireBase();
        initOnBackPressedDispatcher();
        initBottomNavigation();
        initViewPager();

        initViewModel();
        initWorkerListener();

        //service
        bindService();

        requestServerInfo(true);

        //job
//        Utils.startCameraSyncJob(this);

//        syncCamera();

    }

    private void initFireBase() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account != null) {
            FirebaseCrashlytics crashlytics = FirebaseCrashlytics.getInstance();
            crashlytics.setUserId(account.getSignature());

            FirebaseAnalytics analytics = FirebaseAnalytics.getInstance(this);
            analytics.setUserId(account.getSignature());
        }

    }

    private void initOnBackPressedDispatcher() {
        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (binding.pager.getCurrentItem() == INDEX_LIBRARY_TAB) {
                    RepoQuickFragment fragment = (RepoQuickFragment) mainViewModel.getFragments().get(0);
                    boolean canBack = fragment.backTo();
                    if (canBack) {

                    } else {
                        finish();
                    }
                } else {
                    finish();
                }
            }
        });
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);


        // handle notification permission on API level >= 33
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            // request notification permission first and then prompt for storage permissions
            // storage permissions handled in onRequestPermissionsResult

            notificationPermissionLauncher.launch(Manifest.permission.POST_NOTIFICATIONS);
            return;
        }

        PermissionUtil.requestExternalStoragePermission(this, multiplePermissionLauncher, manageStoragePermissionLauncher);
    }


    public static void startThis(Context context) {
        Intent i = new Intent(context, MainActivity.class);
        i.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        context.startActivity(i);
    }

    @Override
    public void onRestart() {
        super.onRestart();
        SLogs.d("onRestart");

        if (curAccount == null
                || !curAccount.equals(SupportAccountManager.getInstance().getCurrentAccount())
                || !SupportAccountManager.getInstance().getCurrentAccount().getToken().equals(curAccount.getToken())) {
            finishAndStartAccountsActivity();
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
        SLogs.d("onStop");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();

        BackgroundJobManagerImpl.getInstance().cancelAllJobs();
    }

    public static void navToThis(Context context, String repo_id, String repo_name, String path, boolean is_dir) {
        Intent intent = new Intent(context, MainActivity.class);
        intent.putExtra("repo_id", repo_id);
        intent.putExtra("repo_name", repo_name);
        intent.putExtra("path", path);
        intent.putExtra("is_dir", is_dir);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        context.startActivity(intent);
    }

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        SLogs.d("onNewIntent");

        // if the user started the Seadroid app from the Launcher, keep the old Activity
        final String intentAction = intent.getAction();
        if (intent.hasCategory(Intent.CATEGORY_LAUNCHER)
                && intentAction != null
                && intentAction.equals(Intent.ACTION_MAIN)) {
            return;
        }

        Account selectedAccount = SupportAccountManager.getInstance().getCurrentAccount();

        SLogs.d("Current account: " + curAccount);
        if (selectedAccount == null
                || !curAccount.equals(selectedAccount)
                || !curAccount.getToken().equals(selectedAccount.getToken())) {
            SLogs.d("Account switched, restarting activity.");
            finishAndStartAccountsActivity();
            return;
        }

        String repoId = intent.getStringExtra("repo_id");
        String repoName = intent.getStringExtra("repo_name");
        String path = intent.getStringExtra("path");
        boolean isDir = intent.getBooleanExtra("is_dir", false);

        if (TextUtils.isEmpty(repoId) || TextUtils.isEmpty(path)) {
            return;
        }

        //
        navToPath(repoId, path, isDir);
    }

    private void navToPath(String repoId, String path, boolean isDir) {
        if (!isDir) {
            path = Utils.getParentPath(path);
        }

        String finalPath = path;
        mainViewModel.requestRepoModel(repoId, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (repoModel.encrypted) {
                    mainViewModel.getEncCacheDB(repoId, new Consumer<EncKeyCacheEntity>() {
                        @Override
                        public void accept(EncKeyCacheEntity encKeyCacheEntity) throws Exception {
                            //check encrypt and decrypt_expire_time
                            long now = TimeUtils.getNowMills();
                            if (encKeyCacheEntity == null || (repoModel.encrypted && now > encKeyCacheEntity.expire_time_long)) {
                                // expired
                                showPasswordDialog(repoModel, finalPath);
                            } else {
                                getNavContext().navToPath(repoModel, finalPath);
                                binding.pager.setCurrentItem(0);
                                getReposFragment().loadData();
                                refreshToolbarTitle();
                            }
                        }
                    });

                } else {
                    getNavContext().navToPath(repoModel, finalPath);
                    binding.pager.setCurrentItem(0);
                    getReposFragment().loadData();
                    refreshToolbarTitle();
                }
            }
        });
    }

    private void initBottomNavigation() {
        binding.navBottomView.setItemHorizontalTranslationEnabled(true);
        binding.navBottomView.setOnItemSelectedListener(new NavigationBarView.OnItemSelectedListener() {
            @Override
            public boolean onNavigationItemSelected(@NonNull MenuItem menuItem) {
                onBottomNavigationSelected(menuItem);
                return true;
            }
        });
    }

    private void onBottomNavigationSelected(MenuItem menuItem) {
        //Invalidate menu
        supportInvalidateOptionsMenu();

        //tab
        if (menuItem.getItemId() == R.id.tabs_library) {
            if (getNavContext().isInRepo()) {
                setActionbarTitle(getNavContext().getNameInCurPath());
                enableUpButton(true);
            } else {
                enableUpButton(false);
                setActionbarTitle(getString(R.string.tabs_library));
            }

            binding.pager.setCurrentItem(0);
        } else if (menuItem.getItemId() == R.id.tabs_starred) {
            enableUpButton(false);
            setActionbarTitle(getString(R.string.tabs_starred));

            binding.pager.setCurrentItem(1);
        } else if (menuItem.getItemId() == R.id.tabs_activity) {
            enableUpButton(false);
            setActionbarTitle(getString(R.string.tabs_activity));

            binding.pager.setCurrentItem(2);
        }
    }

    private void initViewPager() {
        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(mainViewModel.getFragments());
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setOffscreenPageLimit(3);
        binding.pager.setUserInputEnabled(true);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);
                if (0 == position) {
                    binding.navBottomView.setSelectedItemId(R.id.tabs_library);
                } else if (1 == position) {
                    binding.navBottomView.setSelectedItemId(R.id.tabs_starred);
                } else if (2 == position) {
                    binding.navBottomView.setSelectedItemId(R.id.tabs_activity);
                }
            }
        });
    }

    private void initWorkerListener() {
        SupportWorkManager.getWorkManager()
                .getWorkInfoByIdLiveData(FolderBackupScannerWorker.UID)
                .observe(this, new Observer<WorkInfo>() {
                    @Override
                    public void onChanged(WorkInfo workInfo) {
                        if (null == workInfo) {
                            return;
                        }

                        Data data = workInfo.getOutputData();
                        String event = data.getString(TransferWorker.KEY_DATA_EVENT);

                        if (TextUtils.isEmpty(event)) {
                            return;
                        }

                        if (TransferEvent.EVENT_SCAN_END.equals(event)) {
                            if (syncService != null) {
                                syncService.startFolderMonitor();
                            }
                        }
                    }
                });
    }

    private void initViewModel() {
        mainViewModel.getOnForceRefreshRepoListLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (binding.pager.getCurrentItem() == 0) {
                    RepoQuickFragment fragment = (RepoQuickFragment) mainViewModel.getFragments().get(0);
                    fragment.clearExpireRefreshMap();
                    fragment.loadData();
                }
            }
        });


        mainViewModel.getServerInfoLiveData().observe(this, new Observer<ServerInfo>() {
            @Override
            public void onChanged(ServerInfo serverInfo) {
                requestServerInfo(false);
            }
        });

        mainViewModel.getOnNavContextChangeListenerLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                refreshActionbar();
            }
        });
    }

    private void refreshToolbarTitle() {
        if (getNavContext().isInRepoList()) {
            getActionBarToolbar().setTitle(R.string.libraries);
        } else if (getNavContext().isInRepoRoot()) {
            getActionBarToolbar().setTitle(getNavContext().getRepoModel().repo_name);
        } else {
            String toolbarTitle = getNavContext().getLastPathName();
            getActionBarToolbar().setTitle(toolbarTitle);
        }
    }

    /////////////////////service
    private void bindService() {
        // restart service should it have been stopped for some reason
//        Intent mediaObserver = new Intent(this, MediaObserverService.class);
//        startService(mediaObserver);
//        SLogs.d("start MediaObserverService");
//
//        Intent dIntent = new Intent(this, FolderBackupService.class);
//        startService(dIntent);
//        SLogs.d("start FolderBackupService");
//
//        Intent dirIntent = new Intent(this, FolderBackupService.class);
//        bindService(dirIntent, folderBackupConnection, Context.BIND_AUTO_CREATE);
//        SLogs.d("try bind FolderBackupService");
//
//        Intent txIntent = new Intent(this, TransferService.class);
//        startService(txIntent);
//        SLogs.d("start TransferService");
//
//        // bind transfer service
//        Intent bIntent = new Intent(this, TransferService.class);
//        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
//        SLogs.d("try bind TransferService");
//
//        monitorIntent = new Intent(this, FileMonitorService.class);
//        startService(monitorIntent);
//        SLogs.d("start FileMonitorService");

        Intent syncIntent = new Intent(this, FileSyncService.class);
        bindService(syncIntent, syncConnection, Context.BIND_AUTO_CREATE);
    }


    private final ServiceConnection syncConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            FileSyncService.FileSyncBinder binder = (FileSyncService.FileSyncBinder) service;
            syncService = binder.getService();
            SLogs.d("bond FileSyncService");
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            syncService = null;
            SLogs.d("FileSyncService disconnected");
        }
    };

    //////////////////////////check server info
    private void requestServerInfo(boolean loadFromNet) {
        if (!checkServerProEdition()) {
            binding.navBottomView.getMenu().removeItem(R.id.tabs_activity);

            // hide Activity tab
            ViewPager2Adapter adapter = (ViewPager2Adapter) binding.pager.getAdapter();
            if (adapter != null && adapter.getItemCount() > 2) {
                long hashCode = mainViewModel.getFragments().get(2).hashCode();
                if (adapter.containsItem(hashCode)) {
                    adapter.removeFragment(2);
                    adapter.notifyItemRemoved(2);
                }
            }
        }

        if (!checkSearchEnabled()) {
            // hide search menu
            if (menuSearch != null)
                menuSearch.setVisible(false);
        }

        if (loadFromNet) {
            mainViewModel.getServerInfo();
        }
    }

    /**
     * check if server is pro edition
     *
     * @return true, if server is pro edition
     * false, otherwise.
     */
    private boolean checkServerProEdition() {
        if (curAccount == null)
            return false;

        ServerInfo serverInfo = SupportAccountManager.getInstance().getServerInfo(curAccount);
        return serverInfo.isProEdition();
    }

    /**
     * check if server supports searching feature
     *
     * @return true, if search enabled
     * false, otherwise.
     */
    private boolean checkSearchEnabled() {
        if (curAccount == null)
            return false;

        ServerInfo serverInfo = SupportAccountManager.getInstance().getServerInfo(curAccount);
        return serverInfo.isSearchEnabled();
    }


    private void finishAndStartAccountsActivity() {
        Intent newIntent = new Intent(this, AccountsActivity.class);
        newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        finish();
        startActivity(newIntent);
    }


    ////////////////////////menu////////////////////////
    private void refreshActionbar() {
        if (getNavContext().isInRepo()) {

            //refresh back btn state
            enableUpButton(true);

            setActionbarTitle(getNavContext().getNameInCurPath());
        } else {
            enableUpButton(false);

            setActionbarTitle(getString(R.string.tabs_library));
        }

        //refresh toolbar menu
        supportInvalidateOptionsMenu();
    }

    public void setActionbarTitle(String title) {
        if (getSupportActionBar() != null) {
            if (TextUtils.isEmpty(title)) {
                getSupportActionBar().setTitle(null);
            } else {
                getSupportActionBar().setTitle(title);
            }
        }

    }

    private void enableUpButton(boolean isEnable) {
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(isEnable);
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
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                if (getNavContext().isInRepo() && binding.pager.getCurrentItem() == INDEX_LIBRARY_TAB) {
                    getOnBackPressedDispatcher().onBackPressed();
                }
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        menuSearch = menu.findItem(R.id.search);
        MenuItem menuSort = menu.findItem(R.id.sort);
        MenuItem menuAdd = menu.findItem(R.id.add);
        MenuItem menuCreateRepo = menu.findItem(R.id.create_repo);
        MenuItem menuEdit = menu.findItem(R.id.edit);

        // Libraries Tab
        if (binding.pager.getCurrentItem() == INDEX_LIBRARY_TAB) {
            if (getNavContext().isInRepo()) {
                menuCreateRepo.setVisible(false);
                menuAdd.setVisible(true);
                menuEdit.setVisible(true);
                if (getNavContext().hasWritePermissionWithRepo()) {
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
//        if (!checkServerProEdition())
//            menuSearch.setVisible(false);

        return true;
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        if (item.getItemId() == R.id.sort) {
            showSortFilesDialog();
        } else if (item.getItemId() == R.id.search) {
            Intent searchIntent = new Intent(this, Search2Activity.class);
            startActivity(searchIntent);
        } else if (item.getItemId() == R.id.create_repo) {
            showNewRepoDialog();
        } else if (item.getItemId() == R.id.add) {
            showAddFileDialog();
        } else if (item.getItemId() == R.id.transfer_tasks) {
            Intent newIntent = new Intent(this, TransferActivity.class);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(newIntent);
        } else if (item.getItemId() == R.id.accounts) {
            Intent newIntent = new Intent(this, AccountsActivity.class);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(newIntent);
        } else if (item.getItemId() == R.id.edit) {
            if (binding.pager.getCurrentItem() == INDEX_LIBRARY_TAB) {
                getReposFragment().startContextualActionMode();
            }
        } else if (item.getItemId() == R.id.settings) {
            Intent settingsIntent = new Intent(MainActivity.this, SettingsActivity.class);
            settingsIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(settingsIntent);
        } else {
            return super.onOptionsItemSelected(item);
        }
        return true;
    }

    @Override
    public boolean onKeyUp(int keycode, KeyEvent e) {
        if (keycode == KeyEvent.KEYCODE_MENU) {
            if (overFlowMenu != null) {
                overFlowMenu.performIdentifierAction(R.id.menu_overflow, 0);
            }
        }

        return super.onKeyUp(keycode, e);
    }


    /**
     * onConfigurationChanged
     */
    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);

//        if (newConfig.orientation == Configuration.ORIENTATION_LANDSCAPE) {
//            ToastUtils.showLong("LANDSCAPE");
//        } else {
//            ToastUtils.showLong("PORTRAIT");
//        }

        int currentNightMode = newConfig.uiMode & Configuration.UI_MODE_NIGHT_MASK;
        switch (currentNightMode) {
            case Configuration.UI_MODE_NIGHT_YES:
                AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES);
                break;
            case Configuration.UI_MODE_NIGHT_NO:
                AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_NO);
                break;
            case Configuration.UI_MODE_NIGHT_UNDEFINED:
            default:
                AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_FOLLOW_SYSTEM);
                break;
        }

        //restart app
        Intent intent = getPackageManager().getLaunchIntentForPackage(getPackageName());
        if (intent == null) {
            ActivityUtils.finishToActivity(MainActivity.class, false);
            return;
        }

        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP | Intent.FLAG_ACTIVITY_NEW_TASK);
        startActivity(intent);
        ActivityUtils.finishActivity(this);
    }

    //////////////////////////////////////////////////////////////////
    private void showSortFilesDialog() {
        SortFilesDialogFragment dialog = new SortFilesDialogFragment();
        dialog.setOnSortItemClickListener(new OnSortItemClickListener() {
            @Override
            public void onSortFileItemClick(DialogFragment dialog, int position) {
                mainViewModel.getOnResortListLiveData().setValue(position);
            }
        });
        dialog.show(getSupportFragmentManager(), SortFilesDialogFragment.class.getSimpleName());
    }

    private void showPasswordDialog(RepoModel repoModel, String path) {
        PasswordDialogFragment dialogFragment = PasswordDialogFragment.newInstance();
        dialogFragment.initData(repoModel.repo_id, repoModel.repo_name);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    getNavContext().navToPath(repoModel, path);
                    binding.pager.setCurrentItem(0);
                    getReposFragment().loadData();
                    refreshToolbarTitle();
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), PasswordDialogFragment.class.getSimpleName());
    }

    /**
     * create a new repo
     */
    private void showNewRepoDialog() {
        NewRepoDialogFragment dialogFragment = new NewRepoDialogFragment();
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), NewRepoDialogFragment.class.getSimpleName());
    }

    /**
     * add new file/files
     */
    private void showAddFileDialog() {
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
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
                    takePhoto();
            }
        }).show();
    }

    //
    private void showNewDirDialog() {
        if (!getNavContext().hasWritePermissionWithRepo()) {
            ToastUtils.showLong(R.string.library_read_only);
            return;
        }
        String parentPath = getNavContext().getNavPath();
        NewDirFileDialogFragment dialogFragment = NewDirFileDialogFragment.newInstance();
        dialogFragment.initData(getNavContext().getRepoModel().repo_id, parentPath, true);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), NewDirFileDialogFragment.class.getSimpleName());
    }

    private void showNewFileDialog() {
        if (!getNavContext().hasWritePermissionWithRepo()) {
            ToastUtils.showLong(R.string.library_read_only);
            return;
        }

        String parentPath = getNavContext().getNavPath();
        NewDirFileDialogFragment dialogFragment = NewDirFileDialogFragment.newInstance();
        dialogFragment.initData(getNavContext().getRepoModel().repo_id, parentPath, false);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    mainViewModel.getOnForceRefreshRepoListLiveData().setValue(true);
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), NewDirFileDialogFragment.class.getSimpleName());
    }

    private void pickFile() {
        if (!getNavContext().hasWritePermissionWithRepo()) {
            ToastUtils.showLong(R.string.library_read_only);
            return;
        }

        takeFile(false);
    }

    ////////////////Launcher///////////////

    //0 camera
    //1 video
    private int permission_media_select_type = -1;

    private void takePhoto() {
        permission_media_select_type = 0;
        cameraPermissionLauncher.launch(Manifest.permission.CAMERA);
    }

    private void takeVideo() {
        permission_media_select_type = 1;
        cameraPermissionLauncher.launch(Manifest.permission.CAMERA);
    }

    private void takeFile(boolean isSingleSelect) {
        String[] mimeTypes = new String[]{"*/*"};
        if (isSingleSelect) {
            singleFileAndImageChooseLauncher.launch(mimeTypes);
        } else {
            multiFileAndImageChooserLauncher.launch(mimeTypes);
        }
    }

    private final ActivityResultLauncher<String[]> multiplePermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestMultiplePermissions(), new ActivityResultCallback<Map<String, Boolean>>() {
        @Override
        public void onActivityResult(Map<String, Boolean> o) {
            if (o.isEmpty()) {
                return;
            }

            for (Map.Entry<String, Boolean> stringBooleanEntry : o.entrySet()) {
                if (Boolean.FALSE.equals(stringBooleanEntry.getValue())) {
                    ToastUtils.showLong(R.string.get_storage_permission_failed);
                    return;
                }
            }
        }
    });

    private final ActivityResultLauncher<Intent> manageStoragePermissionLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (!PermissionUtil.checkExternalStoragePermission(MainActivity.this)) {
                ToastUtils.showLong(R.string.get_storage_permission_failed);
            }
        }
    });

    private final ActivityResultLauncher<String> notificationPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean result) {

            //if result is false, the app can't display notifications

            //continue
            PermissionUtil.requestExternalStoragePermission(MainActivity.this, multiplePermissionLauncher, manageStoragePermissionLauncher);
        }
    });

    private final ActivityResultLauncher<String> cameraPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean result) {
            if (Boolean.FALSE.equals(result)) {
                ToastUtils.showLong(R.string.permission_camera);
                return;
            }

            if (permission_media_select_type == 0) {
                uriPair = TakeCameras.buildTakePhotoUri(MainActivity.this);
                takePhotoLauncher.launch(uriPair.getFirst());
            } else if (permission_media_select_type == 1) {
                uriPair = TakeCameras.buildTakeVideoUri(MainActivity.this);
                takePhotoLauncher.launch(uriPair.getFirst());
            }
        }
    });

    private final ActivityResultLauncher<String[]> singleFileAndImageChooseLauncher = registerForActivityResult(new ActivityResultContracts.OpenDocument(), new ActivityResultCallback<Uri>() {
        @Override
        public void onActivityResult(Uri o) {
            if (null == o) {
                return;
            }

            doSelectSingleFile(o);
        }
    });

    private final ActivityResultLauncher<String[]> multiFileAndImageChooserLauncher = registerForActivityResult(new ActivityResultContracts.OpenMultipleDocuments(), new ActivityResultCallback<List<Uri>>() {
        @Override
        public void onActivityResult(List<Uri> o) {
            if (CollectionUtils.isEmpty(o)) {
                return;
            }
            if (o.size() == 1) {
                doSelectSingleFile(o.get(0));
            } else {
                doSelectedMultiFile(o);
            }
        }
    });


    //    private Uri tempTakePhotoUri;
    private Pair<Uri, File> uriPair;
    private final ActivityResultLauncher<Uri> takePhotoLauncher = registerForActivityResult(new ActivityResultContracts.TakePicture(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean result) {
            if (Boolean.FALSE.equals(result)) {
                return;
            }

            SLogs.d("take photo");

            if (uriPair == null) {
                return;
            }

            Uri uri = uriPair.getFirst();
            File file = uriPair.getSecond();

            RepoModel repoModel = getNavContext().getRepoModel();

            addUploadTask(repoModel, getNavContext().getNavPath(), file.getAbsolutePath());
        }
    });

    private final ActivityResultLauncher<Uri> takeVideoLauncher = registerForActivityResult(new ActivityResultContracts.CaptureVideo(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean o) {
            if (!o) {
                return;
            }

            SLogs.d("take video");
        }
    });

    private Dialog dialog;

    private void showProgressDialog() {
        if (dialog == null) {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
            builder.setView(R.layout.layout_dialog_progress_bar);
            dialog = builder.create();
        }

        if (dialog.isShowing()) {
            dialog.dismiss();
        }
        dialog.show();
    }

    private void dismissProgressDialog() {
        if (dialog != null) {
            dialog.dismiss();
        }
    }

    /////////////////////////////
    private void doSelectedMultiFile(List<Uri> uriList) {
        showProgressDialog();

        try {
            RepoModel repoModel = getNavContext().getRepoModel();
            String parent_dir = getNavContext().getNavPath();

            mainViewModel.checkLocalDirent(curAccount, this, repoModel, parent_dir, uriList, new Consumer<List<Uri>>() {
                @Override
                public void accept(List<Uri> newUris) throws Exception {

                    dismissProgressDialog();

                    if (!CollectionUtils.isEmpty(newUris)) {
                        ToastUtils.showLong(R.string.added_to_upload_tasks);

                        //start worker
                        BackgroundJobManagerImpl.getInstance().startFileUploadWorker();
                    }
                }
            });

        } catch (Exception e) {
            SLogs.e("Could not open requested document", e);
        }
    }

    private void doSelectSingleFile(Uri uri) {
        showProgressDialog();
        try {
            String fileName = Utils.getFilenameFromUri(this, uri);
            String parent_dir = getNavContext().getNavPath();
            String fullPath = Utils.pathJoin(parent_dir, fileName);

            RepoModel repoModel = getNavContext().getRepoModel();
            mainViewModel.checkRemoteDirent(this, repoModel.repo_id, fullPath, new Consumer<DirentFileModel>() {
                @Override
                public void accept(DirentFileModel direntFileModel) throws Exception {
                    if (direntFileModel != null) {
                        showFileExistDialog(uri, fileName);
                    } else {
                        addUploadTask(repoModel, getNavContext().getNavPath(), uri, false);
                    }

                    dismissProgressDialog();
                }
            });
        } catch (Exception e) {
            SLogs.e("Could not open requested document", e);
        }
    }

    private void showFileExistDialog(final Uri file, String fileName) {

        RepoModel repoModel = getNavContext().getRepoModel();

        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
        builder.setTitle(getString(R.string.upload_file_exist));
        builder.setMessage(String.format(getString(R.string.upload_duplicate_found), fileName));

        builder.setPositiveButton(getString(R.string.upload_replace), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                addUploadTask(repoModel, getNavContext().getNavPath(), file, true);
            }
        });

        builder.setNeutralButton(getString(R.string.cancel), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                dialog.dismiss();
            }
        });

        builder.setNegativeButton(getString(R.string.upload_keep_both), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                addUploadTask(repoModel, getNavContext().getNavPath(), file, false);
            }
        });

        builder.show();
    }


    ////////////////add task/////////////
    private void addUploadTask(RepoModel repoModel, String targetDir, String localFilePath) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        mainViewModel.addUploadTask(account, repoModel, targetDir, localFilePath, false, new Consumer<FileTransferEntity>() {
            @Override
            public void accept(FileTransferEntity transferEntity) throws Exception {
//                if (StringUtils.startsWith(transferEntity.mime_type, "image/")) {
//                } else if (StringUtils.startsWith(transferEntity.mime_type, "video/")) {
//                }

                ToastUtils.showLong(R.string.added_to_upload_tasks);
            }
        });
    }

    private void addUploadTask(RepoModel repoModel, String targetDir, Uri sourceUri, boolean isReplace) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        mainViewModel.addUploadTask(account, this, repoModel, targetDir, sourceUri, isReplace, new Consumer<FileTransferEntity>() {
            @Override
            public void accept(FileTransferEntity fileTransferEntity) throws Exception {
                ToastUtils.showLong(R.string.added_to_upload_tasks);
            }
        });

    }


}
