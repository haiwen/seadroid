package com.seafile.seadroid2.ui.main;

import android.Manifest;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.text.TextUtils;
import android.view.KeyEvent;
import android.view.Menu;
import android.view.MenuItem;

import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.appcompat.widget.Toolbar;
import androidx.core.content.FileProvider;
import androidx.fragment.app.DialogFragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.localbroadcastmanager.content.LocalBroadcastManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.BuildConfig;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.ServerInfo;
import com.seafile.seadroid2.data.db.entities.DirentModel;
import com.seafile.seadroid2.data.db.entities.ObjsModel;
import com.seafile.seadroid2.data.db.entities.RepoModel;
import com.seafile.seadroid2.databinding.ActivityMainBinding;
import com.seafile.seadroid2.monitor.FileMonitorService;
import com.seafile.seadroid2.notification.UploadNotificationProvider;
import com.seafile.seadroid2.transfer.DownloadTaskManager;
import com.seafile.seadroid2.transfer.PendingUploadInfo;
import com.seafile.seadroid2.transfer.TransferManager;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.UploadTaskManager;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseQuickActivity;
import com.seafile.seadroid2.ui.camera_upload.MediaObserverService;
import com.seafile.seadroid2.ui.dialog_fragment.NewDirFileDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.NewRepoDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.SortFilesDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnSortItemClickListener;
import com.seafile.seadroid2.ui.folder_backup.FolderBackupService;
import com.seafile.seadroid2.ui.gesture.UnlockGesturePasswordActivity;
import com.seafile.seadroid2.ui.repo.RepoQuickFragment;
import com.seafile.seadroid2.ui.search.Search2Activity;
import com.seafile.seadroid2.ui.settings.SettingsActivity;
import com.seafile.seadroid2.ui.transfer.TransferActivity;
import com.seafile.seadroid2.util.PermissionUtil;
import com.seafile.seadroid2.util.SLogs;
import com.seafile.seadroid2.util.TakeCameras;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.util.sp.FolderBackupConfigSPs;
import com.seafile.seadroid2.util.sp.SettingsManager;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Locale;

import io.reactivex.functions.Consumer;
import kotlin.Pair;

public class MainActivity extends BaseQuickActivity implements Toolbar.OnMenuItemClickListener {

    private ActivityMainBinding binding;
    public static final int INDEX_LIBRARY_TAB = 0;

    private Intent monitorIntent;
    private TransferService txService = null;
    private FolderBackupService folderBackupService = null;
    private TransferReceiver mTransferReceiver;


    private MainViewModel mainViewModel;

    private Menu overFlowMenu;
    private MenuItem menuSearch;

    private Account curAccount;

    private ArrayList<PendingUploadInfo> pendingUploads = Lists.newArrayList();

    private NavContext getNavContext() {
        return mainViewModel.getNavContext();
    }

    public TransferService getTransferService() {
        return txService;
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

        initTabLayout();
        initViewPager();
        initViewModel();

        //service
//        bindService();

//        requestServerInfo(true);

        //job
//        Utils.startCameraSyncJob(this);

//        syncCamera();
    }


    @Override
    public void onStart() {
        super.onStart();
        SLogs.d("onStart");

        if (SettingsManager.getInstance().isGestureLockRequired()) {
            Intent intent = new Intent(this, UnlockGesturePasswordActivity.class);
            startActivity(intent);
        }

        registerReceiver();
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

        String repoId = intent.getStringExtra("repoID");
        String repoName = intent.getStringExtra("repoName");
        String path = intent.getStringExtra("path");

        if (TextUtils.isEmpty(repoId) || TextUtils.isEmpty(path)) {
            return;
        }

        //
        navToPath(repoId, path);
    }

    private void navToPath(String repoId, String path) {
        mainViewModel.requestRepoModel(repoId, new Consumer<RepoModel>() {
            @Override
            public void accept(RepoModel repoModel) throws Exception {
                if (repoModel.encrypted) {
                    mainViewModel.getObjFromDB(repoId, new Consumer<ObjsModel>() {
                        @Override
                        public void accept(ObjsModel objsModel) throws Exception {
                            //check encrypt and decrypt_expire_time
                            long now = TimeUtils.getNowMills();
                            if (objsModel == null || (repoModel.encrypted && now > objsModel.decrypt_expire_time_long)) {
                                // expired
                                showPasswordDialog(repoModel, path);
                            } else {
                                getNavContext().navToPath(repoModel, path);
                                binding.pager.setCurrentItem(0);
                                getReposFragment().loadData();
                                refreshToolbarTitle();
                            }
                        }
                    });
                } else {
                    getNavContext().navToPath(repoModel, path);
                    binding.pager.setCurrentItem(0);
                    getReposFragment().loadData();
                    refreshToolbarTitle();
                }
            }
        });
    }

    private void initTabLayout() {
        binding.slidingTabs.setTabIndicatorAnimationMode(TabLayout.INDICATOR_ANIMATION_MODE_ELASTIC);
        binding.slidingTabs.setSelectedTabIndicator(R.drawable.cat_tabs_rounded_line_indicator);
        binding.slidingTabs.setTabIndicatorFullWidth(false);
        binding.slidingTabs.setTabGravity(TabLayout.GRAVITY_CENTER);

        binding.slidingTabs.addOnTabSelectedListener(new TabLayout.OnTabSelectedListener() {
            @Override
            public void onTabSelected(TabLayout.Tab tab) {
                onTabLayoutSelected();
            }

            @Override
            public void onTabUnselected(TabLayout.Tab tab) {

            }

            @Override
            public void onTabReselected(TabLayout.Tab tab) {

            }
        });
    }

    private void onTabLayoutSelected() {
        //Invalidate menu
        supportInvalidateOptionsMenu();

        //tab
        if (binding.slidingTabs.getSelectedTabPosition() == 0) {
            if (getNavContext().isInRepo()) {
                setActionbarTitle(getNavContext().getNameInCurPath());
                enableUpButton(true);
            } else {
                enableUpButton(false);
                setActionbarTitle(getString(R.string.tabs_library).toUpperCase());
            }
        } else if (binding.slidingTabs.getSelectedTabPosition() == 1) {
            enableUpButton(false);
            setActionbarTitle(getString(R.string.tabs_starred).toUpperCase());
        } else if (binding.slidingTabs.getSelectedTabPosition() == 2) {
            enableUpButton(false);
            setActionbarTitle(getString(R.string.tabs_activity).toUpperCase());
        }
    }

    private void initViewPager() {
        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(mainViewModel.getFragments());
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setOffscreenPageLimit(3);

        String[] tabArray = getResources().getStringArray(R.array.main_fragment_titles);
        TabLayoutMediator mediator = new TabLayoutMediator(binding.slidingTabs, binding.pager, new TabLayoutMediator.TabConfigurationStrategy() {
            @Override
            public void onConfigureTab(@NonNull TabLayout.Tab tab, int position) {
                tab.setText(tabArray[position]);
            }
        });

        mediator.attach();
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

        mainViewModel.getOnNewFileDownloadLiveData().observe(this, new Observer<Pair<String, String>>() {
            @Override
            public void onChanged(Pair<String, String> pair) {
                if (pair == null) {
                    return;
                }

                String repoId = pair.getFirst();
                String path = pair.getSecond();


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

    @Override
    public void onBackPressed() {
        if (binding.pager.getCurrentItem() == INDEX_LIBRARY_TAB) {
            RepoQuickFragment fragment = (RepoQuickFragment) mainViewModel.getFragments().get(0);
            boolean canBack = fragment.backTo();
            if (canBack) {

            } else {
                super.onBackPressed();
            }
        } else {
            super.onBackPressed();
        }
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);

        // handle notification permission on API level >= 33
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            // request notification permission first and then prompt for storage permissions
            // storage permissions handled in onRequestPermissionsResult
            PermissionUtil.requestNotificationPermission(this);
        } else {
            PermissionUtil.requestExternalStoragePermission(this);
        }
    }

    /////////////////////service
    private void bindService() {
        // restart service should it have been stopped for some reason
        Intent mediaObserver = new Intent(this, MediaObserverService.class);
        startService(mediaObserver);
        SLogs.d("start MediaObserverService");

        Intent dIntent = new Intent(this, FolderBackupService.class);
        startService(dIntent);
        SLogs.d("start FolderBackupService");

        Intent dirIntent = new Intent(this, FolderBackupService.class);
        bindService(dirIntent, folderBackupConnection, Context.BIND_AUTO_CREATE);
        SLogs.d("try bind FolderBackupService");

        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        SLogs.d("start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        SLogs.d("try bind TransferService");

        monitorIntent = new Intent(this, FileMonitorService.class);
        startService(monitorIntent);
        SLogs.d("start FileMonitorService");
    }

    private void registerReceiver() {

        if (mTransferReceiver == null) {
            mTransferReceiver = new TransferReceiver();
        }

        IntentFilter filter = new IntentFilter(TransferManager.BROADCAST_ACTION);
        LocalBroadcastManager.getInstance(this).registerReceiver(mTransferReceiver, filter);

    }

    private final ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
            SLogs.d("bond TransferService");

            for (PendingUploadInfo info : pendingUploads) {
                txService.addTaskToUploadQue(curAccount,
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


    private final ServiceConnection folderBackupConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            FolderBackupService.FileBackupBinder binder = (FolderBackupService.FileBackupBinder) service;
            folderBackupService = binder.getService();
            SLogs.d("bond FolderBackupService");

            boolean dirAutomaticUpload = SettingsManager.getInstance().isFolderAutomaticBackup();
            String backupEmail = FolderBackupConfigSPs.getBackupEmail();
            if (dirAutomaticUpload && folderBackupService != null && !TextUtils.isEmpty(backupEmail)) {
                folderBackupService.backupFolder(backupEmail);
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            folderBackupService = null;
        }
    };


    // for receive broadcast from TransferService
    private static class TransferReceiver extends BroadcastReceiver {

        private TransferReceiver() {
        }

        public void onReceive(Context context, Intent intent) {
            String type = intent.getStringExtra("type");
            if (TextUtils.isEmpty(type)) {
                return;
            }

            if (type.equals(DownloadTaskManager.BROADCAST_FILE_DOWNLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
//                onFileDownloadFailed(taskID);
            } else if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
//                onFileUploaded(taskID);
            } else if (type.equals(UploadTaskManager.BROADCAST_FILE_UPLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
//                onFileUploadFailed(taskID);
            }
        }

    }

    //////////////////////////check server info
    private void requestServerInfo(boolean loadFromNet) {
        if (!checkServerProEdition()) {
            // hide Activity tab
            ViewPager2Adapter adapter = (ViewPager2Adapter) binding.pager.getAdapter();
            if (adapter != null) {
                adapter.removeFragment(2);
                binding.slidingTabs.removeTabAt(2);
                adapter.notifyDataSetChanged();
            }
        }

//        if (!checkSearchEnabled()) {
//            // hide search menu
//            if (menuSearch != null)
//                menuSearch.setVisible(false);
//        }

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


    /**
     * Callback received when a permissions request has been completed.
     */
    @Override
    public void onRequestPermissionsResult(int requestCode, String permissions[], int[] grantResults) {
        // Log.i(DEBUG_TAG, "Received response for permission request.");
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        switch (requestCode) {
            case PermissionUtil.PERMISSIONS_POST_NOTIFICATIONS:
                // handle notification permission on API level >= 33
                // dialogue was dismissed -> prompt for storage permissions
                PermissionUtil.requestExternalStoragePermission(this);
                break;
            case PermissionUtil.PERMISSIONS_EXTERNAL_STORAGE:
                // Check if the only required permission has been granted
                // If request is cancelled, the result arrays are empty.
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    // permission was granted
                    SLogs.i("PERMISSIONS_EXTERNAL_STORAGE has been granted");
                }
                break;
        }
    }


//    @Override
//    public void onSaveInstanceState(Bundle outState) {
//        super.onSaveInstanceState(outState);
//        //outState.putInt("tab", getActionBarToolbar().getSelectedNavigationIndex());
//        if (navContext.getRepoID() != null) {
//            outState.putString("repoID", navContext.getRepoID());
//            outState.putString("repoName", navContext.getRepoName());
//            outState.putString("path", navContext.getDirPath());
//            outState.putString("permission", navContext.getDirPermission());
//        }
//    }

    ////////////////////////menu////////////////////////
    private void refreshActionbar() {
        if (getNavContext().isInRepo()) {

            //refresh back btn state
            enableUpButton(true);

            setActionbarTitle(getNavContext().getNameInCurPath());
        } else {
            enableUpButton(false);

            setActionbarTitle(null);
        }

        //refresh toolbar menu
        supportInvalidateOptionsMenu();
    }

    public void setActionbarTitle(String title) {
        if (getSupportActionBar() != null) {
            if (TextUtils.isEmpty(title)) {
                getSupportActionBar().setTitle(R.string.libraries);
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
                    onBackPressed();
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
                    cameraTakePhoto();
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

    void pickFile() {
        if (!getNavContext().hasWritePermissionWithRepo()) {
            ToastUtils.showLong(R.string.library_read_only);
            return;
        }

        takeFile(true);
    }

    private void cameraTakePhoto() {
        try {
            File ImgDir = DataManager.createTempDir();

            String fileName = new SimpleDateFormat("yyyyMMddHHmmss", Locale.getDefault()).format(new Date()) + ".jpg";
            tempTakePhotoFile = new File(ImgDir, fileName);
            tempTakePhotoUri = FileProvider.getUriForFile(this, BuildConfig.FILE_PROVIDER_AUTHORITIES, tempTakePhotoFile);

            takePhoto();
        } catch (IOException e) {
            ToastUtils.showLong(R.string.unknow_error);
        }
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

    private final ActivityResultLauncher<String[]> singleFileAndImageChooseLauncher = registerForActivityResult(new ActivityResultContracts.OpenDocument(), new ActivityResultCallback<Uri>() {
        @Override
        public void onActivityResult(Uri o) {
            if (null == o) {
                ToastUtils.showLong(R.string.saf_upload_path_not_available);
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

            Uri[] uris = o.toArray(new Uri[0]);

        }
    });

    private final ActivityResultLauncher<String> cameraPermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean result) {
            if (Boolean.FALSE.equals(result)) {
                ToastUtils.showLong(R.string.permission_camera);
//                PermissionUtils.launchAppDetailsSettings();
                return;
            }

            if (permission_media_select_type == 0) {
                tempTakePhotoUri = TakeCameras.buildTakePhotoUriAfterCleanOldCacheFiles(MainActivity.this);
                takePhotoLauncher.launch(tempTakePhotoUri);
            } else if (permission_media_select_type == 1) {
                tempTakePhotoUri = TakeCameras.buildTakePhotoUriAfterCleanOldCacheFiles(MainActivity.this);
                takeVideoLauncher.launch(tempTakePhotoUri);
            }
        }
    });

    private Uri tempTakePhotoUri;
    private File tempTakePhotoFile;
    private final ActivityResultLauncher<Uri> takePhotoLauncher = registerForActivityResult(new ActivityResultContracts.TakePicture(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean result) {
            if (!result) {
                return;
            }
            SLogs.d("take photo complete");

            if (tempTakePhotoFile == null) {
                ToastUtils.showLong(R.string.saf_upload_path_not_available);
                return;
            }

            ToastUtils.showLong(R.string.added_to_upload_tasks);
            RepoModel repoModel = getNavContext().getRepoModel();
            if (repoModel.canLocalDecrypt()) {
                addUploadBlocksTask(repoModel.repo_id, repoModel.repo_name, getNavContext().getNavPath(), tempTakePhotoFile.getAbsolutePath());
            } else {
                addUploadTask(repoModel.repo_id, repoModel.repo_name, getNavContext().getNavPath(), tempTakePhotoFile.getAbsolutePath());
            }
        }
    });

    private final ActivityResultLauncher<Uri> takeVideoLauncher = registerForActivityResult(new ActivityResultContracts.CaptureVideo(), new ActivityResultCallback<Boolean>() {
        @Override
        public void onActivityResult(Boolean o) {
            if (!o) {
                return;
            }

            SLogs.d("take video complete");
        }
    });


    ////////////////add task/////////////
    private int addUploadTask(String repoID, String repoName, String targetDir, String localFilePath) {
        ToastUtils.showLong("TODO: 上传文件");
//        if (txService != null) {
//            return txService.addTaskToUploadQue(curAccount, repoID, repoName, targetDir, localFilePath, false, false);
//        } else {
//            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false, false);
//            pendingUploads.add(info);
//            return 0;
//        }
        return 0;
    }

    private int addUploadBlocksTask(String repoID, String repoName, String targetDir, String localFilePath) {
        ToastUtils.showLong("TODO: 上传文件");

//        if (txService != null) {
//            return txService.addTaskToUploadQueBlock(curAccount, repoID, repoName, targetDir, localFilePath, false, false);
//        } else {
//            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false, false);
//            pendingUploads.add(info);
//            return 0;
//        }
        return 0;
    }

    public void addUpdateTask(String repoID, String repoName, String targetDir, String localFilePath) {
        ToastUtils.showLong("TODO: 更新文件");

//        if (txService != null) {
//            txService.addTaskToUploadQue(curAccount, repoID, repoName, targetDir, localFilePath, true, false);
//        } else {
//            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, true, false);
//            pendingUploads.add(info);
//        }
    }

    public void addUpdateBlocksTask(String repoID, String repoName, String targetDir, String localFilePath) {
        ToastUtils.showLong("TODO: 更新文件");

//        if (txService != null) {
//            txService.addTaskToUploadQueBlock(curAccount, repoID, repoName, targetDir, localFilePath, true, false);
//        } else {
//            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, true, false);
//            pendingUploads.add(info);
//        }
    }

    /////////////////////////////
    private void doSelectSingleFile(Uri o) {
        try {
            File tempDir = DataManager.createTempDir();
            File tempFile = new File(tempDir, Utils.getFilenamefromUri(MainActivity.this, o));

            if (!tempFile.createNewFile()) {
                throw new RuntimeException("could not create temporary file");
            }

            RepoModel repoModel = getNavContext().getRepoModel();
            mainViewModel.getDirentsFromServer(repoModel.repo_id, getNavContext().getNavPath(), new Consumer<List<DirentModel>>() {
                @Override
                public void accept(List<DirentModel> list) {
                    boolean duplicate = false;
                    for (DirentModel dirent : list) {
                        if (dirent.name.equals(tempFile.getName())) {
                            duplicate = true;
                            break;
                        }
                    }
                    if (!duplicate) {
                        ToastUtils.showLong(R.string.added_to_upload_tasks);
                        if (repoModel.canLocalDecrypt()) {
                            addUploadBlocksTask(repoModel.repo_id, repoModel.repo_name, getNavContext().getNavPath(), tempFile.getAbsolutePath());
                        } else {
                            addUploadTask(repoModel.repo_id, repoModel.repo_name, getNavContext().getNavPath(), tempFile.getAbsolutePath());
                        }
                    } else {
                        showFileExistDialog(tempFile);
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
            });

        } catch (Exception e) {
            SLogs.e("Could not open requested document", e);
        }
    }

    private void showFileExistDialog(final File file) {
        RepoModel repoModel = getNavContext().getRepoModel();
        MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
        builder.setTitle(getString(R.string.upload_file_exist));
        builder.setMessage(String.format(getString(R.string.upload_duplicate_found), file.getName()));
        builder.setPositiveButton(getString(R.string.upload_replace), new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                ToastUtils.showLong(R.string.added_to_upload_tasks);

                if (repoModel.canLocalDecrypt()) {
                    addUpdateBlocksTask(repoModel.repo_id, repoModel.repo_name, getNavContext().getNavPath(), file.getAbsolutePath());
                } else {
                    addUpdateTask(repoModel.repo_id, repoModel.repo_name, getNavContext().getNavPath(), file.getAbsolutePath());
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
                if (repoModel.canLocalDecrypt()) {
                    addUploadBlocksTask(repoModel.repo_id, repoModel.repo_name, getNavContext().getNavPath(), file.getAbsolutePath());
                } else {
                    addUploadTask(repoModel.repo_id, repoModel.repo_name, getNavContext().getNavPath(), file.getAbsolutePath());
                }
            }
        });
        builder.show();
    }
}
