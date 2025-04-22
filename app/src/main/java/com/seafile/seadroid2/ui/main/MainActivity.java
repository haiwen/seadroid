package com.seafile.seadroid2.ui.main;

import android.Manifest;
import android.app.Activity;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.res.Configuration;
import android.os.Build;
import android.os.Bundle;
import android.os.IBinder;
import android.text.TextUtils;
import android.view.MenuItem;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.TimeUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.navigation.NavigationBarView;
import com.google.firebase.crashlytics.FirebaseCrashlytics;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.context.GlobalNavContext;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.ActivityMainBinding;
import com.seafile.seadroid2.enums.NightMode;
import com.seafile.seadroid2.enums.RefreshStatusEnum;
import com.seafile.seadroid2.framework.model.ServerInfo;
import com.seafile.seadroid2.framework.db.entities.EncKeyCacheEntity;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.file_monitor.FileSyncService;
import com.seafile.seadroid2.framework.util.PermissionUtil;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.activities.AllActivitiesFragment;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.dialog_fragment.PasswordDialogFragment;
import com.seafile.seadroid2.ui.dialog_fragment.listener.OnRefreshDataListener;
import com.seafile.seadroid2.ui.repo.RepoQuickFragment;

import java.util.List;
import java.util.Map;
import java.util.Optional;

import io.reactivex.functions.Consumer;

public class MainActivity extends BaseActivity {

    public static final int INDEX_LIBRARY_TAB = 0;

    private ActivityMainBinding binding;
    private FileSyncService syncService;
    private MainViewModel mainViewModel;

    private Account curAccount;

//    private NavContext getCurrentNavContext() {
//        return GlobalNavContext.getCurrentNavContext();
//    }

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

        lastOrientation = getResources().getConfiguration().orientation;
        lastNightMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;

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

        initSettings();

        initFireBase();
        initOnBackPressedDispatcher();
        initBottomNavigation();

        initViewPager();

        initViewModel();

        //service
        bindService();

        restoreNavContext();

        requestServerInfo(true);
    }

    private void restoreNavContext() {

        GlobalNavContext.restore();

        refreshActionbar();
    }

    private void initSettings() {
        Settings.initUserSettings();
    }

    private void initFireBase() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account != null) {
            FirebaseCrashlytics crashlytics = FirebaseCrashlytics.getInstance();
            crashlytics.setUserId(account.getSignature());
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

        requestPermissions();
    }

    private void requestPermissions() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            startMultiplePermissionLauncher.launch(new String[]{Manifest.permission.POST_NOTIFICATIONS, Manifest.permission.READ_MEDIA_IMAGES, Manifest.permission.READ_MEDIA_VIDEO});
        } else {
            PermissionUtil.requestExternalStoragePermission(this, multiplePermissionLauncher, manageStoragePermissionLauncher);
        }
    }

    public static void startThis(Activity context) {
        Intent i = new Intent(context, MainActivity.class);
        i.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        context.startActivity(i);
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
    public void onRestart() {
        super.onRestart();
        SLogs.d("onRestart");

//        Account cAccount = SupportAccountManager.getInstance().getCurrentAccount();
//        if (curAccount == null || !curAccount.equals(cAccount)) {
//            finishAndStartAccountsActivity();
//        }
    }

    @Override
    protected void onDestroy() {
        if (isBound) {
            unbindService(syncConnection);
            isBound = false;
            syncService = null;
        }
        BackgroundJobManagerImpl.getInstance().cancelAllJobs();

        super.onDestroy();
    }

    @Override
    protected void onStop() {
        super.onStop();
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
        if (!curAccount.equals(selectedAccount)) {
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
                                GlobalNavContext.switchToPath(repoModel, finalPath);
                                binding.pager.setCurrentItem(0);
                                getReposFragment().loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE);
                                refreshToolbarTitle();
                            }
                        }
                    });

                } else {
                    GlobalNavContext.switchToPath(repoModel, finalPath);
                    binding.pager.setCurrentItem(0);
                    getReposFragment().loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE);
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
//        supportInvalidateOptionsMenu();

        //tab
        if (menuItem.getItemId() == R.id.tabs_library) {
            if (GlobalNavContext.getCurrentNavContext().inRepo()) {
                setActionbarTitle(GlobalNavContext.getCurrentNavContext().getLastPathName());
                enableUpButton(true);
            } else {
                enableUpButton(false);
                setActionbarTitle(getString(R.string.tabs_library));
            }

            binding.pager.setCurrentItem(0);
            return;
        }

        if (menuItem.getItemId() == R.id.tabs_starred) {
            enableUpButton(false);
            setActionbarTitle(getString(R.string.tabs_starred));

            binding.pager.setCurrentItem(1);
            return;
        }

        MenuItem activityMenuItem = binding.navBottomView.getMenu().findItem(R.id.tabs_activity);
        if (null == activityMenuItem) {
            if (menuItem.getItemId() == R.id.tabs_settings) {
                enableUpButton(false);
                setActionbarTitle(getString(R.string.settings));

                binding.pager.setCurrentItem(2);
            }
        } else {
            if (menuItem.getItemId() == R.id.tabs_activity) {
                enableUpButton(false);
                setActionbarTitle(getString(R.string.tabs_activity));

                binding.pager.setCurrentItem(2);
            } else if (menuItem.getItemId() == R.id.tabs_settings) {
                enableUpButton(false);
                setActionbarTitle(getString(R.string.settings));

                binding.pager.setCurrentItem(3);
            }
        }
    }

    private void initViewPager() {
        List<Fragment> fragments = mainViewModel.getFragments();
        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(this);
        viewPager2Adapter.addFragments(fragments);
        binding.pager.setOffscreenPageLimit(fragments.size());
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setUserInputEnabled(false);
        binding.pager.registerOnPageChangeCallback(new ViewPager2.OnPageChangeCallback() {
            @Override
            public void onPageSelected(int position) {
                super.onPageSelected(position);

                onViewPageSelected(position);
            }
        });
    }

    private void onViewPageSelected(int position) {

        invalidateMenu();

        if (0 == position) {
            binding.navBottomView.setSelectedItemId(R.id.tabs_library);
            return;
        }

        if (1 == position) {
            binding.navBottomView.setSelectedItemId(R.id.tabs_starred);
            return;
        }

        MenuItem activityMenuItem = binding.navBottomView.getMenu().findItem(R.id.tabs_activity);
        if (null == activityMenuItem) {
            //means the server is not pro edition
            if (2 == position) {
                binding.navBottomView.setSelectedItemId(R.id.tabs_settings);
            }
        } else {

            if (2 == position) {
                binding.navBottomView.setSelectedItemId(R.id.tabs_activity);
                return;
            }

            if (3 == position) {
                binding.navBottomView.setSelectedItemId(R.id.tabs_settings);
            }
        }
    }

    private void initViewModel() {
        mainViewModel.getServerInfoLiveData().observe(this, new Observer<ServerInfo>() {
            @Override
            public void onChanged(ServerInfo serverInfo) {
                requestServerInfo(false);
            }
        });

        BusHelper.getNavContextObserver().observe(this, new Observer<NavContext>() {
            @Override
            public void onChanged(NavContext context) {
                refreshActionbar();
            }
        });

        Settings.NIGHT_MODE.observe(this, new Observer<NightMode>() {
            @Override
            public void onChanged(NightMode nightMode) {
//                if (checkCanApply(nightMode)) {
                onUserApplyNightMode(nightMode);
//                }
            }
        });
    }


    private void refreshToolbarTitle() {
        if (!GlobalNavContext.getCurrentNavContext().inRepo()) {
            getActionBarToolbar().setTitle(R.string.libraries);
        } else if (GlobalNavContext.getCurrentNavContext().inRepoRoot()) {
            getActionBarToolbar().setTitle(GlobalNavContext.getCurrentNavContext().getRepoModel().repo_name);
        } else {
            String toolbarTitle = GlobalNavContext.getCurrentNavContext().getLastPathName();
            getActionBarToolbar().setTitle(toolbarTitle);
        }
    }

    // service
    private void bindService() {
        if (!isBound) {
            Intent syncIntent = new Intent(this, FileSyncService.class);
            bindService(syncIntent, syncConnection, Context.BIND_AUTO_CREATE);
            isBound = true;
        }
    }

    private boolean isBound = false;
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
            isBound = false;
            SLogs.d("FileSyncService disconnected");
        }
    };

    // check server info
    private void requestServerInfo(boolean loadFromNet) {
        Optional<ServerInfo> optional = checkServerInfo();
        if (optional.isEmpty() || !optional.get().isProEdition()) {
            binding.navBottomView.getMenu().removeItem(R.id.tabs_activity);

            // hide Activity tab
            ViewPager2Adapter adapter = (ViewPager2Adapter) binding.pager.getAdapter();
            if (adapter != null) {
                int index = adapter.removeByClass(AllActivitiesFragment.class);
                if (index != -1) {
                    adapter.notifyItemRemoved(index);
                }
            }
        }

        if (loadFromNet) {
            //invalidate local account cache
            mainViewModel.getServerInfo();
        }
    }

    /**
     * @return 0: is pro edition, 1: is search enable
     */
    private Optional<ServerInfo> checkServerInfo() {
        if (curAccount == null)
            return Optional.empty();

        ServerInfo serverInfo = SupportAccountManager.getInstance().getServerInfo(curAccount);
        return Optional.of(serverInfo);
    }

    private void finishAndStartAccountsActivity() {
        Intent newIntent = new Intent(this, AccountsActivity.class);
        newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(newIntent);
        finish();
    }

    // menu
    private void refreshActionbar() {
        if (binding.pager.getCurrentItem() == 0) {
            if (GlobalNavContext.getCurrentNavContext().inRepo()) {
                enableUpButton(true);
                setActionbarTitle(GlobalNavContext.getCurrentNavContext().getLastPathName());
            } else {
                enableUpButton(false);
                setActionbarTitle(getString(R.string.tabs_library));
            }
        } else if (binding.pager.getCurrentItem() == 1) {
            setActionbarTitle(getString(R.string.tabs_starred));
        } else if (binding.pager.getCurrentItem() == 2) {
            setActionbarTitle(getString(R.string.tabs_activity));
        } else {
            setActionbarTitle(getString(R.string.settings));
        }

        //refresh toolbar menu
        invalidateOptionsMenu();
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


    private int lastNightMode; // 在类顶部声明
    private int lastOrientation;

    /**
     * @see MainActivity -> android:configChanges="uiMode|orientation|screenSize"
     */
    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        if (lastOrientation != newConfig.orientation) {
//            if (newConfig.orientation == Configuration.ORIENTATION_LANDSCAPE) {
//                ToastUtils.showLong("LANDSCAPE");
//            } else {
//                ToastUtils.showLong("PORTRAIT");
//            }
            lastOrientation = newConfig.orientation;
        }

        int newNightMode = newConfig.uiMode & Configuration.UI_MODE_NIGHT_MASK;
        if (newNightMode != lastNightMode) {
            lastNightMode = newNightMode;
            if (newNightMode == Configuration.UI_MODE_NIGHT_NO) {
                onSystemApplyNightMode(NightMode.OFF);
            } else if (newNightMode == Configuration.UI_MODE_NIGHT_YES) {
                onSystemApplyNightMode(NightMode.ON);
            }
        }
    }

    private void onUserApplyNightMode(NightMode nightMode) {
        if (nightMode == null) {
            return;
        }

        AppCompatDelegate.setDefaultNightMode(nightMode.ordinal());
        Settings.NIGHT_MODE.putValue(nightMode);

        restartThis();
    }

    private void onSystemApplyNightMode(NightMode futureNightMode) {
        if (futureNightMode == null) {
            return;
        }

        NightMode localMode = Settings.NIGHT_MODE.queryValue();
        if (localMode != NightMode.FOLLOW_SYSTEM) {
            return;
        }

        AppCompatDelegate.setDefaultNightMode(futureNightMode.ordinal());

        restartThis();
    }

    //todo replace by recreate()
    private void restartThis() {
        Intent intent = getPackageManager().getLaunchIntentForPackage(getPackageName());
        if (intent == null) {
            ActivityUtils.finishToActivity(MainActivity.class, false);
            return;
        }

        intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
        overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
    }

    private void showPasswordDialog(RepoModel repoModel, String path) {
        PasswordDialogFragment dialogFragment = PasswordDialogFragment.newInstance(repoModel.repo_id, repoModel.repo_name);
        dialogFragment.setRefreshListener(new OnRefreshDataListener() {
            @Override
            public void onActionStatus(boolean isDone) {
                if (isDone) {
                    GlobalNavContext.switchToPath(repoModel, path);
                    binding.pager.setCurrentItem(0);
                    getReposFragment().loadData(RefreshStatusEnum.LOCAL_THEN_REMOTE);
                    refreshToolbarTitle();
                }
            }
        });
        dialogFragment.show(getSupportFragmentManager(), PasswordDialogFragment.class.getSimpleName());
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

    private final ActivityResultLauncher<String[]> startMultiplePermissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestMultiplePermissions(), new ActivityResultCallback<Map<String, Boolean>>() {
        @Override
        public void onActivityResult(Map<String, Boolean> o) {
            if (o.isEmpty()) {
                return;
            }

            boolean mediaIsRefuse = false;
            for (Map.Entry<String, Boolean> e : o.entrySet()) {
                if (Boolean.FALSE.equals(e.getValue()) && !TextUtils.equals(Manifest.permission.POST_NOTIFICATIONS, e.getKey())) {
                    mediaIsRefuse = true;
                }
            }

            if (mediaIsRefuse) {
                ToastUtils.showLong(getString(R.string.get_permission_failed, "IMAGE/VIDEO"));
            }

            PermissionUtil.requestExternalStoragePermission(MainActivity.this, multiplePermissionLauncher, manageStoragePermissionLauncher);
        }
    });
}
