package com.seafile.seadroid2.ui.main;

import android.app.Activity;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.graphics.PorterDuff;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.os.SystemClock;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;
import android.widget.LinearLayout;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.core.content.ContextCompat;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.google.android.material.navigation.NavigationBarView;
import com.google.firebase.crashlytics.FirebaseCrashlytics;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.bus.BusAction;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.context.GlobalNavContext;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.ActivityMainBinding;
import com.seafile.seadroid2.enums.NightMode;
import com.seafile.seadroid2.framework.file_monitor.FileDaemonService;
import com.seafile.seadroid2.framework.file_monitor.FileSyncService;
import com.seafile.seadroid2.framework.model.ServerInfo;
import com.seafile.seadroid2.framework.util.PermissionUtil;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.activities.AllActivitiesFragment;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.repo.RepoQuickFragment;
import com.seafile.seadroid2.ui.settings.TabSettings2Fragment;
import com.seafile.seadroid2.ui.star.StarredQuickFragment;
import com.seafile.seadroid2.ui.wiki.WikiFragment;

import java.util.List;
import java.util.Optional;

public class MainActivity extends BaseActivity {
    private static final String TAG = "MainActivity";
    public static final int INDEX_LIBRARY_TAB = 0;

    private ActivityMainBinding binding;
    private FileSyncService syncService;
    private MainViewModel mainViewModel;
    private ViewPager2Adapter pagerAdapter;
    private Account curAccount;

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

        curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (curAccount == null || !curAccount.hasValidToken()) {
            finishAndStartAccountsActivity();
            return;
        }

        applyEdgeToEdgeInsets();

        initSettings();

        initFireBase();

        initOnBackPressedDispatcher();

        resetOverflowIcon();

        initBottomNavigation();

        initViewPager();

        initViewModel();

        //service
        bindService();

        restoreNavContext();

        registerComponent();

        requestServerInfo(true);

        startMediaMountWatching();
    }

    private void resetOverflowIcon() {
        Drawable more = ContextCompat.getDrawable(getBaseContext(), R.drawable.icon_more_vertical_32_18);
        if (more != null) {
            NightMode nm = Settings.NIGHT_MODE.queryValue();
            int c;
            if (nm == NightMode.OFF) {
                c = R.color.material_grey_900;
            } else if (nm == NightMode.ON) {
                c = R.color.material_grey_200;
            } else if (isNightActive()) {
                c = R.color.material_grey_200;
            } else {
                c = R.color.material_grey_900;
            }
            more.setColorFilter(ContextCompat.getColor(getApplicationContext(), c), PorterDuff.Mode.SRC_ATOP);
            binding.toolbarActionbar.setOverflowIcon(more);
        }
    }

    private void applyEdgeToEdgeInsets() {
        ViewCompat.setOnApplyWindowInsetsListener(binding.getRoot(), (v, insets) -> {
            Insets systemBars = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(systemBars.left, 0, systemBars.right, 0);

            LinearLayout.LayoutParams params = (LinearLayout.LayoutParams) binding.statusBarGuideline.getLayoutParams();
            params.height = systemBars.top;
            binding.statusBarGuideline.setLayoutParams(params);
            return insets;
        });
    }

    private void restoreNavContext() {

        GlobalNavContext.restore();

        refreshActionbar();
    }

    private void registerComponent() {
        //register network status changed
        NetworkUtils.registerNetworkStatusChangedListener(onNetworkStatusChangedListener);
    }

    private final NetworkUtils.OnNetworkStatusChangedListener onNetworkStatusChangedListener = new NetworkUtils.OnNetworkStatusChangedListener() {
        @Override
        public void onDisconnected() {
            refreshActionbar();
        }

        @Override
        public void onConnected(NetworkUtils.NetworkType networkType) {
            refreshActionbar();
        }
    };

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

    private long lastBackTs;

    private void initOnBackPressedDispatcher() {
        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (System.currentTimeMillis() < lastBackTs + 1000) {
                    finish();
                    return;
                }

                lastBackTs = System.currentTimeMillis();
                Toasts.showShort(R.string.tip_press_again_to_exit);
            }
        });
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);

//        OneTimeWorkRequest oneTimeWorkRequest = new OneTimeWorkRequest.Builder(LivePhotoScanner.class).build();
//        BackgroundJobManagerImpl.getInstance().getWorkManager().enqueue(oneTimeWorkRequest);

        requestNotificationPermission();
    }

    private void requestNotificationPermission() {
        if (PermissionUtil.hasNotGrantNotificationPermission(this)) {
            PermissionUtil.requestNotificationPermission(this);
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == PermissionUtil.PERMISSIONS_POST_NOTIFICATIONS) {
            if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {

            } else {
                SLogs.d(TAG, "Notification permission denied");
            }
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
        SLogs.d(TAG, "onRestart");

//        Account cAccount = SupportAccountManager.getInstance().getCurrentAccount();
//        if (curAccount == null || !curAccount.equals(cAccount)) {
//            finishAndStartAccountsActivity();
//        }
    }

    @Override
    protected void onStart() {
        super.onStart();
    }

    @Override
    protected void onDestroy() {
        NetworkUtils.unregisterNetworkStatusChangedListener(onNetworkStatusChangedListener);

        stopWatching();

        //
        BusHelper.getCommonObserver().removeObserver(commonBusObserver);
        BusHelper.getNavContextObserver().removeObserver(navContextObserver);

//        BackgroundJobManagerImpl.getInstance().stopAlbumBackupPeriodicScan(SeadroidApplication.getAppContext());
//        BackgroundJobManagerImpl.getInstance().stopFolderBackupPeriodicScan(SeadroidApplication.getAppContext());
//        BackgroundJobManagerImpl.getInstance().cancelAllJobs();


        unbindService();
        //
        super.onDestroy();
    }

    @Override
    protected void onStop() {
        super.onStop();
    }

    @Override
    protected void onNewIntent(Intent intent) {
        super.onNewIntent(intent);
        SLogs.d(TAG, "onNewIntent");

        // if the user started the Seadroid app from the Launcher, keep the old Activity
        final String intentAction = intent.getAction();
        if (intent.hasCategory(Intent.CATEGORY_LAUNCHER)
                && intentAction != null
                && intentAction.equals(Intent.ACTION_MAIN)) {
            return;
        }

        Account selectedAccount = SupportAccountManager.getInstance().getCurrentAccount();
        SLogs.d(TAG, "Current account: " + curAccount);
        if (!curAccount.equals(selectedAccount)) {
            SLogs.d(TAG, "Account switched, restarting activity.");
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
        binding.pager.postDelayed(new Runnable() {
            @Override
            public void run() {
                RepoQuickFragment quickFragment = getReposFragment();
                if (quickFragment != null && quickFragment.isAdded() && !quickFragment.isDetached()) {
                    quickFragment.switchPath(repoId, path, isDir);
                }
                
                binding.navBottomView.setSelectedItemId(R.id.tabs_library);

                refreshActionbar();
            }
        }, 400);

    }

    private ViewPager2Adapter getPagerAdapter() {
        if (pagerAdapter == null) {
            pagerAdapter = new ViewPager2Adapter(this);
        }
        return pagerAdapter;
    }

    private void initViewPager() {
        List<Fragment> fragments = mainViewModel.getFragments();
        getPagerAdapter().addFragments(fragments);

        binding.pager.setOffscreenPageLimit(fragments.size());
        binding.pager.setAdapter(getPagerAdapter());
        binding.pager.setUserInputEnabled(false);
    }

    private static final long DOUBLE_TAP_TIMEOUT = 300;
    private int lastReselectItemId = View.NO_ID;
    private long lastReselectTime = 0L;

    private void initBottomNavigation() {
        binding.navBottomView.setItemHorizontalTranslationEnabled(true);
        binding.navBottomView.setOnItemSelectedListener(new NavigationBarView.OnItemSelectedListener() {
            @Override
            public boolean onNavigationItemSelected(@NonNull MenuItem menuItem) {
                onBottomNavigationSelected(menuItem);
                return true;
            }
        });
        binding.navBottomView.setOnItemReselectedListener(item -> {
            long now = SystemClock.elapsedRealtime();

            if (item.getItemId() == lastReselectItemId && now - lastReselectTime <= DOUBLE_TAP_TIMEOUT) {

                onBottomNavItemDoubleTap(item);
                lastReselectItemId = View.NO_ID;
                lastReselectTime = 0L;
                return;
            }

            lastReselectItemId = item.getItemId();
            lastReselectTime = now;

            // do nothing here
            onBottomNavItemSingleReselect(item);
        });
    }

    private void onBottomNavItemDoubleTap(MenuItem item) {
        SLogs.d(TAG, "onBottomNavItemDoubleTap:" + item.getTitle());

        if (item.getItemId() != R.id.tabs_library) {
            return;
        }

        RepoQuickFragment repoQuickFragment = getReposFragment();
        if (repoQuickFragment != null) {
            repoQuickFragment.backToRepoList();
        }
    }

    private void onBottomNavItemSingleReselect(MenuItem item) {
        SLogs.d(TAG, "onBottomNavItemSingleReselect:" + item.getTitle());
    }

    private void onBottomNavigationSelected(MenuItem menuItem) {
        //tab
        int pagerIndex = -1;
        if (menuItem.getItemId() == R.id.tabs_library) {
            pagerIndex = getPagerAdapter().indexByClass(RepoQuickFragment.class);
        } else if (menuItem.getItemId() == R.id.tabs_starred) {
            pagerIndex = getPagerAdapter().indexByClass(StarredQuickFragment.class);
        } else if (menuItem.getItemId() == R.id.tabs_wiki) {
            pagerIndex = getPagerAdapter().indexByClass(WikiFragment.class);
        } else if (menuItem.getItemId() == R.id.tabs_activity) {
            pagerIndex = getPagerAdapter().indexByClass(AllActivitiesFragment.class);
        } else if (menuItem.getItemId() == R.id.tabs_settings) {
            pagerIndex = getPagerAdapter().indexByClass(TabSettings2Fragment.class);
        }

        if (pagerIndex == -1) {
            return;
        }

        binding.pager.setCurrentItem(pagerIndex);

        //
        refreshActionbar();
    }

    private void initViewModel() {
        //register bus
        BusHelper.getNavContextObserver().observe(this, navContextObserver);
        BusHelper.getCommonObserver().observe(this, commonBusObserver);


        mainViewModel.getServerInfoLiveData().observe(this, new Observer<ServerInfo>() {
            @Override
            public void onChanged(ServerInfo serverInfo) {
                requestServerInfo(false);
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

    // service
    private void bindService() {
        Intent syncIntent = new Intent(this, FileSyncService.class);
        if (!isBound) {
            bindService(syncIntent, syncConnection, Context.BIND_AUTO_CREATE);
            isBound = true;
        }

        boolean isTurnOn = Settings.BACKUP_SETTINGS_BACKGROUND_SWITCH.queryValue();
        Intent daemonIntent = new Intent(this, FileDaemonService.class);
        if (isTurnOn) {
            ContextCompat.startForegroundService(this, daemonIntent);
        } else {
            stopService(daemonIntent);
        }
    }

    private void unbindService() {
        if (isBound) {
            unbindService(syncConnection);
            isBound = false;
            syncService = null;
        }
    }

    private boolean isBound = false;
    private final ServiceConnection syncConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            FileSyncService.FileSyncBinder binder = (FileSyncService.FileSyncBinder) service;
            syncService = binder.getService();
            SLogs.d(TAG, "bond FileSyncService");
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            syncService = null;
            isBound = false;
            SLogs.d(TAG, "FileSyncService disconnected");
        }
    };

    // check server info
    private void requestServerInfo(boolean loadFromNet) {
        Optional<ServerInfo> optional = checkServerInfo();
        if (optional.isEmpty()) { // if load failed, remove special fragments
            binding.navBottomView.getMenu().removeItem(R.id.tabs_activity);
            binding.navBottomView.getMenu().removeItem(R.id.tabs_wiki);

            int activityIndex = getPagerAdapter().removeByClass(AllActivitiesFragment.class);
            if (activityIndex != -1) {
                getPagerAdapter().notifyItemRemoved(activityIndex);
            }

            int wikiIndex = getPagerAdapter().removeByClass(WikiFragment.class);
            if (wikiIndex != -1) {
                getPagerAdapter().notifyItemRemoved(wikiIndex);
            }
            return;
        }

        // remove activity tab
        if (!optional.get().isProEdition()) {
            binding.navBottomView.getMenu().removeItem(R.id.tabs_activity);

            ViewPager2Adapter adapter = (ViewPager2Adapter) binding.pager.getAdapter();
            if (adapter != null) {
                adapter.removeByClass(AllActivitiesFragment.class);
            }
        }

        // remove wiki fragment if disable wiki
        if (!optional.get().isEnableWiki()) {
            binding.navBottomView.getMenu().removeItem(R.id.tabs_wiki);

            ViewPager2Adapter adapter = (ViewPager2Adapter) binding.pager.getAdapter();
            if (adapter != null) {
                adapter.removeByClass(WikiFragment.class);
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

        //refresh toolbar menu
        invalidateOptionsMenu();

        if (binding.pager.getCurrentItem() == 0) {

            String offline = "";
            if (!NetworkUtils.isConnected()) {
                offline = " (" + getString(R.string.offline) + ")";
            }

            if (GlobalNavContext.getCurrentNavContext().inRepo()) {
                enableUpButton(true);
                setActionbarTitle(GlobalNavContext.getCurrentNavContext().getLastPathName() + offline);
            } else {
                enableUpButton(false);
                setActionbarTitle(getString(R.string.tabs_library) + offline);
            }
            return;
        }

        if (binding.pager.getCurrentItem() == 1) {
            setActionbarTitle(getString(R.string.tabs_starred));
            enableUpButton(false);
            return;
        }

        if (binding.pager.getCurrentItem() == 2) {
            setActionbarTitle(getString(R.string.tabs_wiki));
            enableUpButton(false);
            return;
        }

        MenuItem activityMenuItem = binding.navBottomView.getMenu().findItem(R.id.tabs_activity);
        if (null == activityMenuItem) {
            if (binding.pager.getCurrentItem() == 3) {
                setActionbarTitle(getString(R.string.settings));
                enableUpButton(false);
            }
        } else {
            if (binding.pager.getCurrentItem() == 3) {
                setActionbarTitle(getString(R.string.tabs_activity));
                enableUpButton(false);
            } else {
                setActionbarTitle(getString(R.string.settings));
                enableUpButton(false);
            }
        }

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

    private int lastNightMode;
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

    private boolean isNightActive() {
        int newNightMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
        return newNightMode == Configuration.UI_MODE_NIGHT_YES;
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

    private final Observer<NavContext> navContextObserver = new Observer<NavContext>() {
        @Override
        public void onChanged(NavContext navContext) {
            refreshActionbar();
        }
    };

    private final Observer<String> commonBusObserver = new Observer<String>() {
        @Override
        public void onChanged(String action) {
            if (TextUtils.isEmpty(action)) {
                return;
            }

            if (TextUtils.equals(action, BusAction.RESTART_FILE_MONITOR)) {
                if (syncService != null) {
                    syncService.restartFolderMonitor();
                }
            } else if (TextUtils.equals(action, BusAction.START_FOREGROUND_FILE_MONITOR)) {

                Intent intent = new Intent(MainActivity.this, FileDaemonService.class);
                ContextCompat.startForegroundService(MainActivity.this, intent);

            } else if (TextUtils.equals(action, BusAction.STOP_FOREGROUND_FILE_MONITOR)) {
                Intent intent = new Intent(MainActivity.this, FileDaemonService.class);
                stopService(intent);
            }
        }
    };


    /**
     * Start observing mount/unmount events
     */
    public void startMediaMountWatching() {
        IntentFilter filter = new IntentFilter();
        filter.addAction(Intent.ACTION_MEDIA_MOUNTED);
        filter.addAction(Intent.ACTION_MEDIA_REMOVED);
        filter.addAction(Intent.ACTION_MEDIA_UNMOUNTED);
        filter.addDataScheme("file");
        registerReceiver(storageReceiver, filter);
    }

    private final BroadcastReceiver storageReceiver = new BroadcastReceiver() {
        @Override
        public void onReceive(Context context, Intent intent) {
            String action = intent.getAction();
            if (action == null) {
                return;
            }

            Uri data = intent.getData();
            if (data == null) {
                return;
            }

            String path = data.getPath();

            if (Intent.ACTION_MEDIA_MOUNTED.equals(action)) {
                SLogs.d("Storage", "mounted: " + path);//mounted: /storage/67DA-5855
                notifyMountChanged(action, path);
            } else if (Intent.ACTION_MEDIA_UNMOUNTED.equals(action)) {
                SLogs.d("Storage", "unmounted: " + path);//unmounted: /storage/67DA-5855
                notifyMountChanged(action, path);
            } else if (Intent.ACTION_MEDIA_REMOVED.equals(action)) {
                SLogs.d("Storage", "removed: " + path);//removed: /storage/67DA-5855
                notifyMountChanged(action, path);
            }
        }
    };

    private void notifyMountChanged(String action, String path) {
        BusHelper.getCommonObserver().post(action + "-" + path);
    }

    /**
     * Stop observing
     */
    public void stopWatching() {
        unregisterReceiver(storageReceiver);
    }

}
