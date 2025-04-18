package com.seafile.seadroid2.ui;

import android.content.Intent;
import android.content.res.Configuration;
import android.os.Bundle;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.core.splashscreen.SplashScreen;

import com.blankj.utilcode.util.ActivityUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.compat.AppCompatKt;
import com.seafile.seadroid2.enums.NightMode;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.datastore.sp_livedata.AlbumBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.data_migrate.DataMigrationActivity;
import com.seafile.seadroid2.ui.data_migrate.DataMigrationV303Activity;
import com.seafile.seadroid2.ui.main.MainActivity;

import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;

public class SplashActivity extends BaseActivity {
    private Disposable disposable;

    @Override
    protected void onCreate(Bundle savedInstanceState) {

        initAppNightMode();

        SplashScreen splashScreen = SplashScreen.installSplashScreen(this);

        super.onCreate(savedInstanceState);
//        setContentView(R.layout.activity_splash);
        splashScreen.setKeepOnScreenCondition(() -> true);

        long duration = 500;
        disposable = Observable.timer(duration, TimeUnit.MILLISECONDS)
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(aLong -> {
                    navTo();
                });

    }

    private void initAppNightMode() {
        //
        Settings.initUserSettings();

        if (Settings.NIGHT_MODE != null) {
            NightMode nightMode = Settings.NIGHT_MODE.queryValue();
            if (nightMode == NightMode.FOLLOW_SYSTEM) {
                int lastNightMode = getResources().getConfiguration().uiMode & Configuration.UI_MODE_NIGHT_MASK;
                AppCompatDelegate.setDefaultNightMode(lastNightMode);
            } else {
                AppCompatDelegate.setDefaultNightMode(nightMode.ordinal());
            }
        } else {
            AppCompatDelegate.setDefaultNightMode(NightMode.FOLLOW_SYSTEM.ordinal());
        }
    }

    private void navTo() {
        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (!AppDataManager.isMigratedWhenV300()) {

            Intent intent = new Intent(this, DataMigrationActivity.class);
            dataMigrationLauncher.launch(intent);

        } else if (!AppDataManager.isMigratedWhenV303()) {

            Intent intent = new Intent(this, DataMigrationV303Activity.class);
            dataMigrationLauncher.launch(intent);

        } else if (curAccount == null || !curAccount.hasValidToken()) {

            Intent newIntent = new Intent(this, AccountsActivity.class);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            ActivityUtils.startActivity(newIntent);

            finish();
        } else {

            // notice:
            // when the app restarts, reset last scan time, so that it will scan all file again
            FolderBackupSharePreferenceHelper.resetLastScanTime();
            AlbumBackupSharePreferenceHelper.resetLastScanTime();

            MainActivity.startThis(this);
            finish();
        }
    }

    private final ActivityResultLauncher<Intent> dataMigrationLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            navTo();
        }
    });


    @Override
    protected void onDestroy() {
        super.onDestroy();

        if (disposable != null) {
            disposable.dispose();
            disposable = null;
        }
    }
}
