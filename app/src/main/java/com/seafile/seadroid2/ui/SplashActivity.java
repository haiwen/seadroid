package com.seafile.seadroid2.ui;

import android.content.Intent;
import android.os.Bundle;

import androidx.core.splashscreen.SplashScreen;

import com.blankj.utilcode.util.ActivityUtils;
import com.google.firebase.analytics.FirebaseAnalytics;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.AnalyticsEvent;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.base.BaseActivity;
import com.seafile.seadroid2.ui.data_migrate.DataMigrationActivity;
import com.seafile.seadroid2.ui.main.MainActivity;

import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;

public class SplashActivity extends BaseActivity {
    private Disposable disposable;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        SplashScreen splashScreen = SplashScreen.installSplashScreen(this);

        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_splash);

        splashScreen.setKeepOnScreenCondition(new SplashScreen.KeepOnScreenCondition() {
            @Override
            public boolean shouldKeepOnScreen() {
                return true;
            }
        });


        long duration = 500;
        disposable = Observable.timer(duration, TimeUnit.MILLISECONDS)
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(aLong -> {

                    navTo();
                });
    }

    private void navTo() {
        Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (!AppDataManager.isMigratedWhenV300()) {
            //firebase - event -login
            Bundle eventBundle = new Bundle();
            eventBundle.putString(FirebaseAnalytics.Param.METHOD, "DataMigrationActivity");
            FirebaseAnalytics.getInstance(this).logEvent(FirebaseAnalytics.Event.APP_OPEN, eventBundle);

            ActivityUtils.startActivity(DataMigrationActivity.class);
        } else if (curAccount == null || !curAccount.hasValidToken()) {
            Bundle eventBundle = new Bundle();
            eventBundle.putString(FirebaseAnalytics.Param.METHOD, "AccountsActivity");
            FirebaseAnalytics.getInstance(this).logEvent(FirebaseAnalytics.Event.APP_OPEN, eventBundle);


            Intent newIntent = new Intent(this, AccountsActivity.class);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            ActivityUtils.startActivity(newIntent);
        } else {
            Bundle eventBundle = new Bundle();
            eventBundle.putString(FirebaseAnalytics.Param.METHOD, "MainActivity");
            FirebaseAnalytics.getInstance(this).logEvent(FirebaseAnalytics.Event.APP_OPEN, eventBundle);


            ActivityUtils.startActivity(MainActivity.class);
        }

        finish();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();

        if (disposable != null) {
            disposable.dispose();
            disposable = null;
        }
    }
}
