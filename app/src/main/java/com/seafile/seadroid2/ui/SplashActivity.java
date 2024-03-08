package com.seafile.seadroid2.ui;

import android.content.Intent;
import android.os.Bundle;

import com.blankj.utilcode.util.ActivityUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.data_migrate.DataMigrationActivity;
import com.seafile.seadroid2.ui.main.MainActivity;
import com.seafile.seadroid2.util.sp.AppSPs;

import java.util.concurrent.TimeUnit;

import io.reactivex.Observable;
import io.reactivex.android.schedulers.AndroidSchedulers;
import io.reactivex.disposables.Disposable;

public class SplashActivity extends BaseActivity {
    private Disposable disposable;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.activity_splash);

        long duration = 500;
        disposable = Observable.timer(duration, TimeUnit.MILLISECONDS)
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(aLong -> {

                    AppSPs.isMigratedWhenV300();

                    Account curAccount = SupportAccountManager.getInstance().getCurrentAccount();
                    if (curAccount == null || !curAccount.hasValidToken()) {
                        Intent newIntent = new Intent(this, AccountsActivity.class);
                        newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                        newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                        ActivityUtils.startActivity(newIntent);
                    }
//                    else if (!AppSPs.isMigratedWhenV300()) {
//                        ActivityUtils.startActivity(DataMigrationActivity.class);
//                    }
                    else {
                        ActivityUtils.startActivity(MainActivity.class);
                    }
                    finish();
                });
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
