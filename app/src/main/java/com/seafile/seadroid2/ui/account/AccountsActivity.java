package com.seafile.seadroid2.ui.account;

import android.accounts.AccountManagerCallback;
import android.accounts.AccountManagerFuture;
import android.accounts.OnAccountsUpdateListener;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.Bundle;
import android.os.IBinder;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.webkit.CookieManager;
import android.webkit.ValueCallback;
import android.widget.AdapterView;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;

import androidx.appcompat.widget.Toolbar;

import com.blankj.utilcode.util.AppUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.listener.OnCallback;
import com.seafile.seadroid2.ui.dialog_fragment.PolicyDialogFragment;
import com.seafile.seadroid2.util.sp.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.monitor.FileMonitorService;
import com.seafile.seadroid2.ui.BaseActivity;
import com.seafile.seadroid2.ui.account.adapter.AccountAdapter;
import com.seafile.seadroid2.ui.main.MainActivity;

import java.util.List;
import java.util.Locale;


public class AccountsActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "AccountsActivity";

    public static final int DETAIL_ACTIVITY_REQUEST = 1;

    private ListView accountsView;

    private android.accounts.AccountManager mAccountManager;
    private AccountAdapter adapter;
    private List<Account> accounts;
    private FileMonitorService mMonitorService;
    private Account currentDefaultAccount;

    private final OnAccountsUpdateListener accountsUpdateListener = new OnAccountsUpdateListener() {
        @Override
        public void onAccountsUpdated(android.accounts.Account[] accounts) {
            refreshView();
        }
    };

    private final ServiceConnection mMonitorConnection = new ServiceConnection() {

        @Override
        public void onServiceConnected(ComponentName className, IBinder binder) {
            FileMonitorService.MonitorBinder monitorBinder = (FileMonitorService.MonitorBinder) binder;
            mMonitorService = monitorBinder.getService();
        }

        @Override
        public void onServiceDisconnected(ComponentName className) {
            mMonitorService = null;
        }

    };

    private final AccountManagerCallback<Bundle> accountCallback = new AccountManagerCallback<Bundle>() {

        @Override
        public void run(AccountManagerFuture<Bundle> future) {
            if (future.isCancelled())
                return;

            try {
                Bundle b = future.getResult();

                if (b.getBoolean(android.accounts.AccountManager.KEY_BOOLEAN_RESULT)) {
                    String accountName = b.getString(android.accounts.AccountManager.KEY_ACCOUNT_NAME);
                    Log.d(DEBUG_TAG, "switching to account " + accountName);
                    SupportAccountManager.getInstance().saveCurrentAccount(accountName);
                    startFilesActivity();
                }
            } catch (Exception e) {
                Log.e(DEBUG_TAG, "unexpected error: " + e);
            }
        }
    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "AccountsActivity.onCreate is called");
        super.onCreate(savedInstanceState);
        setContentView(R.layout.start);
        mAccountManager = android.accounts.AccountManager.get(this);
        accountsView = (ListView) findViewById(R.id.account_list_view);

        currentDefaultAccount = SupportAccountManager.getInstance().getCurrentAccount();

        View footerView = ((LayoutInflater) this
                .getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(
                R.layout.account_list_footer, null, false);

        Button addAccount = (Button) footerView.findViewById(R.id.account_footer_btn);
        addAccount.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View btn) {
                mAccountManager.addAccount(Constants.Account.ACCOUNT_TYPE,
                        Authenticator.AUTHTOKEN_TYPE, null, null,
                        AccountsActivity.this, accountCallback, null);
            }
        });

        accountsView.addFooterView(footerView, null, true);
        accountsView.setFooterDividersEnabled(false);
        adapter = new AccountAdapter(this);
        accountsView.setAdapter(adapter);
        accountsView.setOnItemClickListener(new OnItemClickListener() {
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {

                Account account = accounts.get(position);
                if (!account.hasValidToken()) {
                    // user already signed out, input password first
                    startEditAccountActivity(account);
                } else {
                    // update current Account info from SharedPreference
                    SupportAccountManager.getInstance().saveCurrentAccount(account.getSignature());
                    startFilesActivity();
                }
            }
        });

        mAccountManager.addOnAccountsUpdatedListener(accountsUpdateListener, null, false);

        registerForContextMenu(accountsView);

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);

        accounts = SupportAccountManager.getInstance().getAccountList();
        // updates toolbar back button
        if (currentDefaultAccount == null || !currentDefaultAccount.hasValidToken()) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(false);
        } else {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        }

        getSupportActionBar().setTitle(R.string.accounts);

        String country = Locale.getDefault().getCountry();
        String language = Locale.getDefault().getLanguage();
        int privacyPolicyConfirmed = SettingsManager.getInstance().getPrivacyPolicyConfirmed();
        if (country.equals("CN") && language.equals("zh") && (privacyPolicyConfirmed == 0)) {
            showDialog();
        }
    }

    @Override
    public void onStart() {
        Log.d(DEBUG_TAG, "onStart");
        super.onStart();
        Intent bIntent = new Intent(this, FileMonitorService.class);
        bindService(bIntent, mMonitorConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    public void onStop() {
        Log.d(DEBUG_TAG, "onStop");
        super.onStop();
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
        super.onDestroy();
        if (mMonitorService != null) {
            unbindService(mMonitorConnection);
            mMonitorService = null;
        }
        mAccountManager.removeOnAccountsUpdatedListener(accountsUpdateListener);
    }

    // Always reload accounts on resume, so that when user add a new account,
    // it will be shown.
    @Override
    public void onResume() {
        Log.d(DEBUG_TAG, "onResume");
        super.onResume();

        refreshView();
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                // if the current account sign out and no account was to logged in,
                // then always goes to AccountsActivity
                if (SupportAccountManager.getInstance().getCurrentAccount() == null) {
                    Intent intent = new Intent(this, MainActivity.class);
                    intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                    startActivity(intent);
                }

                this.finish();
            default:
                return super.onOptionsItemSelected(item);
        }
    }

    private void refreshView() {
        Log.d(DEBUG_TAG, "refreshView");
        accounts = SupportAccountManager.getInstance().getAccountList();
        adapter.clear();
        adapter.setItems(accounts);

        // if the user switched default account while we were in background,
        // switch to BrowserActivity
        Account newCurrentAccount = SupportAccountManager.getInstance().getCurrentAccount();
        if (newCurrentAccount != null && !newCurrentAccount.equals(currentDefaultAccount)) {
            startFilesActivity();
        }

        // updates toolbar back button
        if (newCurrentAccount == null || !newCurrentAccount.hasValidToken()) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(false);
        }

        adapter.notifyChanged();
    }

    private void startFilesActivity() {

        removeAllCookie();

        Intent intent = new Intent(this, MainActivity.class);

        // first finish this activity, so the BrowserActivity is again "on top"
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
        startActivity(intent);
        finish();
    }

    private void removeAllCookie() {
        CookieManager cookieManager = CookieManager.getInstance();
        cookieManager.removeAllCookies(new ValueCallback<Boolean>() {
            @Override
            public void onReceiveValue(Boolean value) {
                Log.d(AccountsActivity.class.getSimpleName(), "removeAllCookie? " + value);
            }
        });
    }


    private void startEditAccountActivity(Account account) {
        mAccountManager.updateCredentials(account.getAndroidAccount(), Authenticator.AUTHTOKEN_TYPE, null, this, accountCallback, null);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        switch (requestCode) {
            case DETAIL_ACTIVITY_REQUEST:
                if (resultCode == RESULT_OK) {
                    startFilesActivity();
                }
                break;
            default:
                break;
        }
    }

    @Override
    public void onCreateContextMenu(ContextMenu menu, View v, ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);
        android.view.MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.account_menu, menu);
    }

    @Override
    public boolean onContextItemSelected(android.view.MenuItem item) {
        AdapterContextMenuInfo info = (AdapterContextMenuInfo) item.getMenuInfo();
        Account account;
        switch (item.getItemId()) {
            case R.id.edit:
                account = adapter.getItem((int) info.id);
                startEditAccountActivity(account);
                return true;
            case R.id.delete:
                //TODO 清除传输队列数据库

                account = adapter.getItem((int) info.id);

                Log.d(DEBUG_TAG, "removing account " + account);
                mAccountManager.removeAccount(account.getAndroidAccount(), null, null);

                if (mMonitorService != null) {
                    mMonitorService.removeAccount(account);
                }
                return true;
            default:
                return super.onContextItemSelected(item);
        }
    }

    @Override
    public void onBackPressed() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account != null) {
            // force exit when current account was deleted
            Intent i = new Intent(this, MainActivity.class);
            i.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(i);
            finish();
        } else
            super.onBackPressed();
    }


    private void showDialog() {
        PolicyDialogFragment dialogFragment = new PolicyDialogFragment();
        dialogFragment.setOnCallback(new OnCallback() {
            @Override
            public void onFailed() {
                AppUtils.exitApp();
            }

            @Override
            public void onSuccess() {
                SettingsManager.getInstance().savePrivacyPolicyConfirmed(1);
            }
        });
        dialogFragment.show(getSupportFragmentManager(),PolicyDialogFragment.class.getSimpleName());
    }
}
