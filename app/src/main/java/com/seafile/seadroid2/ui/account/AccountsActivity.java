package com.seafile.seadroid2.ui.account;

import android.accounts.AccountManagerCallback;
import android.accounts.AccountManagerFuture;
import android.accounts.OnAccountsUpdateListener;
import android.content.Intent;
import android.os.Bundle;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.Button;
import android.widget.ListView;

import androidx.activity.OnBackPressedCallback;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.ActivityUtils;
import com.blankj.utilcode.util.AppUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.ServerInfo;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.account.AccountUtils;
import com.seafile.seadroid2.listener.OnCallback;
import com.seafile.seadroid2.ui.SplashActivity;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.PolicyDialogFragment;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.ui.account.adapter.AccountAdapter;
import com.seafile.seadroid2.ui.main.MainActivity;

import java.util.List;
import java.util.Locale;

public class AccountsActivity extends BaseActivityWithVM<AccountViewModel> implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "AccountsActivity";
    public static final int DETAIL_ACTIVITY_REQUEST = 1;
    public static final String FROM_THIS = AccountsActivity.class.getName();

    private ListView accountsView;

    private android.accounts.AccountManager mAccountManager;
    private AccountAdapter adapter;
    private List<Account> accounts;
    private Account currentDefaultAccount;

    private final OnAccountsUpdateListener accountsUpdateListener = new OnAccountsUpdateListener() {
        @Override
        public void onAccountsUpdated(android.accounts.Account[] accounts) {
            refreshView();
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

        accounts = SupportAccountManager.getInstance().getAccountList();
        currentDefaultAccount = SupportAccountManager.getInstance().getCurrentAccount();

        View footerView = getLayoutInflater().inflate(R.layout.account_list_footer, null, false);

        Button addAccount = (Button) footerView.findViewById(R.id.account_footer_btn);
        addAccount.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View btn) {
                mAccountManager.addAccount(Constants.Account.ACCOUNT_TYPE,
                        Authenticator.AUTHTOKEN_TYPE, null, null,
                        AccountsActivity.this, accountCallback, null);
            }
        });

        accountsView = (ListView) findViewById(R.id.account_list_view);
        accountsView.addFooterView(footerView, null, true);
        accountsView.setFooterDividersEnabled(false);

        adapter = new AccountAdapter(this);
        accountsView.setAdapter(adapter);
        accountsView.setOnItemClickListener((parent, view, position, id) -> onListItemClick(position));

        mAccountManager.addOnAccountsUpdatedListener(accountsUpdateListener, null, false);

        registerForContextMenu(accountsView);

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);

        // updates toolbar back button
        if (currentDefaultAccount == null || !currentDefaultAccount.hasValidToken()) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(false);
        } else {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        }

        getSupportActionBar().setTitle(R.string.accounts);


        initOnBackPressedDispatcher();
        initViewModel();

        String country = Locale.getDefault().getCountry();
        String language = Locale.getDefault().getLanguage();

        int privacyPolicyConfirmed = AppDataManager.getPrivacyPolicyConfirmed();
        if (country.equals("CN") && language.equals("zh") && (privacyPolicyConfirmed == 0)) {
            showDialog();
        }
    }

    private void initOnBackPressedDispatcher() {
        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                finish();
            }
        });
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    showLoadingDialog();
                } else {
                    dismissLoadingDialog();
                }
            }
        });

        getViewModel().getServerInfoLiveData().observe(this, new Observer<ServerInfo>() {
            @Override
            public void onChanged(ServerInfo serverInfo) {
                //finish main firstly
                ActivityUtils.finishActivity(MainActivity.class);

                //start splash
                startActivity(new Intent(AccountsActivity.this, SplashActivity.class));
                finish();
                overridePendingTransition(R.anim.fade_in, R.anim.fade_out);
            }
        });
    }

    private void onListItemClick(int position) {
        Account clickedAccount = accounts.get(position);

        Account loggedAccount = SupportAccountManager.getInstance().getCurrentAccount();

        if (clickedAccount.is_checked && loggedAccount != null) {
            finish();
            return;
        }

        if (!clickedAccount.hasValidToken()) {
            // user already signed out, input password first
            startEditAccountActivity(clickedAccount);
        } else {

            //save
            SupportAccountManager.getInstance().saveCurrentAccount(clickedAccount.getSignature());

            //switch account
            AccountUtils.switchAccount(clickedAccount);

            startFilesActivity();
        }
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy");
        super.onDestroy();

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
        adapter.notifyChanged();

        Account newCurrentAccount = SupportAccountManager.getInstance().getCurrentAccount();

        // updates toolbar back button
        if (newCurrentAccount == null || !newCurrentAccount.hasValidToken()) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(false);

            //close main activity
            ActivityUtils.finishActivity(MainActivity.class);
        }

//        // if the user switched default account while we were in background,
//        // switch to BrowserActivity
//        if (newCurrentAccount != null && !newCurrentAccount.equals(currentDefaultAccount)) {
//            startFilesActivity();
//        }
    }

    private void startFilesActivity() {
        getViewModel().getServerInfo();
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
        if (item.getItemId() == R.id.edit) {
            account = adapter.getItem((int) info.id);
            startEditAccountActivity(account);
            return true;
        } else if (item.getItemId() == R.id.delete) {
            account = adapter.getItem((int) info.id);

            getViewModel().deleteAccount(account);
            Log.d(DEBUG_TAG, "removing account " + account);
            return true;
        }
        return super.onContextItemSelected(item);
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
                AppDataManager.savePrivacyPolicyConfirmed(1);
            }
        });
        dialogFragment.show(getSupportFragmentManager(), PolicyDialogFragment.class.getSimpleName());
    }
}
