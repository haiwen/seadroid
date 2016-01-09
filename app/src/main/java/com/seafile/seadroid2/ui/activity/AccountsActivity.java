package com.seafile.seadroid2.ui.activity;

import android.accounts.AccountManagerCallback;
import android.accounts.AccountManagerFuture;
import android.accounts.OnAccountsUpdateListener;
import android.content.*;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.app.DialogFragment;
import android.support.v7.app.ActionBar;
import android.support.v7.app.AlertDialog;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.avatar.Avatar;
import com.seafile.seadroid2.avatar.AvatarManager;
import com.seafile.seadroid2.monitor.FileMonitorService;
import com.seafile.seadroid2.ui.adapter.AccountAdapter;
import com.seafile.seadroid2.ui.adapter.SeafAccountAdapter;
import com.seafile.seadroid2.util.Utils;

import java.util.ArrayList;
import java.util.List;


public class AccountsActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener{
    private static final String DEBUG_TAG = "AccountsActivity";

    public static final int DETAIL_ACTIVITY_REQUEST = 1;

    private ListView accountsView;

    private android.accounts.AccountManager mAccountManager;
    private AccountManager accountManager;
    private AvatarManager avatarManager;
    private AccountAdapter adapter;
    private List<Account> accounts;
    private FileMonitorService mMonitorService;
    private Account currentDefaultAccount;

    private OnAccountsUpdateListener accountsUpdateListener = new OnAccountsUpdateListener() {
        @Override
        public void onAccountsUpdated(android.accounts.Account[] accounts) {
            refreshView();
        }
    };

    private ServiceConnection mMonitorConnection = new ServiceConnection() {

        @Override
        public void onServiceConnected(ComponentName className, IBinder binder) {
            FileMonitorService.MonitorBinder monitorBinder = (FileMonitorService.MonitorBinder)binder;
            mMonitorService = monitorBinder.getService();
        }

        @Override
        public void onServiceDisconnected(ComponentName className) {
            mMonitorService = null;
        }

    };

    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "AccountsActivity.onCreate is called");
        super.onCreate(savedInstanceState);
        setContentView(R.layout.start);

        mAccountManager = android.accounts.AccountManager.get(this);
        accountsView = (ListView) findViewById(R.id.account_list_view);
        accountManager = new AccountManager(this);
        avatarManager = new AvatarManager();
        currentDefaultAccount = accountManager.getCurrentAccount();

        View footerView = ((LayoutInflater) this
                .getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(
                R.layout.account_list_footer, null, false);
        Button addAccount = (Button) footerView.findViewById(R.id.account_footer_btn);
        addAccount.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View btn) {
                 mAccountManager.addAccount(Account.ACCOUNT_TYPE,
                         Authenticator.AUTHTOKEN_TYPE, null, null,
                         AccountsActivity.this, accountCallback, null);
            }
        });
        accountsView.addFooterView(footerView, null, true);
        accountsView.setFooterDividersEnabled(false);
        adapter = new SeafAccountAdapter(this);
        accountsView.setAdapter(adapter);
        accountsView.setOnItemClickListener(new OnItemClickListener() {
            public void onItemClick(AdapterView<?> parent, View view, int position,
                    long id) {

                Account account = accounts.get(position);
                if (!account.hasValidToken()) {
                    // user already signed out, input password first
                    startEditAccountActivity(account);
                } else {
                    // update current Account info from SharedPreference
                    accountManager.saveCurrentAccount(account.getSignature());
                    startFilesActivity();
                }
            }
        });

        mAccountManager.addOnAccountsUpdatedListener(accountsUpdateListener, null, false);

        registerForContextMenu(accountsView);

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.accounts);
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
                if (accountManager.getCurrentAccount() == null) {
                    Intent intent = new Intent(this, BrowserActivity.class);
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
        accounts = accountManager.getAccountList();
        adapter.clear();
        adapter.setItems(accounts);

        // if the user switched default account while we were in background,
        // switch to BrowserActivity
        Account newCurrentAccount = accountManager.getCurrentAccount();
        if (newCurrentAccount != null && !newCurrentAccount.equals(currentDefaultAccount)) {
            startFilesActivity();
        }

        loadAvatarUrls(48);

        adapter.notifyChanged();
    }

    private void startFilesActivity() {
        Intent intent = new Intent(this, BrowserActivity.class);

        // first finish this activity, so the BrowserActivity is again "on top"
        finish();
        startActivity(intent);
    }

    AccountManagerCallback<Bundle> accountCallback = new AccountManagerCallback<Bundle>() {

        @Override
        public void run(AccountManagerFuture<Bundle> future) {
            if (future.isCancelled())
                return;

            try {
                Bundle b = future.getResult();

                if (b.getBoolean(android.accounts.AccountManager.KEY_BOOLEAN_RESULT)) {
                    String accountName = b.getString(android.accounts.AccountManager.KEY_ACCOUNT_NAME);
                    Log.d(DEBUG_TAG, "switching to account " + accountName);
                    accountManager.saveCurrentAccount(accountName);
                    startFilesActivity();
                }
            } catch (Exception e) {
                Log.e(DEBUG_TAG, "unexpected error: " + e);
            }
        }
    };

    private void startEditAccountActivity(Account account) {
        mAccountManager.updateCredentials(account.getAndroidAccount(), Authenticator.AUTHTOKEN_TYPE, null, this, accountCallback, null);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
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
    public void onCreateContextMenu(ContextMenu menu, View v,
            ContextMenuInfo menuInfo) {
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
            account = adapter.getItem((int)info.id);
            startEditAccountActivity(account);
            return true;
        case R.id.delete:
            account = adapter.getItem((int)info.id);

            Log.d(DEBUG_TAG, "removing account "+account);
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
        Account account = accountManager.getCurrentAccount();
        if (account == null) {
            // force exit when current account was deleted
            Intent i = new Intent(this, BrowserActivity.class);
            i.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(i);
            finish();
        }
        super.onBackPressed();
    }

    /**
     * asynchronously load avatars
     *
     * @param avatarSize set a avatar size in one of 24*24, 32*32, 48*48, 64*64, 72*72, 96*96
     */
    public void loadAvatarUrls(int avatarSize) {
        List<Avatar> avatars;

        if (!Utils.isNetworkOn() || !avatarManager.isNeedToLoadNewAvatars()) {
            // Toast.makeText(AccountsActivity.this, getString(R.string.network_down), Toast.LENGTH_SHORT).show();

            // use cached avatars
            avatars = avatarManager.getAvatarList();

            if (avatars == null) {
                return;
            }

            // set avatars url to adapter
            adapter.setAvatars((ArrayList<Avatar>) avatars);

            // notify adapter data changed
            adapter.notifyDataSetChanged();

            return;
        }

        LoadAvatarUrlsTask task = new LoadAvatarUrlsTask(avatarSize);

        ConcurrentAsyncTask.execute(task);

    }

    private class LoadAvatarUrlsTask extends AsyncTask<Void, Void, List<Avatar>> {

        private List<Avatar> avatars;
        private int avatarSize;
        private SeafConnection httpConnection;

        public LoadAvatarUrlsTask(int avatarSize) {
            this.avatarSize = avatarSize;
            this.avatars = Lists.newArrayList();
        }

        @Override
        protected List<Avatar> doInBackground(Void... params) {
            // reuse cached avatars
            avatars = avatarManager.getAvatarList();

            // contains accounts who don`t have avatars yet
            List<Account> acts = avatarManager.getAccountsWithoutAvatars();

            // contains new avatars in order to persist them to database
            List<Avatar> newAvatars = new ArrayList<Avatar>(acts.size());

            // load avatars from server
            for (Account account : acts) {
                httpConnection = new SeafConnection(account);

                String avatarRawData = null;
                try {
                    avatarRawData = httpConnection.getAvatar(account.getEmail(), avatarSize);
                } catch (SeafException e) {
                    e.printStackTrace();
                    return avatars;
                }

                Avatar avatar = avatarManager.parseAvatar(avatarRawData);
                if (avatar == null)
                    continue;

                avatar.setSignature(account.getSignature());

                avatars.add(avatar);

                newAvatars.add(avatar);
            }

            // save new added avatars to database
            avatarManager.saveAvatarList(newAvatars);

            return avatars;
        }

        @Override
        protected void onPostExecute(List<Avatar> avatars) {
            if (avatars == null) {
                return;
            }

            // set avatars url to adapter
            adapter.setAvatars((ArrayList<Avatar>) avatars);

            // notify adapter data changed
            adapter.notifyDataSetChanged();
        }
    }

}
