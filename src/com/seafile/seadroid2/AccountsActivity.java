package com.seafile.seadroid2;

import java.util.List;

import android.app.Dialog;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.SharedPreferences;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.app.DialogFragment;
import android.util.Log;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.monitor.FileMonitorService;
import com.seafile.seadroid2.sync.CameraUploadService;
import com.seafile.seadroid2.ui.SeafileStyleDialogBuilder;


public class AccountsActivity extends SherlockFragmentActivity {
    public static final String SHARED_PREF_NAME = "latest_account";
    public static final String SHARED_PREF_SERVER_KEY = "com.seafile.seadroid.server";
    public static final String SHARED_PREF_EMAIL_KEY = "com.seafile.seadroid.email";
    public static final String SHARED_PREF_TOKEN_KEY = "com.seafile.seadroid.token";

    private static final String DEBUG_TAG = "AccountsActivity";

    private static AccountsActivity accountsActivity;

    private ListView accountsView;

    private AccountManager accountManager;
    private AccountAdapter adapter;
    List<Account> accounts;
    private FileMonitorService mMonitorService;
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
        accountsActivity = this;

        accountsView = (ListView) findViewById(R.id.account_list_view);
        accountManager = new AccountManager(this);
       
        View footerView = ((LayoutInflater) this
                .getSystemService(Context.LAYOUT_INFLATER_SERVICE)).inflate(
                R.layout.account_list_footer, null, false);
        Button addAccount = (Button) footerView.findViewById(R.id.account_footer_btn);
        addAccount.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View btn) {
                new CreateAccountChoiceDialog().show(getSupportFragmentManager(), "Choose a server");
            }
        });
        accountsView.addFooterView(footerView, null, true);
        accountsView.setFooterDividersEnabled(false);
        adapter = new AccountAdapter(this);
        accountsView.setAdapter(adapter);
        accountsView.setOnItemClickListener(new OnItemClickListener() {
            public void onItemClick(AdapterView<?> parent, View view, int position,
                    long id) {

                Account account = accounts.get(position);
                startFilesActivity(account);
            }
        });
        registerForContextMenu(accountsView);

        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayHomeAsUpEnabled(true);
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
        accounts = accountManager.getAccountList();
        /*AvatarManager avatarManager = new AvatarManager(accounts);*/
        // Log.d(DEBUG_TAG, "Load accounts num " + accounts.size());
        adapter.clear();
        /*for (Account a : accounts) {
            adapter.add(a);
        }*/
        adapter.setItems(accounts);
        adapter.notifyChanged();
    }

    private void writeToSharedPreferences(Account account) {
        SharedPreferences sharedPref = getSharedPreferences(SHARED_PREF_NAME, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString(SHARED_PREF_SERVER_KEY, account.server);
        editor.putString(SHARED_PREF_EMAIL_KEY, account.email);
        editor.putString(SHARED_PREF_TOKEN_KEY, account.token);
        editor.commit();
    }

    private void clearDataFromSharedPreferences(Account account) {
        SharedPreferences sharedPref = getSharedPreferences(SHARED_PREF_NAME, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();
        
        String latestServer = sharedPref.getString(SHARED_PREF_SERVER_KEY, null);
        String latestEmail = sharedPref.getString(SHARED_PREF_EMAIL_KEY, null);
        // update cache data of settings module
        String settingsServer = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_SERVER, null);
        String settingsEmail = sharedPref.getString(SettingsManager.SHARED_PREF_CAMERA_UPLOAD_ACCOUNT_EMAIL, null);
        
        if (account.server.equals(latestServer) && account.email.equals(latestEmail)) {
            editor.putString(SHARED_PREF_SERVER_KEY, null);
            editor.putString(SHARED_PREF_EMAIL_KEY, null);
            editor.putString(SHARED_PREF_TOKEN_KEY, null);
            editor.commit();
        }
        if (account.server.equals(settingsServer) && account.email.equals(settingsEmail)) {
            SettingsManager.instance().clearCameraUploadInfo();
        }
    }

    private void startFilesActivity(Account account) {
        Intent intent = new Intent(this, BrowserActivity.class);
        intent.putExtra("server", account.server);
        intent.putExtra("email", account.email);
        intent.putExtra("token", account.token);

        writeToSharedPreferences(account);

        startActivity(intent);
        finish();
    }

    private void startEditAccountActivity(Account account) {
        Intent intent = new Intent(this, AccountDetailActivity.class);
        intent.putExtra("server", account.server);
        intent.putExtra("email", account.email);
        intent.putExtra("isEdited", true);
        startActivity(intent);
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
            accountManager.deleteAccount(account);
            // stop camera upload service
            if (SettingsManager.instance().getCameraUploadAccountEmail()
                    .equals(account.getEmail())
                    && SettingsManager.instance()
                            .getCameraUploadAccountServer()
                            .equals(account.getServer())) {
                Intent cameraUploadIntent = new Intent(this,
                        CameraUploadService.class);
                stopService(cameraUploadIntent);
            }
            if (mMonitorService != null) {
                mMonitorService.removeAccount(account);
            }
            clearDataFromSharedPreferences(account);

            refreshView();
            return true;
        default:
            return super.onContextItemSelected(item);
        }
    }

    @Override
    public void onBackPressed() {
        super.onBackPressed();
    }

    public static final int PRIVATE_SERVER = 0;
    public static final int SEACLOUD_CC = 1;
    public static final int CLOUD_SEAFILE_COM = 2;

    public static class CreateAccountChoiceDialog extends DialogFragment {
        // final Context context = SeadroidApplication.getAppContext();
        @Override
        public Dialog onCreateDialog(Bundle savedInstanceState) {
            SeafileStyleDialogBuilder builder = 
                    new SeafileStyleDialogBuilder(getActivity()).
                    setTitle(getResources().getString(R.string.choose_server)).
                    setItems(R.array.choose_server_array,
                            new DialogInterface.OnClickListener() {
                                @Override
                                public void onClick(DialogInterface dialog, int which) {
                                    Intent intent;
                                    switch (which) {
                                    case PRIVATE_SERVER:
                                        intent = new Intent(getActivity(), AccountDetailActivity.class);
                                        startActivity(intent);
                                        break;
                                    case SEACLOUD_CC:
                                        intent = new Intent(getActivity(), AccountDetailActivity.class);
                                        intent.putExtra("server", "https://seacloud.cc");
                                        startActivity(intent);
                                        break;
                                    case CLOUD_SEAFILE_COM:
                                        intent = new Intent(getActivity(), AccountDetailActivity.class);
                                        intent.putExtra("server", "https://cloud.seafile.com");
                                        startActivity(intent);
                                        break;
                                    default:
                                        return;
                                    }
                                    accountsActivity.finish();
                                }
                            });
            return builder.show();
        }
    }
}
