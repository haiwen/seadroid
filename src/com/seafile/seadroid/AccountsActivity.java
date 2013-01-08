package com.seafile.seadroid;

import java.util.List;

import com.seafile.seadroid.account.Account;
import com.seafile.seadroid.account.AccountManager;

import android.app.Activity;
import android.os.Bundle;
import android.view.ContextMenu;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.widget.AdapterView;
import android.widget.AdapterView.AdapterContextMenuInfo;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.ListView;
import android.content.Intent;
import android.util.Log;


public class AccountsActivity extends Activity {

    private static final String DEBUG_TAG = "StartActivity";
    
    private ListView accountsView;
    
    private AccountManager accountManager;
    
    private AccountAdapter adapter;
    List<Account> accounts;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.start);
        
        accountsView = (ListView) findViewById(R.id.account_list_view);
        
        accountManager = new AccountManager(this);

        Button addAccount = new Button(this);
        addAccount.setText(R.string.add_account);
        accountsView.addFooterView(addAccount, null, true);
        accountsView.setFooterDividersEnabled(false);
        adapter = new AccountAdapter(this);
        accountsView.setAdapter(adapter);
        addAccount.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View btn) {
                startAccountDetailActivity();
            }
        });
        accountsView.setOnItemClickListener(new OnItemClickListener() {
            public void onItemClick(AdapterView<?> parent, View view, int position,
                    long id) {
                Account account = accounts.get(position);
                startFilesActivity(account);
            }
        });
        registerForContextMenu(accountsView);
    }
    
    // Always reload accounts on resume, so that when user add a new account,
    // it will be shown.
    @Override
    public void onResume() {
        super.onResume();
        
        refreshView();
    }
    
    private void refreshView() {
        accounts = accountManager.getAccountList();
        // Log.d(DEBUG_TAG, "Load accounts num " + accounts.size());
        adapter.clear();
        for (Account a : accounts) {
            adapter.add(a);
        }
        adapter.notifyChanged();
    }

    private void startFilesActivity(Account account) {
        Intent intent = new Intent(this, BrowserActivity.class);
        intent.putExtra("server", account.server);
        intent.putExtra("email", account.email);
        intent.putExtra("token", account.token);
        startActivity(intent);
        finish();
    }
    
    private void startAccountDetailActivity() {
        Intent intent = new Intent(this, AccountDetailActivity.class);
        startActivity(intent);
    }
    
    private void startEditAccountActivity(Account account) {
        Intent intent = new Intent(this, AccountDetailActivity.class);
        intent.putExtra("server", account.server);
        intent.putExtra("email", account.email);
        startActivity(intent);
    }
    
    @Override
    public void onCreateContextMenu(ContextMenu menu, View v,
            ContextMenuInfo menuInfo) {
        super.onCreateContextMenu(menu, v, menuInfo);
        MenuInflater inflater = getMenuInflater();
        inflater.inflate(R.menu.account_menu, menu);
    }

    @Override
    public boolean onContextItemSelected(MenuItem item) {
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
            refreshView();
            return true;
        default:
            return super.onContextItemSelected(item);
        }
    }
    
}
