package com.seafile.seadroid;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.List;

import android.app.Activity;
import android.os.Bundle;
import android.os.AsyncTask;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.AdapterView;
import android.widget.AdapterView.OnItemClickListener;
import android.widget.Button;
import android.widget.EditText;
import android.widget.ListView;
import android.widget.TextView;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.util.Log;


public class StartActivity extends Activity {

    private static final String DEBUG_TAG = "StartActivity";
    
    private ListView accountsView;
    
    private AccountManager accountManager;
    private Account defaultAccount;
    
    private AccountAdapter adapter;
    List<Account> accounts;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.start);
        
        accountsView = (ListView) findViewById(R.id.account_list_view);
        
        accountManager = new AccountManager(this);
        defaultAccount = accountManager.getDefaultAccount();
        accounts = accountManager.getAccountList();
        
        Log.d(DEBUG_TAG, "Load accounts num " + accounts.size());
        adapter = new AccountAdapter(this);
        for (Account a : accounts) {
            adapter.add(a);
        }
        Button addAccount = new Button(this);
        addAccount.setText(R.string.add_account);
        accountsView.addFooterView(addAccount, null, true);
        accountsView.setFooterDividersEnabled(false);
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
    }

    private void startFilesActivity(Account account) {
        Intent intent = new Intent(this, BrowserActivity.class);
        intent.putExtra("server", account.server);
        intent.putExtra("email", account.email);
        intent.putExtra("token", account.token);
        startActivity(intent);
    }
    
    private void startAccountDetailActivity() {
        Intent intent = new Intent(this, AccountDetailActivity.class);
        startActivity(intent);
    }

}
