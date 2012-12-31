package com.seafile.seadroid;

import java.net.MalformedURLException;
import java.net.URL;

import com.seafile.seadroid.account.Account;
import com.seafile.seadroid.account.AccountManager;

import android.app.Activity;
import android.os.Bundle;
import android.os.AsyncTask;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;
import android.widget.TextView;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.util.Log;


public class AccountDetailActivity extends Activity {

    private static final String DEBUG_TAG = "AddAccountActivity";
    
    private TextView statusView;
    private Button loginButton;
    private EditText serverText;
    private EditText emailText;
    private EditText passwdText;
    
    private AccountManager accountManager;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.account_detail);
        
        statusView = (TextView) findViewById(R.id.status_view);
        loginButton = (Button) findViewById(R.id.login_button);
        serverText = (EditText) findViewById(R.id.server_url);
        emailText = (EditText) findViewById(R.id.email_address);
        passwdText = (EditText) findViewById(R.id.password);
        
        accountManager = new AccountManager(this);
    }
    
    private String cleanServerURL(String serverURL) throws MalformedURLException {
        if (!serverURL.startsWith("http://") && !serverURL.startsWith("https://")) {
            serverURL = "http://" + serverURL;
        }
        
        if (!serverURL.endsWith("/")) {
            serverURL = serverURL + "/";
        }
        
        new URL(serverURL); // will throw MalformedURLException if serverURL not valid
        return serverURL;
    }

    /** Called when the user clicks the Login button */
    public void login(View view) {
        String serverURL = serverText.getText().toString();
        String email = emailText.getText().toString();
        String passwd = passwdText.getText().toString();
        
        ConnectivityManager connMgr = (ConnectivityManager) 
            getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();

        if (networkInfo != null && networkInfo.isConnected()) {
            if (serverURL.length() == 0)
                return;
            try {
                serverURL = cleanServerURL(serverURL);
            } catch (MalformedURLException e) {
                Log.d(DEBUG_TAG, "Invalid URL " + serverURL);
                return;
            }
            
            loginButton.setEnabled(false);
            Account tmpAccount = new Account(serverURL, email, passwd);
            new LoginTask(tmpAccount).execute();
        } else {
            statusView.setText("No network connection available.");
        }
    }

    private void startFilesActivity(Account account) {
        Intent intent = new Intent(this, BrowserActivity.class);
        intent.putExtra("server", account.server);
        intent.putExtra("email", account.email);
        intent.putExtra("token", account.token);
        startActivity(intent);
        finish(); // so the user will not return to this activity when press 'back'
    }
    
    private class LoginTask extends AsyncTask<Void, Void, String> {
        
        Account loginAccount;
        
        public LoginTask(Account loginAccount) {
            this.loginAccount = loginAccount;
        }
        
        @Override
        protected String doInBackground(Void... params) {              
            if (params.length != 0)
                return "Error number of parameter";

            return doLogin();
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(String result) {
            if (result.equals("Success")) {
                accountManager.saveDefaultAccount(loginAccount);
                startFilesActivity(loginAccount);
            } else {
                statusView.setText(result);
            }
            loginButton.setEnabled(true);
        }

        private String doLogin() {   
            SeafConnection sc = new SeafConnection(loginAccount);
            
            if (sc.doLogin() == false)
                return "Login failed";
            return "Success";
        }
    }

}
