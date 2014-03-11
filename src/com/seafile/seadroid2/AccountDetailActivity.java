package com.seafile.seadroid2;

import java.net.MalformedURLException;
import java.net.URL;

import android.content.Context;
import android.content.Intent;
import android.content.SharedPreferences;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.FragmentActivity;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.TextView;

import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;


public class AccountDetailActivity extends FragmentActivity {

    private static final String DEBUG_TAG = "AccountDetailActivity";

    private static final String HTTP_PREFIX = "http://";
    private static final String HTTPS_PREFIX = "https://";

    private TextView statusView;
    private Button loginButton;
    private EditText serverText;
    private EditText emailText;
    private EditText passwdText;
    private CheckBox httpsCheckBox;
    private TextView seahubUrlHintText;

    private AccountManager accountManager;
    private Account account = null;
    private boolean isFromEdit = false;
    private boolean serverTextHasFocus;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.account_detail);

        statusView = (TextView) findViewById(R.id.status_view);
        loginButton = (Button) findViewById(R.id.login_button);
        httpsCheckBox = (CheckBox) findViewById(R.id.https_checkbox);
        serverText = (EditText) findViewById(R.id.server_url);
        emailText = (EditText) findViewById(R.id.email_address);
        passwdText = (EditText) findViewById(R.id.password);
        seahubUrlHintText = (TextView) findViewById(R.id.seahub_url_hint);

        setupServerText();
        accountManager = new AccountManager(this);

        Intent intent = getIntent();
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        isFromEdit = intent.getBooleanExtra("isEdited", false);
        if (server != null) {
            if (email == null) email = "";
            account = new Account(server, email);
            if (account.isHttps())
                httpsCheckBox.setChecked(true);
            serverText.setText(account.getServer());
            emailText.setText(account.getEmail());
            emailText.requestFocus();
            seahubUrlHintText.setVisibility(View.GONE);
        } else {
            serverText.setText(HTTP_PREFIX);
            int prefixLen = HTTP_PREFIX.length();
            serverText.setSelection(prefixLen, prefixLen);
        }
    }

    public void onHttpsCheckboxClicked(View view) {
        refreshServerUrlPrefix();
    }

    private void refreshServerUrlPrefix() {
        boolean isHttps = httpsCheckBox.isChecked();
        String url = serverText.getText().toString();
        String prefix = isHttps ? HTTPS_PREFIX : HTTP_PREFIX;

        String urlWithoutScheme = url.replace(HTTPS_PREFIX, "").replace(HTTP_PREFIX, "");

        int oldOffset = serverText.getSelectionStart();

        // Change the text
        serverText.setText(prefix + urlWithoutScheme);

        if (serverTextHasFocus) {
            // Change the cursor position since we changed the text
            if (isHttps) {
                int offset = oldOffset + 1;
                serverText.setSelection(offset, offset);
            } else {
                int offset = Math.max(0, oldOffset - 1);
                serverText.setSelection(offset, offset);
            }
        }
    }

    private void setupServerText() {
        serverText.setOnFocusChangeListener(new View.OnFocusChangeListener () {
            @Override
            public void onFocusChange(View v, boolean hasFocus) {
                Log.d(DEBUG_TAG, "serverText has focus: " + (hasFocus ? "yes" : "no"));
                serverTextHasFocus = hasFocus;
            }
        });

        serverText.addTextChangedListener(new TextWatcher() {
            private String old;
            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
            }

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count,
                                          int after) {
                old = serverText.getText().toString();
            }

            @Override
            public void afterTextChanged(Editable s) {
                // Don't allow the user to edit the "https://" or "http://" part of the serverText
                String url = serverText.getText().toString();
                boolean isHttps = httpsCheckBox.isChecked();
                String prefix = isHttps ? HTTPS_PREFIX : HTTP_PREFIX;
                if (!url.startsWith(prefix)) {
                    int oldOffset = Math.max(prefix.length(), serverText.getSelectionStart());
                    serverText.setText(old);
                    serverText.setSelection(oldOffset, oldOffset);
                }
            }
        });
    }

    private String cleanServerURL(String serverURL, boolean isHttps) throws MalformedURLException {
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
        boolean isHttps = httpsCheckBox.isChecked();

        ConnectivityManager connMgr = (ConnectivityManager)
            getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();

        if (networkInfo != null && networkInfo.isConnected()) {
            if (serverURL.length() == 0) {
                statusView.setText(R.string.err_server_andress_empty);
                return;
            }
            try {
                serverURL = cleanServerURL(serverURL, isHttps);
            } catch (MalformedURLException e) {
                statusView.setText(R.string.invalid_server_address);
                Log.d(DEBUG_TAG, "Invalid URL " + serverURL);
                return;
            }

            loginButton.setEnabled(false);
            Account tmpAccount = new Account(serverURL, email, passwd);
            ConcurrentAsyncTask.execute(new LoginTask(tmpAccount));
        } else {
            statusView.setText(R.string.network_down);
        }
    }

    private void writeToSharedPreferences(Account account) {

        SharedPreferences sharedPref = getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);
        SharedPreferences.Editor editor = sharedPref.edit();
        editor.putString(AccountsActivity.SHARED_PREF_SERVER_KEY, account.server);
        editor.putString(AccountsActivity.SHARED_PREF_EMAIL_KEY, account.email);
        editor.putString(AccountsActivity.SHARED_PREF_TOKEN_KEY, account.token);
        editor.commit();
    }

    private void startFilesActivity(Account account) {
        Intent intent = new Intent(this, BrowserActivity.class);
        intent.putExtra("server", account.server);
        intent.putExtra("email", account.email);
        intent.putExtra("token", account.token);

        writeToSharedPreferences(account);

        startActivity(intent);
        finish(); // so the user will not return to this activity when press 'back'
    }

    private class LoginTask extends AsyncTask<Void, Void, String> {

        Account loginAccount;
        SeafException err = null;

        public LoginTask(Account loginAccount) {
            this.loginAccount = loginAccount;
        }

        @Override
        protected String doInBackground(Void... params) {
            if (params.length != 0)
                return "Error number of parameter";

            return doLogin();
        }

        @Override
        protected void onPostExecute(String result) {
            if (result != null && result.equals("Success")) {
                if (isFromEdit) {
                    accountManager.updateAccount(account, loginAccount);
                    isFromEdit = false;
                } else {
                    accountManager.saveDefaultAccount(loginAccount);
                }
                startFilesActivity(loginAccount);
            } else {
                if (err != null && err == SeafException.sslException) {
                    statusView.setText("SSL Error or Certification Error. You may try again.");
                } else
                    statusView.setText(result);
            }
            loginButton.setEnabled(true);
        }

        private String doLogin() {
            SeafConnection sc = new SeafConnection(loginAccount);

            try {
                if (sc.doLogin() == false)
                    return getString(R.string.err_login_failed);
                return "Success";
            } catch (SeafException e) {
                err = e;
                switch (e.getCode()) {
                case 400:
                    return getString(R.string.err_wrong_user_or_passwd);
                case 404:
                    return getString(R.string.invalid_server_address);
                default:
                    return e.getMessage();
                }
            }
        }
    }

    /*  // no longer used
    private class TrustServerDialogFragment extends DialogFragment {

        Account account;

        TrustServerDialogFragment(Account loginAccount) {
            account = loginAccount;
        }

        @Override
        public Dialog onCreateDialog(Bundle savedInstanceState) {
            AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
            builder.setMessage(R.string.trust_https_server)
                   .setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
                       public void onClick(DialogInterface dialog, int id) {
                           try {
                               TrustManagerFactory.addCertificateChain(TrustManagerFactory.getLastCertChain());
                               ConcurrentAsyncTask.execute(new LoginTask(account));
                           } catch (CertificateException e) {
                               e.printStackTrace();
                           }
                       }
                   })
                   .setNegativeButton(R.string.cancel, new DialogInterface.OnClickListener() {
                       public void onClick(DialogInterface dialog, int id) {
                           // User cancelled the dialog
                       }
                   });
            return builder.create();
        }
    }
    */
}
