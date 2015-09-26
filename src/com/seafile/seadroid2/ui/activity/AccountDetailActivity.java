package com.seafile.seadroid2.ui.activity;

import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.ArrayList;

import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.NavUtils;
import android.support.v4.app.TaskStackBuilder;
import android.text.Editable;
import android.text.TextWatcher;
import android.util.Log;
import android.view.View;
import android.widget.*;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;
import com.seafile.seadroid2.CertsManager;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.ui.CustomClearableEditText;
import com.seafile.seadroid2.ui.dialog.SslConfirmDialog;
import com.seafile.seadroid2.util.Utils;

public class AccountDetailActivity extends SherlockFragmentActivity {
    private static final String DEBUG_TAG = "AccountDetailActivity";

    private static final String HTTP_PREFIX = "http://";
    private static final String HTTPS_PREFIX = "https://";

    private TextView statusView;
    private Button loginButton;
    private EditText serverText;
    private CustomClearableEditText emailText;
    private CustomClearableEditText passwdText;
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
        //This has to be called before setContentView and you must use the
        //class in com.actionbarsherlock.view and NOT android.view
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        setContentView(R.layout.account_detail);

        statusView = (TextView) findViewById(R.id.status_view);
        loginButton = (Button) findViewById(R.id.login_button);
        httpsCheckBox = (CheckBox) findViewById(R.id.https_checkbox);
        serverText = (EditText) findViewById(R.id.server_url);
        emailText = (CustomClearableEditText) findViewById(R.id.email_address);
        emailText.setInputType(CustomClearableEditText.INPUT_TYPE_EMAIL);
        passwdText = (CustomClearableEditText) findViewById(R.id.password);
        passwdText.setInputType(CustomClearableEditText.INPUT_TYPE_PASSWORD);
        seahubUrlHintText = (TextView) findViewById(R.id.seahub_url_hint);

        setupServerText();
        accountManager = new AccountManager(this);

        // email address auto complete when login in
        ArrayList<String> accounts = accountManager.getAccountAutoCompleteTexts();
        if (accounts != null) {
            ArrayAdapter<String> adapter = new ArrayAdapter<String>(this, android.R.layout.simple_dropdown_item_1line, accounts);
            emailText.setEmailAddressAutoCompleteAdapter(adapter);
        }

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
        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayHomeAsUpEnabled(true);
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        savedInstanceState.putString("email", emailText.getText().toString());
        savedInstanceState.putString("password", passwdText.getText().toString());

        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);

        emailText.setText((String) savedInstanceState.get("email"));
        passwdText.setText((String) savedInstanceState.get("password"));
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
         switch (item.getItemId()) {
            case android.R.id.home:

                /* FYI {@link http://stackoverflow.com/questions/13293772/how-to-navigate-up-to-the-same-parent-state?rq=1} */
                Intent upIntent = new Intent(this, AccountsActivity.class);
                if (NavUtils.shouldUpRecreateTask(this, upIntent)) {
                    // This activity is NOT part of this app's task, so create a new task
                    // when navigating up, with a synthesized back stack.
                    TaskStackBuilder.create(this)
                            // Add all of this activity's parents to the back stack
                            .addNextIntentWithParentStack(upIntent)
                            // Navigate up to the closest parent
                            .startActivities();
                } else {
                    // This activity is part of this app's task, so simply
                    // navigate up to the logical parent activity.
                    // NavUtils.navigateUpTo(this, upIntent);
                    upIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                    startActivity(upIntent);
                    finish();
                }

                return true;
        }
         return super.onOptionsItemSelected(item);
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

    /** Called when the user clicks the Login button */
    public void login(View view) {
        String serverURL = serverText.getText().toString().trim();
        String email = emailText.getText().toString().trim();
        String passwd = passwdText.getText().toString();

        ConnectivityManager connMgr = (ConnectivityManager)
            getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();

        if (networkInfo != null && networkInfo.isConnected()) {
            if (serverURL.length() == 0) {
                statusView.setText(R.string.err_server_andress_empty);
                return;
            }
            
            if (email.length() == 0) {
                emailText.setError(getResources().getString(R.string.err_email_empty));
                return;
            }
            
            if (passwd.length() == 0) {
                passwdText.setError(getResources().getString(R.string.err_passwd_empty));
                return;
            }
            
            try {
                serverURL = Utils.cleanServerURL(serverURL);
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

    private class LoginTask extends AsyncTask<Void, Void, String> {
        Account loginAccount;
        SeafException err = null;

        public LoginTask(Account loginAccount) {
            this.loginAccount = loginAccount;
        }

        @Override
        protected void onPreExecute() {
            //super.onPreExecute();
            setSupportProgressBarIndeterminateVisibility(true);
        }

        @Override
        protected String doInBackground(Void... params) {
            if (params.length != 0)
                return "Error number of parameter";

            return doLogin();
        }

        private void resend() {
            ConcurrentAsyncTask.execute(new LoginTask(loginAccount));
        }

        @Override
        protected void onPostExecute(final String result) {
            if (err == SeafException.sslException) {
                SslConfirmDialog dialog = new SslConfirmDialog(loginAccount,
                new SslConfirmDialog.Listener() {
                    @Override
                    public void onAccepted(boolean rememberChoice) {
                        CertsManager.instance().saveCertForAccount(loginAccount, rememberChoice);
                        resend();
                    }

                    @Override
                    public void onRejected() {
                        statusView.setText(result);
                        loginButton.setEnabled(true);
                    }
                });
                dialog.show(getSupportFragmentManager(), SslConfirmDialog.FRAGMENT_TAG);
                return;
            }

            if (result != null && result.equals("Success")) {
                if (isFromEdit) {
                    accountManager.updateAccountFromDB(account, loginAccount);
                    isFromEdit = false;
                } else {
                    accountManager.saveAccountToDB(loginAccount);
                }

                // save account to SharedPreference
                accountManager.saveCurrentAccount(loginAccount);

                setResult(RESULT_OK);
                finish();

            } else {
                statusView.setText(result);
            }
            setSupportProgressBarIndeterminateVisibility(false);
            loginButton.setEnabled(true);
        }

        private String doLogin() {
            SeafConnection sc = new SeafConnection(loginAccount);

            try {
                if (!sc.doLogin())
                    return getString(R.string.err_login_failed);
                return "Success";
            } catch (SeafException e) {
                err = e;
                if (e == SeafException.sslException) {
                    return getString(R.string.ssl_error);
                }
                switch (e.getCode()) {
                case HttpURLConnection.HTTP_BAD_REQUEST:
                    return getString(R.string.err_wrong_user_or_passwd);
                case HttpURLConnection.HTTP_NOT_FOUND:
                    return getString(R.string.invalid_server_address);
                default:
                    return e.getMessage();
                }
            }
        }
    }
}
