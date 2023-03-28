package com.seafile.seadroid2.account.ui;

import android.app.ProgressDialog;
import android.content.Context;
import android.content.Intent;
import android.net.ConnectivityManager;
import android.net.NetworkInfo;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.design.widget.TextInputLayout;
import android.support.v4.app.NavUtils;
import android.support.v4.app.TaskStackBuilder;
import android.support.v7.widget.Toolbar;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.text.method.HideReturnsTransformationMethod;
import android.text.method.PasswordTransformationMethod;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountInfo;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.ssl.CertsManager;
import com.seafile.seadroid2.ui.EmailAutoCompleteTextView;
import com.seafile.seadroid2.ui.activity.AccountsActivity;
import com.seafile.seadroid2.ui.activity.BaseActivity;
import com.seafile.seadroid2.ui.dialog.SslConfirmDialog;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONException;

import java.net.HttpURLConnection;
import java.net.MalformedURLException;

public class AccountDetailActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "AccountDetailActivity";

    private static final String HTTP_PREFIX = "http://";
    private static final String HTTPS_PREFIX = "https://";
    public static final String TWO_FACTOR_AUTH = "two_factor_auth";

    private TextView mStatusTv;
    private Button mLoginBtn;
    private EditText mServerEt;
    private ProgressDialog mProgressDialog;
    private EmailAutoCompleteTextView mEmailEt;
    private EditText mPasswdEt;
    private CheckBox mHttpsCheckBox;
    private TextView mSeaHubUrlHintTv;
    private ImageView mClearEmailIv, mClearPasswordIv, mEyeClickIv;
    private RelativeLayout mEyeContainer;
    private TextInputLayout mAuthTokenInputLayout;
    private EditText mAuthTokenEt;

    private android.accounts.AccountManager mAccountManager;
    private boolean serverTextHasFocus;
    private boolean isPasswordVisible;
    private CheckBox mRemDeviceCheckBox;
    private String mSessionKey;

    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.account_detail);

        mAccountManager = android.accounts.AccountManager.get(getBaseContext());

        mStatusTv = (TextView) findViewById(R.id.status_view);
        mLoginBtn = (Button) findViewById(R.id.login_button);
        mHttpsCheckBox = (CheckBox) findViewById(R.id.https_checkbox);
        mServerEt = (EditText) findViewById(R.id.server_url);
        mEmailEt = (EmailAutoCompleteTextView) findViewById(R.id.email_address);
        mPasswdEt = (EditText) findViewById(R.id.password);
        mSeaHubUrlHintTv = (TextView) findViewById(R.id.seahub_url_hint);

        mClearEmailIv = (ImageView) findViewById(R.id.iv_delete_email);
        mClearPasswordIv = (ImageView) findViewById(R.id.iv_delete_pwd);
        mEyeContainer = (RelativeLayout) findViewById(R.id.rl_layout_eye);
        mEyeClickIv = (ImageView) findViewById(R.id.iv_eye_click);

        mAuthTokenInputLayout = (TextInputLayout) findViewById(R.id.auth_token_hint);
        mAuthTokenEt = (EditText) findViewById(R.id.auth_token);
        mAuthTokenInputLayout.setVisibility(View.GONE);

        mRemDeviceCheckBox = findViewById(R.id.remember_device);
        mRemDeviceCheckBox.setVisibility(View.GONE);
        setupServerText();

        Intent intent = getIntent();

        String defaultServerUri = intent.getStringExtra(SeafileAuthenticatorActivity.ARG_SERVER_URI);

        if (intent.getBooleanExtra("isEdited", false)) {
            String account_name = intent.getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_NAME);
            String account_type = intent.getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE);
            android.accounts.Account account = new android.accounts.Account(account_name, account_type);

            String server = mAccountManager.getUserData(account, Authenticator.KEY_SERVER_URI);
            String email = mAccountManager.getUserData(account, Authenticator.KEY_EMAIL);
            String name = mAccountManager.getUserData(account, Authenticator.KEY_NAME);
            mSessionKey = mAccountManager.getUserData(account, Authenticator.SESSION_KEY);
            // isFromEdit = mAccountManager.getUserData(account, Authenticator.KEY_EMAIL);

            if (server.startsWith(HTTPS_PREFIX))
                mHttpsCheckBox.setChecked(true);

            mServerEt.setText(server);
            mEmailEt.setText(email);
            mEmailEt.requestFocus();
            mSeaHubUrlHintTv.setVisibility(View.GONE);


        } else if (defaultServerUri != null) {
            if (defaultServerUri.startsWith(HTTPS_PREFIX))
                mHttpsCheckBox.setChecked(true);
            mServerEt.setText(defaultServerUri);
            mEmailEt.requestFocus();
        } else {
            mServerEt.setText(HTTP_PREFIX);
            int prefixLen = HTTP_PREFIX.length();
            mServerEt.setSelection(prefixLen, prefixLen);
        }
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.login);

        initListener();
    }

    private void initListener() {
        mEmailEt.setOnFocusChangeListener(new View.OnFocusChangeListener() {
            @Override
            public void onFocusChange(View v, boolean hasFocus) {
                if (hasFocus && mEmailEt.getText().toString().trim().length() > 0) {
                    mClearEmailIv.setVisibility(View.VISIBLE);
                } else {
                    mClearEmailIv.setVisibility(View.INVISIBLE);
                }
            }
        });

        mPasswdEt.setOnFocusChangeListener(new View.OnFocusChangeListener() {
            @Override
            public void onFocusChange(View v, boolean hasFocus) {
                if (hasFocus && mPasswdEt.getText().toString().trim().length() > 0) {
                    mClearPasswordIv.setVisibility(View.VISIBLE);
                } else {
                    mClearPasswordIv.setVisibility(View.INVISIBLE);
                }
            }
        });

        mEmailEt.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                if (mEmailEt.getText().toString().trim().length() > 0) {
                    mClearEmailIv.setVisibility(View.VISIBLE);
                } else {
                    mClearEmailIv.setVisibility(View.INVISIBLE);
                }
            }

            @Override
            public void afterTextChanged(Editable s) {
            }
        });


        mPasswdEt.addTextChangedListener(new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                if (mPasswdEt.getText().toString().trim().length() > 0) {
                    mClearPasswordIv.setVisibility(View.VISIBLE);
                } else {
                    mClearPasswordIv.setVisibility(View.INVISIBLE);
                }
            }

            @Override
            public void afterTextChanged(Editable s) {
            }
        });

        mClearEmailIv.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mEmailEt.setText("");
            }
        });

        mClearPasswordIv.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mPasswdEt.setText("");
            }
        });

        mEyeContainer.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (!isPasswordVisible) {
                    mEyeClickIv.setImageResource(R.drawable.icon_eye_open);
                    mPasswdEt.setTransformationMethod(HideReturnsTransformationMethod.getInstance());
                } else {
                    mEyeClickIv.setImageResource(R.drawable.icon_eye_close);
                    mPasswdEt.setTransformationMethod(PasswordTransformationMethod.getInstance());
                }
                isPasswordVisible = !isPasswordVisible;
                mPasswdEt.postInvalidate();
                String input = mPasswdEt.getText().toString().trim();
                if (!TextUtils.isEmpty(input)) {
                    mPasswdEt.setSelection(input.length());
                }
            }
        });
    }

    @Override
    protected void onDestroy() {
        if (mProgressDialog != null)
            mProgressDialog.dismiss();
        super.onDestroy();
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        savedInstanceState.putString("email", mEmailEt.getText().toString());
        savedInstanceState.putString("password", mPasswdEt.getText().toString());
        savedInstanceState.putBoolean("rememberDevice", mRemDeviceCheckBox.isChecked());
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);

        mEmailEt.setText((String) savedInstanceState.get("email"));
        mPasswdEt.setText((String) savedInstanceState.get("password"));
        mRemDeviceCheckBox.setChecked((boolean) savedInstanceState.get("rememberDevice"));
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return false;
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
        boolean isHttps = mHttpsCheckBox.isChecked();
        String url = mServerEt.getText().toString();
        String prefix = isHttps ? HTTPS_PREFIX : HTTP_PREFIX;

        String urlWithoutScheme = url.replace(HTTPS_PREFIX, "").replace(HTTP_PREFIX, "");

        int oldOffset = mServerEt.getSelectionStart();

        // Change the text
        mServerEt.setText(prefix + urlWithoutScheme);

        if (serverTextHasFocus) {
            // Change the cursor position since we changed the text
            if (isHttps) {
                int offset = oldOffset + 1;
                mServerEt.setSelection(offset, offset);
            } else {
                int offset = Math.max(0, oldOffset - 1);
                mServerEt.setSelection(offset, offset);
            }
        }
    }

    private void setupServerText() {
        mServerEt.setOnFocusChangeListener(new View.OnFocusChangeListener() {
            @Override
            public void onFocusChange(View v, boolean hasFocus) {
                Log.d(DEBUG_TAG, "serverText has focus: " + (hasFocus ? "yes" : "no"));
                serverTextHasFocus = hasFocus;
            }
        });

        mServerEt.addTextChangedListener(new TextWatcher() {
            private String old;

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
            }

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                old = mServerEt.getText().toString();
            }

            @Override
            public void afterTextChanged(Editable s) {
                // Don't allow the user to edit the "https://" or "http://" part of the serverText
                String url = mServerEt.getText().toString();
                boolean isHttps = mHttpsCheckBox.isChecked();
                String prefix = isHttps ? HTTPS_PREFIX : HTTP_PREFIX;
                if (!url.startsWith(prefix)) {
                    int oldOffset = Math.max(prefix.length(), mServerEt.getSelectionStart());
                    mServerEt.setText(old);
                    mServerEt.setSelection(oldOffset, oldOffset);
                }
            }
        });
    }

    /**
     * Called when the user clicks the Login button
     */
    public void login(View view) {
        String serverURL = mServerEt.getText().toString().trim();
        String email = mEmailEt.getText().toString().trim();
        String passwd = mPasswdEt.getText().toString();
        ConnectivityManager connMgr = (ConnectivityManager) getSystemService(Context.CONNECTIVITY_SERVICE);
        NetworkInfo networkInfo = connMgr.getActiveNetworkInfo();

        if (networkInfo == null || !networkInfo.isConnected()) {
//        if (!NetworkUtils.isConnected()) {
            mStatusTv.setText(R.string.network_down);
            return;
        }

        if (serverURL.length() == 0) {
            mStatusTv.setText(R.string.err_server_andress_empty);
            return;
        }

        if (email.length() == 0) {
            mEmailEt.setError(getResources().getString(R.string.err_email_empty));
            return;
        }

        if (passwd.length() == 0) {
            mPasswdEt.setError(getResources().getString(R.string.err_passwd_empty));
            return;
        }

        String authToken = null;
        if (mAuthTokenInputLayout.getVisibility() == View.VISIBLE) {
            authToken = mAuthTokenEt.getText().toString().trim();
            if (TextUtils.isEmpty(authToken)) {
                mAuthTokenEt.setError(getResources().getString(R.string.two_factor_auth_token_empty));
                return;
            }
        }

        boolean rememberDevice = false;
        if (mRemDeviceCheckBox.getVisibility() == View.VISIBLE) {
            rememberDevice = mRemDeviceCheckBox.isChecked();
        }
        try {
            serverURL = Utils.cleanServerURL(serverURL);
        } catch (MalformedURLException e) {
            mStatusTv.setText(R.string.invalid_server_address);
            Log.d(DEBUG_TAG, "Invalid URL " + serverURL);
            return;
        }

        // force the keyboard to be hidden in all situations
        if (getCurrentFocus() != null) {
            InputMethodManager imm = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
            imm.hideSoftInputFromWindow(getCurrentFocus().getWindowToken(), 0);
        }

        mLoginBtn.setEnabled(false);
        Account tmpAccount = new Account(null, serverURL, email, null, false, mSessionKey);
        mProgressDialog = new ProgressDialog(this);
        mProgressDialog.setMessage(getString(R.string.settings_cuc_loading));
        mProgressDialog.setCancelable(false);
        ConcurrentAsyncTask.execute(new LoginTask(tmpAccount, passwd, authToken, rememberDevice));

    }

    private class LoginTask extends AsyncTask<Void, Void, String> {
        Account loginAccount;
        SeafException err = null;
        String passwd;
        String authToken;
        boolean rememberDevice;

        public LoginTask(Account loginAccount, String passwd, String authToken, boolean rememberDevice) {
            this.loginAccount = loginAccount;
            this.passwd = passwd;
            this.authToken = authToken;
            this.rememberDevice = rememberDevice;
        }

        @Override
        protected void onPreExecute() {
            //super.onPreExecute();
            mProgressDialog.show();
        }

        @Override
        protected String doInBackground(Void... params) {
            if (params.length != 0)
                return "Error number of parameter";

            return doLogin();
        }

        private void resend() {
            ConcurrentAsyncTask.execute(new LoginTask(loginAccount, passwd, authToken, rememberDevice));
        }

        @Override
        protected void onPostExecute(final String result) {
            mProgressDialog.dismiss();
            if (err == SeafException.sslException) {
                mAuthTokenInputLayout.setVisibility(View.GONE);
                mRemDeviceCheckBox.setVisibility(View.GONE);
                SslConfirmDialog dialog = new SslConfirmDialog(loginAccount,
                        new SslConfirmDialog.Listener() {
                            @Override
                            public void onAccepted(boolean rememberChoice) {
                                CertsManager.instance().saveCertForAccount(loginAccount, rememberChoice);
                                resend();
                            }

                            @Override
                            public void onRejected() {
                                mStatusTv.setText(result);
                                mLoginBtn.setEnabled(true);
                            }
                        });
                dialog.show(getSupportFragmentManager(), SslConfirmDialog.FRAGMENT_TAG);
                return;
            } else if (err == SeafException.twoFactorAuthTokenMissing) {
                // show auth token input box
                mAuthTokenInputLayout.setVisibility(View.VISIBLE);
                mRemDeviceCheckBox.setVisibility(View.VISIBLE);
                mRemDeviceCheckBox.setChecked(false);
                mAuthTokenEt.setError(getString(R.string.two_factor_auth_error));
            } else if (err == SeafException.twoFactorAuthTokenInvalid) {
                // show auth token input box
                mAuthTokenInputLayout.setVisibility(View.VISIBLE);
                mRemDeviceCheckBox.setVisibility(View.VISIBLE);
                mRemDeviceCheckBox.setChecked(false);
                mAuthTokenEt.setError(getString(R.string.two_factor_auth_invalid));
            } else {
                mAuthTokenInputLayout.setVisibility(View.GONE);
                mRemDeviceCheckBox.setVisibility(View.GONE);
            }

            if (result != null && result.equals("Success")) {
                Intent retData = new Intent();
                retData.putExtras(getIntent());
                retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_NAME, loginAccount.getSignature());
                retData.putExtra(android.accounts.AccountManager.KEY_AUTHTOKEN, loginAccount.getToken());
                retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_TYPE, getIntent().getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE));
                retData.putExtra(SeafileAuthenticatorActivity.ARG_EMAIL, loginAccount.getEmail());
                retData.putExtra(SeafileAuthenticatorActivity.ARG_NAME, loginAccount.getName());
                retData.putExtra(SeafileAuthenticatorActivity.ARG_AUTH_SESSION_KEY, loginAccount.getSessionKey());
                retData.putExtra(SeafileAuthenticatorActivity.ARG_SERVER_URI, loginAccount.getServer());
                retData.putExtra(TWO_FACTOR_AUTH, mRemDeviceCheckBox.isChecked());
                setResult(RESULT_OK, retData);
                finish();
            } else {
                mStatusTv.setText(result);
            }
            mLoginBtn.setEnabled(true);
        }

        private String doLogin() {
            SeafConnection sc = new SeafConnection(loginAccount);

            try {
                // if successful, this will place the auth token into "loginAccount"
                if (!sc.doLogin(passwd, authToken, rememberDevice))
                    return getString(R.string.err_login_failed);

                // fetch email address from the server
                DataManager manager = new DataManager(loginAccount);
                AccountInfo accountInfo = manager.getAccountInfo();

                if (accountInfo == null)
                    return "Unknown error";

                // replace email address/username given by the user with the address known by the server.
//                loginAccount = new Account(loginAccount.server, accountInfo.getEmail(), loginAccount.token, false, loginAccount.sessionKey);
                loginAccount = new Account(accountInfo.getName(), loginAccount.server, accountInfo.getEmail(), loginAccount.token, false, loginAccount.sessionKey);

                return "Success";

            } catch (SeafException e) {
                err = e;
                if (e == SeafException.sslException) {
                    return getString(R.string.ssl_error);
                } else if (e == SeafException.twoFactorAuthTokenMissing) {
                    return getString(R.string.two_factor_auth_error);
                } else if (e == SeafException.twoFactorAuthTokenInvalid) {
                    return getString(R.string.two_factor_auth_invalid);
                }
                switch (e.getCode()) {
                    case HttpURLConnection.HTTP_BAD_REQUEST:
                        return getString(R.string.err_wrong_user_or_passwd);
                    case HttpURLConnection.HTTP_NOT_FOUND:
                        return getString(R.string.invalid_server_address);
                    default:
                        return e.getMessage();
                }
            } catch (JSONException e) {
                return e.getMessage();
            }
        }
    }
}
