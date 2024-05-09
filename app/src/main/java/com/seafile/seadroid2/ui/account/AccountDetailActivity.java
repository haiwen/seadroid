package com.seafile.seadroid2.ui.account;

import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.text.method.HideReturnsTransformationMethod;
import android.text.method.PasswordTransformationMethod;
import android.util.Log;
import android.util.Pair;
import android.view.MenuItem;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.appcompat.widget.Toolbar;
import androidx.core.app.NavUtils;
import androidx.core.app.TaskStackBuilder;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.NetworkUtils;
import com.google.android.material.button.MaterialButton;
import com.google.android.material.dialog.MaterialAlertDialogBuilder;
import com.google.android.material.textfield.TextInputEditText;
import com.google.android.material.textfield.TextInputLayout;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ssl.CertsManager;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog.SslConfirmDialog;

import java.net.HttpURLConnection;
import java.net.MalformedURLException;

public class AccountDetailActivity extends BaseActivityWithVM<AccountViewModel> implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "AccountDetailActivity";

    private final String TWO_FACTOR_AUTH = "two_factor_auth";

    private TextView mStatusTv;
    private Button mLoginBtn;
    private EditText mServerEt;
    private TextInputEditText mEmailEt;
    private EditText mPasswdEt;
    private CheckBox mHttpsCheckBox;
    private TextView mSeaHubUrlHintTv;
    private ImageView mClearEmailIv;
    private TextInputLayout mAuthTokenInputLayout;
    private EditText mAuthTokenEt;

    private boolean serverTextHasFocus;
    private CheckBox mRemDeviceCheckBox;
    private String mSessionKey;

    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.account_detail);


        mStatusTv = findViewById(R.id.status_view);
        mHttpsCheckBox = findViewById(R.id.https_checkbox);
        mServerEt = findViewById(R.id.server_url);
        mEmailEt = findViewById(R.id.email_address);
        mPasswdEt = findViewById(R.id.password);
        mSeaHubUrlHintTv = findViewById(R.id.seahub_url_hint);

        mClearEmailIv = findViewById(R.id.iv_delete_email);

        mAuthTokenInputLayout = findViewById(R.id.auth_token_hint);
        mAuthTokenEt = findViewById(R.id.auth_token);
        mAuthTokenInputLayout.setVisibility(View.GONE);

        mRemDeviceCheckBox = findViewById(R.id.remember_device);
        mRemDeviceCheckBox.setVisibility(View.GONE);

        mLoginBtn = findViewById(R.id.login_button);
        mLoginBtn.setOnClickListener(v -> login());

        setupServerText();

        Intent intent = getIntent();

        String defaultServerUri = intent.getStringExtra(SeafileAuthenticatorActivity.ARG_SERVER_URI);

        if (intent.getBooleanExtra("isEdited", false)) {
            String account_name = intent.getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_NAME);
            String account_type = intent.getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE);
            android.accounts.Account account = new android.accounts.Account(account_name, account_type);

            android.accounts.AccountManager mAccountManager = android.accounts.AccountManager.get(getBaseContext());
            String server = mAccountManager.getUserData(account, Authenticator.KEY_SERVER_URI);
            String email = mAccountManager.getUserData(account, Authenticator.KEY_EMAIL);
            String name = mAccountManager.getUserData(account, Authenticator.KEY_NAME);
            mSessionKey = mAccountManager.getUserData(account, Authenticator.SESSION_KEY);
            // isFromEdit = mAccountManager.getUserData(account, Authenticator.KEY_EMAIL);

            if (server.startsWith(Constants.Protocol.HTTPS))
                mHttpsCheckBox.setChecked(true);

            mServerEt.setText(server);
            mEmailEt.setText(email);
            mEmailEt.requestFocus();
            mSeaHubUrlHintTv.setVisibility(View.GONE);
        } else if (defaultServerUri != null) {
            if (defaultServerUri.startsWith(Constants.Protocol.HTTPS))
                mHttpsCheckBox.setChecked(true);
            mServerEt.setText(defaultServerUri);
            mEmailEt.requestFocus();
        } else {
            mServerEt.setText(Constants.Protocol.HTTP);
            int prefixLen = Constants.Protocol.HTTP.length();
            mServerEt.setSelection(prefixLen, prefixLen);
        }


        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.login);

        initListener();

        initViewModel();
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


        mClearEmailIv.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                mEmailEt.setText("");
            }
        });

        TextInputLayout passwordInputLayout = findViewById(R.id.password_hint);
        passwordInputLayout.setEndIconOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (mPasswdEt.getTransformationMethod() instanceof PasswordTransformationMethod) {
                    mPasswdEt.setTransformationMethod(HideReturnsTransformationMethod.getInstance());
                    passwordInputLayout.setEndIconDrawable(R.drawable.icon_eye_open);
                } else {
                    passwordInputLayout.setEndIconDrawable(R.drawable.icon_eye_close);
                    mPasswdEt.setTransformationMethod(PasswordTransformationMethod.getInstance());
                }

                String input = mPasswdEt.getText().toString().trim();
                if (!TextUtils.isEmpty(input)) {
                    mPasswdEt.setSelection(input.length());
                }
            }
        });
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                if (aBoolean) {
                    showProgressDialog();
                } else {
                    dismissProgressDialog();
                }
            }
        });

        getViewModel().getAccountSeafExceptionLiveData().observe(this, new Observer<Pair<Account, SeafException>>() {
            @Override
            public void onChanged(Pair<Account, SeafException> pair) {
                onLoginException(pair.first, pair.second);
            }
        });

        getViewModel().getAccountLiveData().observe(this, new Observer<Account>() {
            @Override
            public void onChanged(Account account) {
                onLoggedIn(account);
            }
        });
    }

    private void onLoginException(Account account, SeafException err) {
        if (err == SeafException.sslException) {
            mAuthTokenInputLayout.setVisibility(View.GONE);
            mRemDeviceCheckBox.setVisibility(View.GONE);
            SslConfirmDialog sslConfirmDialog = new SslConfirmDialog(account,
                    new SslConfirmDialog.Listener() {
                        @Override
                        public void onAccepted(boolean rememberChoice) {
                            CertsManager.instance().saveCertForAccount(account, rememberChoice);
                            login();
                        }

                        @Override
                        public void onRejected() {
                            mStatusTv.setText(R.string.ssl_error);
                            mLoginBtn.setEnabled(true);
                        }
                    });
            sslConfirmDialog.show(getSupportFragmentManager(), SslConfirmDialog.FRAGMENT_TAG);
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
        } else if (err.getCode() == HttpURLConnection.HTTP_NOT_FOUND) {
            mStatusTv.setText(R.string.invalid_server_address);
        } else if (err.getCode() == HttpURLConnection.HTTP_BAD_REQUEST) {
            mStatusTv.setText(R.string.err_wrong_user_or_passwd);
        } else {
            mAuthTokenInputLayout.setVisibility(View.GONE);
            mRemDeviceCheckBox.setVisibility(View.GONE);
            mStatusTv.setText(err.getMessage());
        }

        mLoginBtn.setEnabled(true);
    }

    private void onLoggedIn(Account loginAccount) {
        Intent retData = new Intent();
        retData.putExtras(getIntent());
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_NAME, loginAccount.getSignature());
        retData.putExtra(android.accounts.AccountManager.KEY_AUTHTOKEN, loginAccount.getToken());
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_TYPE, getIntent().getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE));

        //extra params
        retData.putExtra(SeafileAuthenticatorActivity.ARG_AVATAR_URL, loginAccount.getAvatarUrl());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_EMAIL, loginAccount.getEmail());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_NAME, loginAccount.getName());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_AUTH_SESSION_KEY, loginAccount.getSessionKey());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SERVER_URI, loginAccount.getServer());
        retData.putExtra(TWO_FACTOR_AUTH, mRemDeviceCheckBox.isChecked());
        setResult(RESULT_OK, retData);
        finish();
    }

    @Override
    protected void onDestroy() {
        dismissProgressDialog();
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
        String prefix = isHttps ? Constants.Protocol.HTTPS : Constants.Protocol.HTTP;

        String urlWithoutScheme = url.replace(Constants.Protocol.HTTPS, "").replace(Constants.Protocol.HTTP, "");

        int oldOffset = mServerEt.getSelectionStart();

        // Change the text
        mServerEt.setText(String.format("%s%s", prefix, urlWithoutScheme));

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
                String prefix = isHttps ? Constants.Protocol.HTTPS : Constants.Protocol.HTTP;
                if (!url.startsWith(prefix)) {
                    int oldOffset = Math.max(prefix.length(), mServerEt.getSelectionStart());
                    mServerEt.setText(old);
                    mServerEt.setSelection(oldOffset, oldOffset);
                }
            }
        });
    }


    private void login() {
        String serverURL = mServerEt.getText().toString().trim();
        String email = mEmailEt.getText().toString().trim();
        String passwd = mPasswdEt.getText().toString();

        if (!NetworkUtils.isConnected()) {
            mStatusTv.setText(R.string.network_down);
            return;
        }

        if (serverURL.isEmpty()) {
            mStatusTv.setText(R.string.err_server_andress_empty);
            return;
        }

        if (email.isEmpty()) {
            mEmailEt.setError(getResources().getString(R.string.err_email_empty));
            return;
        }

        if (passwd.isEmpty()) {
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

        Account tempAccount = new Account(null, serverURL, email, null, null, false, mSessionKey, String.valueOf(System.currentTimeMillis()));
        getViewModel().login(tempAccount, passwd, authToken, rememberDevice);

    }

    private Dialog dialog;

    private void showProgressDialog() {

        if (dialog == null) {
            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
            builder.setView(R.layout.layout_dialog_progress_bar);
            dialog = builder.create();
        }

        if (dialog.isShowing()) {
            dialog.dismiss();
        }
        dialog.show();
    }

    private void dismissProgressDialog() {
        if (dialog != null) {
            dialog.dismiss();
        }
    }
}
