package com.seafile.seadroid2.ui.account;

import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.text.TextWatcher;
import android.util.Log;
import android.util.Pair;
import android.view.MenuItem;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.CompoundButton;

import androidx.appcompat.widget.Toolbar;
import androidx.core.app.NavUtils;
import androidx.core.app.TaskStackBuilder;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.AccountDetailBinding;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ssl.CertsManager;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;
import com.seafile.seadroid2.ui.dialog.SslConfirmDialog;

import java.net.HttpURLConnection;
import java.net.MalformedURLException;

public class AccountDetailActivity extends BaseActivityWithVM<AccountViewModel> implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "AccountDetailActivity";

    private final String TWO_FACTOR_AUTH = "two_factor_auth";

    private AccountDetailBinding binding;
    private boolean serverTextHasFocus;
    private String mSessionKey;

    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        binding = AccountDetailBinding.inflate(getLayoutInflater());

        setContentView(binding.getRoot());

        initView();

        initViewModel();
    }


    private void initView() {

        binding.authTokenHint.setVisibility(View.GONE);
        binding.rememberDevice.setVisibility(View.GONE);
        binding.loginButton.setOnClickListener(v -> login());

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
                binding.httpsCheckbox.setChecked(true);

            binding.serverUrl.setText(server);
            binding.emailAddress.setText(email);
            binding.emailAddress.requestFocus();

            binding.seahubUrlHint.setVisibility(View.GONE);
        } else if (defaultServerUri != null) {
            if (defaultServerUri.startsWith(Constants.Protocol.HTTPS))
                binding.httpsCheckbox.setChecked(true);
            binding.serverUrl.setText(defaultServerUri);
            binding.emailAddress.requestFocus();

        } else {
            binding.serverUrl.setText(Constants.Protocol.HTTP);
            int prefixLen = Constants.Protocol.HTTP.length();
            binding.serverUrl.setSelection(prefixLen, prefixLen);
        }


        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.login);

        binding.httpsCheckbox.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                refreshServerUrlPrefix();
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

    private void refreshServerUrlPrefix() {
        boolean isHttps = binding.httpsCheckbox.isChecked();
        String url = binding.serverUrl.getText().toString();
        String prefix = isHttps ? Constants.Protocol.HTTPS : Constants.Protocol.HTTP;

        String urlWithoutScheme = url.replace(Constants.Protocol.HTTPS, "").replace(Constants.Protocol.HTTP, "");

        int oldOffset = binding.serverUrl.getSelectionStart();

        // Change the text
        binding.serverUrl.setText(String.format("%s%s", prefix, urlWithoutScheme));

        if (serverTextHasFocus) {
            // Change the cursor position since we changed the text
            int offset;
            if (isHttps) {
                offset = oldOffset + 1;
            } else {
                offset = Math.max(0, oldOffset - 1);
            }

            if (offset > binding.serverUrl.getText().length()) {
                return;
            }
            binding.serverUrl.setSelection(offset, offset);
        }
    }

    private void onLoginException(Account account, SeafException err) {
        if (err == SeafException.SSL_EXCEPTION) {
            binding.authTokenHint.setVisibility(View.GONE);
            binding.rememberDevice.setVisibility(View.GONE);

//            MaterialAlertDialogBuilder builder = new MaterialAlertDialogBuilder(this);
//            builder.setTitle(R.string.ssl_confirm_title);
//            builder.setMessage(getString(R.string.ssl_not_trusted,account.getServerHost()));
//            builder.setPositiveButton(R.string.ok, new DialogInterface.OnClickListener() {
//                @Override
//                public void onClick(DialogInterface dialog, int which) {
//                    dialog.dismiss();
//                }
//            });
//            builder.create().show();

            SslConfirmDialog sslConfirmDialog = new SslConfirmDialog(account,
                    new SslConfirmDialog.Listener() {
                        @Override
                        public void onAccepted(boolean rememberChoice) {
                            CertsManager.instance().saveCertForAccount(account, rememberChoice);
                            login();
                        }

                        @Override
                        public void onRejected() {
                            binding.statusView.setText(R.string.ssl_error);
                            binding.loginButton.setEnabled(true);
                        }
                    });
            sslConfirmDialog.show(getSupportFragmentManager(), SslConfirmDialog.FRAGMENT_TAG);
        } else if (err == SeafException.TWO_FACTOR_AUTH_TOKEN_MISSING_EXCEPTION) {
            // show auth token input box
            binding.authTokenHint.setVisibility(View.VISIBLE);
            binding.rememberDevice.setVisibility(View.VISIBLE);
            binding.rememberDevice.setChecked(false);
            binding.authToken.setError(getString(R.string.two_factor_auth_error));
        } else if (err == SeafException.TWO_FACTOR_AUTH_TOKEN_INVALID_EXCEPTION) {
            // show auth token input box
            binding.authTokenHint.setVisibility(View.VISIBLE);
            binding.rememberDevice.setVisibility(View.VISIBLE);
            binding.rememberDevice.setChecked(false);
            binding.authToken.setError(getString(R.string.two_factor_auth_invalid));
        } else if (err.getCode() == HttpURLConnection.HTTP_NOT_FOUND) {
            binding.statusView.setText(R.string.invalid_server_address);
        } else if (err.getCode() == HttpURLConnection.HTTP_BAD_REQUEST) {
            binding.statusView.setText(R.string.err_wrong_user_or_passwd);
        } else {
            binding.authTokenHint.setVisibility(View.GONE);
            binding.rememberDevice.setVisibility(View.GONE);
            binding.statusView.setText(err.getMessage());
        }

        binding.loginButton.setEnabled(true);
    }

    private void onLoggedIn(Account loginAccount) {
        Intent retData = new Intent();
        retData.putExtras(getIntent());
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_NAME, loginAccount.getSignature());
        retData.putExtra(android.accounts.AccountManager.KEY_AUTHTOKEN, loginAccount.getToken());
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_TYPE, getIntent().getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE));

        //extra params
        retData.putExtra(SeafileAuthenticatorActivity.ARG_AVATAR_URL, loginAccount.getAvatarUrl());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SPACE_TOTAL, loginAccount.getTotalSpace());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SPACE_USAGE, loginAccount.getUsageSpace());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_EMAIL, loginAccount.getEmail());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_NAME, loginAccount.getName());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_AUTH_SESSION_KEY, loginAccount.getSessionKey());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SERVER_URI, loginAccount.getServer());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SHIB, true);
        retData.putExtra(TWO_FACTOR_AUTH, binding.rememberDevice.isChecked());
        setResult(RESULT_OK, retData);
        finish();
    }

    @Override
    protected void onDestroy() {
        dismissLoadingDialog();
        super.onDestroy();
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        savedInstanceState.putString("email", binding.emailAddress.getText().toString());
        savedInstanceState.putString("password", binding.password.getText().toString());
        savedInstanceState.putBoolean("rememberDevice", binding.rememberDevice.isChecked());
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);

        binding.emailAddress.setText((String) savedInstanceState.get("email"));
        binding.password.setText((String) savedInstanceState.get("password"));
        binding.rememberDevice.setChecked((boolean) savedInstanceState.get("rememberDevice"));
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

    private void setupServerText() {
        binding.serverUrl.setOnFocusChangeListener(new View.OnFocusChangeListener() {
            @Override
            public void onFocusChange(View v, boolean hasFocus) {
                Log.d(DEBUG_TAG, "serverText has focus: " + (hasFocus ? "yes" : "no"));
                serverTextHasFocus = hasFocus;
            }
        });

        binding.serverUrl.addTextChangedListener(new TextWatcher() {
            private String old;

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
            }

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                old = binding.serverUrl.getText().toString();
            }

            @Override
            public void afterTextChanged(Editable s) {
                // Don't allow the user to edit the "https://" or "http://" part of the serverText
                String url = binding.serverUrl.getText().toString();
                boolean isHttps = binding.httpsCheckbox.isChecked();
                String prefix = isHttps ? Constants.Protocol.HTTPS : Constants.Protocol.HTTP;
                if (!url.startsWith(prefix)) {
                    int oldOffset = Math.max(prefix.length(), binding.serverUrl.getSelectionStart());
                    binding.serverUrl.setText(old);
                    binding.serverUrl.setSelection(oldOffset, oldOffset);
                }
            }
        });
    }


    private void login() {
        String serverURL = binding.serverUrl.getText().toString().trim();
        String email = binding.emailAddress.getText().toString().trim();
        String passwd = binding.password.getText().toString();

        if (!NetworkUtils.isConnected()) {
            binding.statusView.setText(R.string.network_down);
            return;
        }

        String urlWithoutScheme = serverURL.replace(Constants.Protocol.HTTPS, "").replace(Constants.Protocol.HTTP, "");
        if (TextUtils.isEmpty(urlWithoutScheme)) {
            binding.serverHint.setErrorEnabled(true);
            binding.serverHint.setError(getResources().getString(R.string.err_server_andress_empty));
            return;
        } else {
            binding.serverHint.setError(null);
            binding.serverHint.setErrorEnabled(false);
        }

        if (email.isEmpty()) {
            binding.emailHint.setErrorEnabled(true);
            binding.emailHint.setError(getResources().getString(R.string.err_email_empty));
            return;
        } else {
            binding.serverHint.setError(null);
            binding.serverHint.setErrorEnabled(false);
        }

        if (passwd.isEmpty()) {
            binding.passwordHint.setErrorEnabled(true);
            binding.passwordHint.setError(getResources().getString(R.string.err_passwd_empty));
            return;
        } else {
            binding.passwordHint.setError(null);
            binding.passwordHint.setErrorEnabled(false);
        }

        String authToken = null;
        if (binding.authTokenHint.getVisibility() == View.VISIBLE) {
            authToken = binding.authToken.getText().toString().trim();
            if (TextUtils.isEmpty(authToken)) {
                binding.authToken.setError(getResources().getString(R.string.two_factor_auth_token_empty));
                return;
            }
        }

        boolean rememberDevice = false;
        if (binding.rememberDevice.getVisibility() == View.VISIBLE) {
            rememberDevice = binding.rememberDevice.isChecked();
        }

        try {
            serverURL = Utils.cleanServerURL(serverURL);
        } catch (MalformedURLException e) {
            binding.statusView.setText(R.string.invalid_server_address);
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
}
