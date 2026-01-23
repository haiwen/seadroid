package com.seafile.seadroid2.ui.account.sso;

import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.widget.Toolbar;
import androidx.core.app.NavUtils;
import androidx.core.app.TaskStackBuilder;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.databinding.SingleSignOnWelcomeLayoutBinding;
import com.seafile.seadroid2.framework.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.account.AccountsActivity;
import com.seafile.seadroid2.ui.account.SeafileAuthenticatorActivity;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;

import java.util.Locale;
import java.util.Timer;
import java.util.TimerTask;

/**
 * Single Sign-On welcome page
 * <p/>
 */
public class SingleSignOnActivity extends BaseActivityWithVM<SingleSignOnViewModel> implements Toolbar.OnMenuItemClickListener {
    public static final String DEBUG_TAG = "SingleSignOnActivity";

    public static final String SINGLE_SIGN_ON_HTTPS_PREFIX = "https://";

    private SingleSignOnWelcomeLayoutBinding binding;

    private ActivityResultLauncher<Intent> authLauncher;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = SingleSignOnWelcomeLayoutBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());

        registerAuthLauncher();

        initView();
        initViewModel();

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (timer != null) {
                    stopAction();
                    if (isDialogShowing()) {
                        dismissLoadingDialog();
                    }
                } else {
                    finish();
                }
            }
        });
    }

    private void registerAuthLauncher() {
        authLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
            @Override
            public void onActivityResult(ActivityResult o) {
                setResult(o.getResultCode(), o.getData());
                finish();
            }
        });
    }

    private void initView() {
        String url = getIntent().getStringExtra(SeafileAuthenticatorActivity.SINGLE_SIGN_ON_SERVER_URL);
        if (!TextUtils.isEmpty(url)) {
            binding.serverEditText.setText(url);
            int len = url.length();
            binding.serverEditText.setSelection(len, len);
        } else {
            binding.serverEditText.setText(SINGLE_SIGN_ON_HTTPS_PREFIX);
            int prefixLen = SINGLE_SIGN_ON_HTTPS_PREFIX.length();
            binding.serverEditText.setSelection(prefixLen, prefixLen);
        }

        binding.nextBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                doNext();
            }
        });

        applyEdgeToEdge(binding.getRoot());
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        if (getSupportActionBar() != null) {
            getSupportActionBar().setDisplayHomeAsUpEnabled(true);
            getSupportActionBar().setTitle(R.string.shib_login_title);
        }
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return false;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                finish();
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoadingDialog(aBoolean);
            }
        });

        getViewModel().getServerInfoLiveData().observe(this, new Observer<ServerInfoModel>() {
            @Override
            public void onChanged(ServerInfoModel serverInfoModel) {
                String host = getServerHost();
                if (CollectionUtils.isEmpty(serverInfoModel.features)) {
                    dismissLoadingDialog();
                    openAuthorizePage(host);
                    return;
                }

                if (!serverInfoModel.features.contains("client-sso-via-local-browser")) {
                    dismissLoadingDialog();
                    openAuthorizePage(host);
                    return;
                }

                getViewModel().getSsoLink(host);

            }
        });

        getViewModel().getSsoLinkLiveData().observe(this, new Observer<String>() {
            @Override
            public void onChanged(String s) {
                openLocalBrowser(s);
            }
        });

        getViewModel().getSsoStatusLiveData().observe(this, new Observer<String>() {
            @Override
            public void onChanged(String s) {
                SLogs.e(s);
                startDelayedAction();
            }
        });

        getViewModel().getAccountLiveData().observe(this, new Observer<Account>() {
            @Override
            public void onChanged(Account account) {
                onLoggedIn(account);
            }
        });
    }

    private void doNext() {
        String host = getServerHost();
        if (isServerHostValid(host)) {
            getViewModel().loadServerInfo(host);
        }
    }

    private boolean isServerHostValid(String hostUrl) {
        if (TextUtils.isEmpty(hostUrl)) {
            Toasts.show(R.string.shib_server_url_empty);
            return false;
        }

        if (!hostUrl.startsWith(SINGLE_SIGN_ON_HTTPS_PREFIX)) {
            Toasts.show(getString(R.string.shib_server_incorrect_prefix));
            return false;
        }

        String serverUrl1 = hostUrl
                .toLowerCase(Locale.ROOT)
                .replace("https://", "")
                .replace("http://", "");
        if (TextUtils.isEmpty(serverUrl1)) {
            Toasts.show(R.string.err_server_andress_empty);
            return false;
        }

        return true;
    }

    private String getServerHost() {
        Editable editable = binding.serverEditText.getText();
        if (null == editable) {
            return null;
        }

        String host = editable.toString().trim();
        if (!host.endsWith("/")) {
            host = host + "/";
        }
        return host;
    }

    private void openAuthorizePage(String serverUrl) {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_down);
            return;
        }

        Intent intent = new Intent(this, SingleSignOnAuthorizeActivity.class);
        intent.putExtra(SeafileAuthenticatorActivity.SINGLE_SIGN_ON_SERVER_URL, serverUrl);
        intent.putExtras(getIntent());
        authLauncher.launch(intent);
    }


    private String ssoLink = null;

    private void openLocalBrowser(String url) {
        ssoLink = url;
        WidgetUtils.openUrlByLocalBrowser(this, ssoLink);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        getSsoStatus();
    }

    @Override
    protected void onStop() {
        super.onStop();
        getViewModel().getRefreshLiveData().setValue(false);
        stopAction();
    }

    private void getSsoStatus() {
        if (TextUtils.isEmpty(ssoLink)) {
            return;
        }

        // https://host/client-sso/13de82ce0861430ba5a9f672cf89fe41fbaa6c7c94487b92ff8c8d76c260/
        String link = StringUtils.trimEnd(ssoLink, "/");
        String token = link.substring(link.lastIndexOf("/") + 1);
        String host = getServerHost();
        getViewModel().getSsoStatus(host, token);
    }

    private void onLoggedIn(Account account) {
        Intent retData = new Intent();
//        retData.putExtras(getIntent());
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_NAME, account.getSignature());
        retData.putExtra(android.accounts.AccountManager.KEY_AUTHTOKEN, account.getToken());
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_TYPE, getIntent().getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE));


        retData.putExtra(SeafileAuthenticatorActivity.ARG_EMAIL, account.getEmail());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_NAME, account.getName());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SHIB, account.is_shib);
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SERVER_URI, account.getServer());

        retData.putExtra(SeafileAuthenticatorActivity.ARG_AVATAR_URL, account.getAvatarUrl());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SPACE_TOTAL, account.getTotalSpace());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SPACE_USAGE, account.getUsageSpace());
        retData.putExtra(SeafileAuthenticatorActivity.ARG_SHIB, true);

        setResult(RESULT_OK, retData);
        finish();
    }


    private Timer timer;

    public void startDelayedAction() {
        if (timer != null) {
            timer.cancel();
        }

        timer = new Timer();
        timer.schedule(new TimerTask() {
            @Override
            public void run() {
                runOnUiThread(new TimerTask() {
                    @Override
                    public void run() {
                        getSsoStatus();
                    }
                });
            }
        }, 2 * 1000);
    }

    public void stopAction() {
        if (timer != null) {
            timer.cancel();
            timer = null;
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        stopAction();
    }
}
