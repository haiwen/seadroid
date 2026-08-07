package com.seafile.seadroid2.ui.account.sso;

import android.content.DialogInterface;
import android.content.Intent;
import android.net.InetAddresses;
import android.net.Uri;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.Editable;
import android.text.TextUtils;
import android.util.Patterns;
import android.view.MenuItem;
import android.view.View;

import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.widget.Toolbar;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.SingleSignOnWelcomeLayoutBinding;
import com.seafile.seadroid2.framework.model.server.ServerInfoModel;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.account.SeafileAuthenticatorActivity;
import com.seafile.seadroid2.ui.base.BaseActivityWithVM;

import java.util.Locale;

/**
 * Single Sign-On welcome page
 */
public class SingleSignOnActivity extends BaseActivityWithVM<SingleSignOnViewModel> {
    public static final String DEBUG_TAG = "SingleSignOnActivity";

    public static final String SINGLE_SIGN_ON_HTTPS_PREFIX = "https://";

    private SingleSignOnWelcomeLayoutBinding binding;

    private ActivityResultLauncher<Intent> authLauncher;
    private boolean _SsoStatusPeriodicityStatus = false;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        binding = SingleSignOnWelcomeLayoutBinding.inflate(getLayoutInflater());
        setContentView(binding.getRoot());
        initToolbar();

        applyEdgeToEdge(binding.getRoot());

        registerAuthLauncher();

        initView();
        initViewModel();


        setOnDismissListener(new DialogInterface.OnDismissListener() {
            @Override
            public void onDismiss(DialogInterface dialog) {
                if (_SsoStatusPeriodicityStatus) {
                    _SsoStatusPeriodicityStatus = false;

                    stopAction();

                    dismissLoadingDialog();
                }
            }
        });
    }


    private void initView() {
        Toolbar toolbar = getActionBarToolbar();
        toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                finish();
            }
        });
        if (getSupportActionBar() != null) {
            getSupportActionBar().setTitle(R.string.shib_login_title);
        }

        getOnBackPressedDispatcher().addCallback(new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                finish();
            }
        });

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
                if (TextUtils.isEmpty(s)) {
                    _SsoStatusPeriodicityStatus = false;

                    dismissLoadingDialog();
                } else {
                    startDelayedAction();
                }
            }
        });

        getViewModel().getAccountLiveData().observe(this, new Observer<Account>() {
            @Override
            public void onChanged(Account account) {
                _SsoStatusPeriodicityStatus = false;
                dismissLoadingDialog();

                if (account != null) {
                    onLoggedIn(account);
                }
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

        Uri uri = Uri.parse(hostUrl);
        String host = uri.getHost();
        if (TextUtils.isEmpty(host)) {
            Toasts.show(R.string.err_server_andress_empty);
            return false;
        }

        host = host.toLowerCase(Locale.ROOT);
        int port = uri.getPort();
        if (port != -1 && (port < 1 || port > 65535)) {
            Toasts.show(R.string.invalid_server_address);
            return false;
        }

        boolean isValidDomain = Patterns.DOMAIN_NAME.matcher(host).matches();
        boolean isValidIp;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            isValidIp = InetAddresses.isNumericAddress(host);
        } else {
            isValidIp = Patterns.IP_ADDRESS.matcher(host).matches();
        }
        if (!isValidDomain && !isValidIp) {
            Toasts.show(R.string.invalid_server_address);
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

        if (!TextUtils.isEmpty(ssoLink)) {
            _SsoStatusPeriodicityStatus = true;

            showLoadingDialog();

            startDelayedAction();
        }
    }

    @Override
    protected void onStop() {
        super.onStop();

        dismissLoadingDialog();

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
        retData.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_TYPE, getIntent().getStringExtra(Constants.AccountKeys.ARG_ACCOUNT_TYPE));

        retData.putExtra(Constants.AccountKeys.ARG_EMAIL, account.getEmail());
        retData.putExtra(Constants.AccountKeys.ARG_CONTACT_EMAIL, account.getContactEmail());
        retData.putExtra(Constants.AccountKeys.ARG_NAME, account.getName());
        retData.putExtra(Constants.AccountKeys.ARG_SERVER_URI, account.getServer());
        retData.putExtra(Constants.AccountKeys.ARG_AVATAR_URL, account.getAvatarUrl());
        retData.putExtra(Constants.AccountKeys.ARG_SPACE_TOTAL, account.getTotalSpace());
        retData.putExtra(Constants.AccountKeys.ARG_SPACE_USAGE, account.getUsageSpace());
        retData.putExtra(Constants.AccountKeys.ARG_SHIB, true);

        setResult(RESULT_OK, retData);
        finish();
    }


    private final Handler handler = new Handler(Looper.getMainLooper());
    private final Runnable delayedSsoStatusAction = this::getSsoStatus;

    public void startDelayedAction() {
        stopAction();
        handler.postDelayed(delayedSsoStatusAction, 2 * 1000L);
    }

    public void stopAction() {
        handler.removeCallbacks(delayedSsoStatusAction);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();

        dismissLoadingDialog();

        stopAction();
    }
}
