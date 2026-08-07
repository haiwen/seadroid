package com.seafile.seadroid2.ui.account;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;

import androidx.activity.result.ActivityResult;
import androidx.activity.result.ActivityResultCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.appcompat.widget.Toolbar;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.context.ContextStackPreferenceHelper;
import com.seafile.seadroid2.framework.datastore.DataStoreKeys;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.preferences.Settings;
import com.seafile.seadroid2.ui.account.sso.SingleSignOnActivity;

import java.util.Locale;

/**
 * The Authenticator activity.
 * <p>
 * Called by the Authenticator and in charge of identifing the user.
 * <p>
 * It sends back to the Authenticator the result.
 */
public class SeafileAuthenticatorActivity extends BaseAuthenticatorActivity {
    public static final String SINGLE_SIGN_ON_SERVER_URL = "single sign on server url";

    public static final int SEACLOUD_CC = 0;
    public static final int SINGLE_SIGN_ON_LOGIN = 1;
    public static final int OTHER_SERVER = 2;

    private static final int REQ_SIGNUP = 1;
    private static final String STATE_PENDING_FLOW = "pending_flow";
    private static final int FLOW_NONE = 0;
    private static final int FLOW_ACCOUNT_DETAIL = 1;
    private static final int FLOW_SSO = 2;

    private final String DEBUG_TAG = this.getClass().getSimpleName();
    private ActivityResultLauncher<Intent> activityLauncher;
    private int pendingFlow = FLOW_NONE;
    private boolean hasLaunchedChildFlow;

    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onCreate");
        super.onCreate(savedInstanceState);

        if (savedInstanceState != null) {
            pendingFlow = savedInstanceState.getInt(STATE_PENDING_FLOW, FLOW_NONE);
        }

        setContentView(R.layout.account_create_type_select);
        initToolbar();

        applyEdgeToEdge(findViewById(R.id.root_layout));

        initView();
        initListView();
        initLauncher();

        if (!maybeRestoreAuthFlow()) {
            if (getIntent().getBooleanExtra(Constants.AccountKeys.ARG_SHIB, false)) {
                launchSingleSignOnFlow();
            } else if (getIntent().getBooleanExtra(Constants.AccountKeys.ARG_IS_EDITING, false)) {
                launchEditAccountFlow();
            }
        }
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
            getSupportActionBar().setTitle(R.string.choose_server);
        }
    }

    private void initListView() {


        String country = Locale.getDefault().getCountry();
        String language = Locale.getDefault().getLanguage();
        boolean isZH = TextUtils.equals("CN", country) || TextUtils.equals("zh", language);

        String[] array = getResources().getStringArray(R.array.choose_server_array);
        String[] strArray;
        if (isZH) {
            strArray = new String[1 + array.length];
            strArray[0] = getString(R.string.server_name_top);
            System.arraycopy(array, 0, strArray, 1, array.length);
        } else {
            strArray = new String[array.length];
            System.arraycopy(array, 0, strArray, 0, array.length);
        }


        ArrayAdapter<String> listAdapter = new ArrayAdapter<>(this, R.layout.list_item_authenticator, strArray);
        ListView listView = (ListView) findViewById(R.id.account_create_list);
        listView.setAdapter(listAdapter);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                Intent intent = null;
                int flow = FLOW_NONE;

                if (!isZH) {
                    id++;
                }

                if (id == SEACLOUD_CC) {
                    intent = new Intent(SeafileAuthenticatorActivity.this, AccountDetailActivity.class);
                    intent.putExtras(getIntent());
                    intent.putExtra(Constants.AccountKeys.ARG_SERVER_URI, getString(R.string.server_url_seacloud));
                    flow = FLOW_ACCOUNT_DETAIL;
                } else if (id == SINGLE_SIGN_ON_LOGIN) {
                    intent = new Intent(SeafileAuthenticatorActivity.this, SingleSignOnActivity.class);
                    intent.putExtras(getIntent());
                    flow = FLOW_SSO;
                } else if (id == OTHER_SERVER) {
                    intent = new Intent(SeafileAuthenticatorActivity.this, AccountDetailActivity.class);
                    intent.putExtras(getIntent());
                    flow = FLOW_ACCOUNT_DETAIL;
                }

                if (intent != null) {
                    launchAuthFlow(intent, flow);
                }

            }
        });
    }

    private void initLauncher() {

        activityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
            @Override
            public void onActivityResult(ActivityResult o) {
                pendingFlow = FLOW_NONE;
                hasLaunchedChildFlow = false;

                if (o == null || o.getData() == null) {
                    finish();
                    return;
                }

                if (o.getResultCode() == RESULT_OK) {
                    finishLogin(o.getData());
                } else {
                    finish();
                }
            }
        });
    }

    @Override
    protected void onSaveInstanceState(Bundle outState) {
        outState.putInt(STATE_PENDING_FLOW, pendingFlow);
        super.onSaveInstanceState(outState);
    }

    private boolean maybeRestoreAuthFlow() {
        if (hasLaunchedChildFlow || pendingFlow == FLOW_NONE) {
            return false;
        }

        if (pendingFlow == FLOW_SSO) {
            launchSingleSignOnFlow();
            return true;
        }

        if (pendingFlow == FLOW_ACCOUNT_DETAIL) {
            launchEditAccountFlow();
            return true;
        }

        return false;
    }

    private void launchSingleSignOnFlow() {
        Intent intent = new Intent(this, SingleSignOnActivity.class);
        Account account = new Account(getIntent().getStringExtra(Constants.AccountKeys.ARG_ACCOUNT_NAME), Constants.Account.ACCOUNT_TYPE);

        String serverUrl = SupportAccountManager.getInstance().getUserData(account, Authenticator.KEY_SERVER_URI);
        intent.putExtra(SeafileAuthenticatorActivity.SINGLE_SIGN_ON_SERVER_URL, serverUrl);
        if (getIntent() != null) {
            intent.putExtras(getIntent().getExtras());
        }
        launchAuthFlow(intent, FLOW_SSO);
    }

    private void launchEditAccountFlow() {
        Intent intent = new Intent(this, AccountDetailActivity.class);
        if (getIntent() != null) {
            intent.putExtras(getIntent().getExtras());
        }
        launchAuthFlow(intent, FLOW_ACCOUNT_DETAIL);
    }

    private void launchAuthFlow(Intent intent, int flow) {
        if (hasLaunchedChildFlow) {
            return;
        }

        pendingFlow = flow;
        hasLaunchedChildFlow = true;
        activityLauncher.launch(intent);
    }

    private void finishLogin(Intent intent) {
        SLogs.d(DEBUG_TAG, "finishLogin");

        String newAccountName = intent.getStringExtra(AccountManager.KEY_ACCOUNT_NAME);
        String accountType = intent.getStringExtra(AccountManager.KEY_ACCOUNT_TYPE);
        String authToken = intent.getStringExtra(AccountManager.KEY_AUTHTOKEN);
        if (TextUtils.isEmpty(newAccountName) || TextUtils.isEmpty(accountType) || TextUtils.isEmpty(authToken)) {
            finish();
            return;
        }

        String avatarUrl = intent.getStringExtra(Constants.AccountKeys.ARG_AVATAR_URL);
        String email = intent.getStringExtra(Constants.AccountKeys.ARG_EMAIL);
        String contactEmail = intent.getStringExtra(Constants.AccountKeys.ARG_CONTACT_EMAIL);
        String name = intent.getStringExtra(Constants.AccountKeys.ARG_NAME);
        String sessionKey = intent.getStringExtra(Constants.AccountKeys.ARG_AUTH_SESSION_KEY);
        String serverUri = intent.getStringExtra(Constants.AccountKeys.ARG_SERVER_URI);
        boolean shib = intent.getBooleanExtra(Constants.AccountKeys.ARG_SHIB, false);
        long totalSpace = intent.getLongExtra(Constants.AccountKeys.ARG_SPACE_TOTAL, 0L);
        long usageSpace = intent.getLongExtra(Constants.AccountKeys.ARG_SPACE_USAGE, 0L);

        Bundle bundle = new Bundle();
        bundle.putBoolean(Authenticator.KEY_SHIB, shib);
        bundle.putString(Authenticator.KEY_SERVER_URI, serverUri);
        bundle.putString(Authenticator.KEY_EMAIL, email);
        bundle.putString(Authenticator.KEY_CONTACT_EMAIL, contactEmail);
        bundle.putString(Authenticator.KEY_NAME, name);
        bundle.putString(Authenticator.KEY_AVATAR_URL, avatarUrl);
        bundle.putString(Authenticator.SESSION_KEY, sessionKey);
        bundle.putString(Authenticator.LOGIN_TIME, String.valueOf(System.currentTimeMillis()));
        bundle.putString(Authenticator.SPACE_TOTAL, String.valueOf(totalSpace));
        bundle.putString(Authenticator.SPACE_USAGE, String.valueOf(usageSpace));


        //new android account
        final android.accounts.Account androidAccount = new android.accounts.Account(newAccountName, accountType);
        //add account
        SupportAccountManager.getInstance().addAccountExplicitly(androidAccount, null, bundle);
        SupportAccountManager.getInstance().updateAuthToken(androidAccount, Authenticator.AUTHTOKEN_TYPE, authToken);
        if (shib) {
            SupportAccountManager.getInstance().updateShib(androidAccount, "shib");
        }

        // clear context stack
        ContextStackPreferenceHelper.clear(DataStoreKeys.KEY_GLOBAL_NAV_CONTEXT_STACK);

        // save current account
        SupportAccountManager.getInstance().saveCurrentAccount(newAccountName);

        // remove last path of share to seafile
        Settings.getCommonPreferences().edit().remove(DataStoreKeys.KEY_LAST_PATH_OF_SHARE_TO_SEAFILE).apply();

        // reset http instance
        HttpIO.resetLoggedInInstance();
        // reset settings
        Settings.initUserSettings();

        Bundle result = new Bundle();
        result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, true);
        result.putString(AccountManager.KEY_ACCOUNT_NAME, newAccountName);
        setAccountAuthenticatorResult(result);
        setResult(RESULT_OK, intent);
        finish();
    }
}
