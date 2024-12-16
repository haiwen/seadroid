package com.seafile.seadroid2.ui.account;

import android.accounts.Account;
import android.accounts.AccountManager;
import android.app.Activity;
import android.content.ContentResolver;
import android.content.Intent;
import android.content.pm.PackageManager;
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
import androidx.core.app.ComponentActivity;
import androidx.core.app.NavUtils;
import androidx.core.app.TaskStackBuilder;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Authenticator;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.framework.http.HttpIO;
import com.seafile.seadroid2.framework.util.SLogs;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;

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

    public final static String ARG_ACCOUNT_TYPE = "ACCOUNT_TYPE";
    public final static String ARG_ACCOUNT_NAME = "ACCOUNT_NAME";
    public final static String ARG_SERVER_URI = "SERVER_URI";
    public final static String ARG_EDIT_OLD_ACCOUNT_NAME = "EDIT_OLD_ACCOUNT";
    public final static String ARG_EMAIL = "EMAIL";
    public final static String ARG_AVATAR_URL = "AVATAR_URL";
    public final static String ARG_SPACE_TOTAL = "SPACE_TOTAL";
    public final static String ARG_SPACE_USAGE = "SPACE_USAGE";
    public final static String ARG_NAME = "NAME";
    public final static String ARG_SHIB = "SHIB";
    public final static String ARG_LOGIN_TIME = "LOGIN_TIME";
    public final static String ARG_AUTH_SESSION_KEY = "TWO_FACTOR_AUTH";
    public final static String ARG_IS_EDITING = "isEdited";

    private static final int REQ_SIGNUP = 1;

    private final String DEBUG_TAG = this.getClass().getSimpleName();

    /**
     * Called when the activity is first created.
     */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onCreate");
        super.onCreate(savedInstanceState);
        setContentView(R.layout.account_create_type_select);


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

                if (!isZH) {
                    id++;
                }

                if (id == SEACLOUD_CC) {
                    intent = new Intent(SeafileAuthenticatorActivity.this, AccountDetailActivity.class);
                    intent.putExtras(getIntent());
                    intent.putExtra(SeafileAuthenticatorActivity.ARG_SERVER_URI, getString(R.string.server_url_seacloud));
                } else if (id == SINGLE_SIGN_ON_LOGIN) {
                    intent = new Intent(SeafileAuthenticatorActivity.this, SingleSignOnActivity.class);
                    intent.putExtras(getIntent());
                } else if (id == OTHER_SERVER) {
                    intent = new Intent(SeafileAuthenticatorActivity.this, AccountDetailActivity.class);
                    intent.putExtras(getIntent());
                }


                if (intent != null) {
                    activityLauncher.launch(intent);
                }

            }
        });

        if (getIntent().getBooleanExtra(ARG_SHIB, false)) {

            Intent intent = new Intent(this, SingleSignOnAuthorizeActivity.class);
            Account account = new Account(getIntent().getStringExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_NAME), Constants.Account.ACCOUNT_TYPE);

            String serverUrl = SupportAccountManager.getInstance().getUserData(account, Authenticator.KEY_SERVER_URI);
            intent.putExtra(SeafileAuthenticatorActivity.SINGLE_SIGN_ON_SERVER_URL, serverUrl);
            if (getIntent() != null) {
                intent.putExtras(getIntent().getExtras());
            }
            activityLauncher.launch(intent);

        } else if (getIntent().getBooleanExtra(ARG_IS_EDITING, false)) {

            Intent intent = new Intent(this, AccountDetailActivity.class);
            if (getIntent() != null) {
                intent.putExtras(getIntent().getExtras());
            }
            activityLauncher.launch(intent);
        }

        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.choose_server);
        toolbar.setNavigationOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                navigateUpOrBack(SeafileAuthenticatorActivity.this, null);
            }
        });
    }

    /**
     * This utility method handles Up navigation intents by searching for a parent activity and
     * navigating there if defined. When using this for an activity make sure to define both the
     * native parentActivity as well as the AppCompat one when supporting API levels less than 16.
     * when the activity has a single parent activity. If the activity doesn't have a single parent
     * activity then don't define one and this method will use back button functionality. If "Up"
     * functionality is still desired for activities without parents then use
     * {@code syntheticParentActivity} to define one dynamically.
     * <p>
     * Note: Up navigation intents are represented by a back arrow in the top left of the Toolbar
     * in Material Design guidelines.
     *
     * @param currentActivity         Activity in use when navigate Up action occurred.
     * @param syntheticParentActivity Parent activity to use when one is not already configured.
     */
    public void navigateUpOrBack(ComponentActivity currentActivity, Class<? extends Activity> syntheticParentActivity) {
        // Retrieve parent activity from AndroidManifest.
        Intent intent = NavUtils.getParentActivityIntent(currentActivity);

        // Synthesize the parent activity when a natural one doesn't exist.
        if (intent == null && syntheticParentActivity != null) {
            try {
                intent = NavUtils.getParentActivityIntent(currentActivity, syntheticParentActivity);
            } catch (PackageManager.NameNotFoundException e) {
                e.printStackTrace();
            }
        }

        if (intent == null) {
            // No parent defined in manifest. This indicates the activity may be used by
            // in multiple flows throughout the app and doesn't have a strict parent. In
            // this case the navigation up button should act in the same manner as the
            // back button. This will result in users being forwarded back to other
            // applications if currentActivity was invoked from another application.
            currentActivity.onBackPressed();
        } else {
            if (NavUtils.shouldUpRecreateTask(currentActivity, intent)) {
                // Need to synthesize a backstack since currentActivity was probably invoked by a
                // different app. The preserves the "Up" functionality within the app according to
                // the activity hierarchy defined in AndroidManifest.xml via parentActivity
                // attributes.
                TaskStackBuilder builder = TaskStackBuilder.create(currentActivity);
                builder.addNextIntentWithParentStack(intent);
                builder.startActivities();
            } else {
                // Navigate normally to the manifest defined "Up" activity.
                NavUtils.navigateUpTo(currentActivity, intent);
            }
        }
    }

    private final ActivityResultLauncher<Intent> activityLauncher = registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), new ActivityResultCallback<ActivityResult>() {
        @Override
        public void onActivityResult(ActivityResult o) {
            if (o == null) {
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

    private void finishLogin(Intent intent) {
        SLogs.d(DEBUG_TAG, "finishLogin");

        String newAccountName = intent.getStringExtra(AccountManager.KEY_ACCOUNT_NAME);
        String accountType = intent.getStringExtra(AccountManager.KEY_ACCOUNT_TYPE);
        String authToken = intent.getStringExtra(AccountManager.KEY_AUTHTOKEN);

        String avatarUrl = intent.getStringExtra(ARG_AVATAR_URL);
        String email = intent.getStringExtra(ARG_EMAIL);
        String name = intent.getStringExtra(ARG_NAME);
        String sessionKey = intent.getStringExtra(ARG_AUTH_SESSION_KEY);
        String serverUri = intent.getStringExtra(ARG_SERVER_URI);
        boolean shib = intent.getBooleanExtra(ARG_SHIB, false);
        long totalSpace = intent.getLongExtra(SeafileAuthenticatorActivity.ARG_SPACE_TOTAL, 0L);
        long usageSpace = intent.getLongExtra(SeafileAuthenticatorActivity.ARG_SPACE_USAGE, 0L);

        //new android account
        final Account newAccount = new Account(newAccountName, accountType);

        int cameraIsSyncable = 0;
        boolean cameraSyncAutomatically = true;

        if (intent.getBooleanExtra(ARG_IS_EDITING, false)) {

            String oldAccountName = intent.getStringExtra(ARG_EDIT_OLD_ACCOUNT_NAME);
            final Account oldAccount = new Account(oldAccountName, intent.getStringExtra(AccountManager.KEY_ACCOUNT_TYPE));

            // serverUri and mail stay the same. so just update the token and exit
            if (oldAccount.equals(newAccount)) {

                SupportAccountManager.getInstance().setAuthToken(newAccount, Authenticator.AUTHTOKEN_TYPE, authToken);
                SupportAccountManager.getInstance().setUserData(newAccount, Authenticator.SESSION_KEY, sessionKey);

                Bundle result = new Bundle();
                result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, true);
                result.putString(AccountManager.KEY_ACCOUNT_NAME, newAccountName);
                setAccountAuthenticatorResult(result);
                setResult(RESULT_OK, intent);
                finish();
                return;
            }

            Log.d(DEBUG_TAG, "removing old account " + oldAccountName);

            cameraIsSyncable = ContentResolver.getIsSyncable(oldAccount, CameraUploadManager.AUTHORITY);
            cameraSyncAutomatically = ContentResolver.getSyncAutomatically(oldAccount, CameraUploadManager.AUTHORITY);

            SupportAccountManager.getInstance().removeAccount(oldAccount, null, null);
        }

        Log.d(DEBUG_TAG, "adding new account " + newAccountName);

        Bundle bundle = new Bundle();
        bundle.putString(Authenticator.KEY_SERVER_URI, serverUri);
        bundle.putString(Authenticator.KEY_EMAIL, email);
        bundle.putString(Authenticator.KEY_NAME, name);
        bundle.putString(Authenticator.KEY_AVATAR_URL, avatarUrl);
        bundle.putString(Authenticator.SESSION_KEY, sessionKey);
        bundle.putString(Authenticator.LOGIN_TIME, String.valueOf(System.currentTimeMillis()));
        bundle.putString(Authenticator.SPACE_TOTAL, String.valueOf(totalSpace));
        bundle.putString(Authenticator.SPACE_USAGE, String.valueOf(usageSpace));

        //add account
        SupportAccountManager.getInstance().addAccountExplicitly(newAccount, null, bundle);
        SupportAccountManager.getInstance().setAuthToken(newAccount, Authenticator.AUTHTOKEN_TYPE, authToken);


        if (shib) {
            SupportAccountManager.getInstance().setUserData(newAccount, Authenticator.KEY_SHIB, "shib");
        }

        //save current account
        SupportAccountManager.getInstance().saveCurrentAccount(newAccountName);
        //reset httpio instance
        HttpIO.resetLoggedInInstance();

        // set sync settings
        ContentResolver.setIsSyncable(newAccount, CameraUploadManager.AUTHORITY, cameraIsSyncable);
        ContentResolver.setSyncAutomatically(newAccount, CameraUploadManager.AUTHORITY, cameraSyncAutomatically);

        Bundle result = new Bundle();
        result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, true);
        result.putString(AccountManager.KEY_ACCOUNT_NAME, newAccountName);
        setAccountAuthenticatorResult(result);
        setResult(RESULT_OK, intent);
        finish();
    }
}
