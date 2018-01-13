package com.seafile.seadroid2.account;

import android.accounts.*;
import android.accounts.AccountManager;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.text.TextUtils;
import android.util.Log;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.ui.SeafileAuthenticatorActivity;
import com.seafile.seadroid2.data.DataManager;

import org.json.JSONException;

import java.net.HttpURLConnection;

/*
 * Seafile Authenticator.
 *
 */
public class Authenticator extends AbstractAccountAuthenticator {

    private String DEBUG_TAG = "SeafileAuthenticator";
    private final Context context;

    private com.seafile.seadroid2.account.AccountManager manager;

    /**
     * Type of the auth token used (there is only one type)
     */
    public static final String AUTHTOKEN_TYPE = "api2";

    /**
     * Key of Server URI in userData
     */
    public final static String KEY_SERVER_URI = "server";

    /**
     * Key of email in userData
     */
    public final static String KEY_EMAIL = "email";

    /**
     * Key of Server version in userData
     */
    public final static String KEY_SERVER_VERSION = "version";

    /**
     * Key of Server Feature-list in userData
     */
    public final static String KEY_SERVER_FEATURES = "features";

    /**
     * Key of shib_setting in userData
     */
    public final static String KEY_SHIB = "shib";
    /**
     * Two Factor Auth in  userData
     */
    public final static String SESSION_KEY = "sessionKey";

    public Authenticator(Context context) {
        super(context);
        Log.d(DEBUG_TAG, "SeafileAuthenticator created.");
        this.context = context;
        this.manager = new com.seafile.seadroid2.account.AccountManager(context);
    }

    /**
     * We have no properties.
     */
    @Override
    public Bundle editProperties(
            AccountAuthenticatorResponse r, String s) {
        Log.d(DEBUG_TAG, "editProperties");

        throw new UnsupportedOperationException();
    }

    @Override
    public Bundle addAccount(AccountAuthenticatorResponse response,
                             String accountType,
                             String authTokenType,
                             String[] requiredFeatures,
                             Bundle options) throws NetworkErrorException {

        Log.d(DEBUG_TAG, "addAccount of type "+accountType);

        if (authTokenType != null && !authTokenType.equals(Authenticator.AUTHTOKEN_TYPE)) {
            Bundle result = new Bundle();
            result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, false);
            result.putInt(AccountManager.KEY_ERROR_CODE, AccountManager.ERROR_CODE_BAD_ARGUMENTS);
            return result;
        }

        final Intent intent = new Intent(context, SeafileAuthenticatorActivity.class);
        intent.putExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE, accountType);
        intent.putExtra(SeafileAuthenticatorActivity.ARG_IS_EDITING, false);
        intent.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE, response);

        final Bundle bundle = new Bundle();
        bundle.putParcelable(android.accounts.AccountManager.KEY_INTENT, intent);
        return bundle;
    }

    @Override
    public Bundle confirmCredentials(
            AccountAuthenticatorResponse response,
            android.accounts.Account account,
            Bundle bundle) throws NetworkErrorException {
        Log.d(DEBUG_TAG, "confirmCredentials");

        Account a = manager.getSeafileAccount(account);
        DataManager manager = new DataManager(a);

        try {
            // test auth token
            manager.getAccountInfo();
        } catch (SeafException e) {
            if (e.getCode() == HttpURLConnection.HTTP_UNAUTHORIZED) {
                // Token is invalid
                Bundle result = new Bundle();
                result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, false);
                result.putInt(AccountManager.KEY_ERROR_CODE, AccountManager.ERROR_CODE_INVALID_RESPONSE);
                result.putString(AccountManager.KEY_ERROR_MESSAGE, "Authentication error.");
                return result;
            } else {
                // could not test token
                throw new NetworkErrorException(e);
            }
        } catch (JSONException e) {
            throw new NetworkErrorException(e);
        }

        // token is valid
        Bundle result = new Bundle();
        result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, true);
        return result;
    }

    @Override
    public Bundle getAuthToken(AccountAuthenticatorResponse response,
                               android.accounts.Account account,
                               String authTokenType,
                               Bundle options) throws NetworkErrorException {
        Log.d(DEBUG_TAG, "getAuthToken");

        if (authTokenType != null && !authTokenType.equals(Authenticator.AUTHTOKEN_TYPE)) {
            final Bundle result = new Bundle();
            result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, false);
            result.putInt(AccountManager.KEY_ERROR_CODE, AccountManager.ERROR_CODE_BAD_ARGUMENTS);
            return result;
        }

        final AccountManager am = AccountManager.get(context);
        String authToken = am.peekAuthToken(account, authTokenType);

        if (!TextUtils.isEmpty(authToken)) {
            final Bundle result = new Bundle();
            result.putString(AccountManager.KEY_ACCOUNT_NAME, account.name);
            result.putString(AccountManager.KEY_ACCOUNT_TYPE, account.type);
            result.putString(AccountManager.KEY_AUTHTOKEN, authToken);
            return result;
        }

        // there is no auth token -> the account is signed-out.

        final Bundle result = new Bundle();
        result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, false);
        result.putInt(AccountManager.KEY_ERROR_CODE, AccountManager.ERROR_CODE_BAD_REQUEST);
        result.putString(android.accounts.AccountManager.KEY_ERROR_MESSAGE, "Account is not signed in. Not auth token available.");
        return result;
    }

    @Override
    public String getAuthTokenLabel(String authTokenType) {
        return "Seafile";
    }

    @Override
    public Bundle updateCredentials(AccountAuthenticatorResponse response,
            android.accounts.Account account,
            String authTokenType, Bundle options) throws NetworkErrorException {
        Log.d(DEBUG_TAG, "updateCredentials");

        if (authTokenType != null && !authTokenType.equals(Authenticator.AUTHTOKEN_TYPE)) {
            final Bundle result = new Bundle();
            result.putBoolean(AccountManager.KEY_BOOLEAN_RESULT, false);
            result.putInt(AccountManager.KEY_ERROR_CODE, AccountManager.ERROR_CODE_BAD_ARGUMENTS);
            return result;
        }

        final Intent intent = new Intent(context, SeafileAuthenticatorActivity.class);
        intent.putExtra(android.accounts.AccountManager.KEY_ACCOUNT_AUTHENTICATOR_RESPONSE, response);
        intent.putExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_TYPE, account.type);
        intent.putExtra(SeafileAuthenticatorActivity.ARG_ACCOUNT_NAME, account.name); // will be overridden
        intent.putExtra(SeafileAuthenticatorActivity.ARG_EDIT_OLD_ACCOUNT_NAME, account.name);
        intent.putExtra(SeafileAuthenticatorActivity.ARG_IS_EDITING, true);
        boolean is_shib = manager.getSeafileAccount(account).isShib();
        intent.putExtra(SeafileAuthenticatorActivity.ARG_SHIB, is_shib);

        final Bundle bundle = new Bundle();
        bundle.putParcelable(android.accounts.AccountManager.KEY_INTENT, intent);
        return bundle;
    }

    @Override
    public Bundle hasFeatures(AccountAuthenticatorResponse r,
            android.accounts.Account account, String[] strings) throws NetworkErrorException {
        Log.d(DEBUG_TAG, "hasFeatures");

        final Bundle result = new Bundle();
        result.putBoolean(android.accounts.AccountManager.KEY_BOOLEAN_RESULT, true);
        return result;
    }
}