package com.seafile.seadroid2.account.ui;

import android.content.Intent;
import android.os.Bundle;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.activity.BaseActivity;

/**
 * Single Sign-On welcome page
 * <p/>
 */
public class SingleSignOnActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    public static final String DEBUG_TAG = "SingleSignOnActivity";

    public static final String SINGLE_SIGN_ON_SERVER_URL = "single sign on server url";
    public static final String SINGLE_SIGN_ON_HTTPS_PREFIX = "https://";

    private Button mNextBtn;
    private EditText mServerUrlEt;

    private static final int SINGLE_SIGN_ON_AUTH = 1;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.single_sign_on_welcome_layout);
        mNextBtn = (Button) findViewById(R.id.single_sign_on_next_btn);
        mServerUrlEt = (EditText) findViewById(R.id.single_sign_on_server_url_et);

        mServerUrlEt.setText(SINGLE_SIGN_ON_HTTPS_PREFIX);
        int prefixLen = SINGLE_SIGN_ON_HTTPS_PREFIX.length();
        mServerUrlEt.setSelection(prefixLen, prefixLen);

        mNextBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                String url = getServerUrl();
                if (isServerUrlValid(url))
                    openAuthorizePage(url);
            }
        });

        Toolbar toolbar = getActionBarToolbar();
        toolbar.setOnMenuItemClickListener(this);
        setSupportActionBar(toolbar);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.shib_login_title);
    }

        @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                finish();
                break;
        }
        return true;
    }

    private boolean isServerUrlValid(String serverUrl) {
        if (serverUrl == null || serverUrl.isEmpty()) {
            showShortToast(this, getString(R.string.shib_server_url_empty));
            return false;
        }

        if (!serverUrl.startsWith(SINGLE_SIGN_ON_HTTPS_PREFIX)) {
            showShortToast(this, getString(R.string.shib_server_incorrect_prefix));
            return false;
        }

        return true;
    }

    private String getServerUrl() {
        String serverUrl = mServerUrlEt.getText().toString().trim();
        return serverUrl;
    }

    private void openAuthorizePage(String serverUrl) {
        Intent intent = new Intent(SingleSignOnActivity.this, SingleSignOnAuthorizeActivity.class);
        intent.putExtra(SINGLE_SIGN_ON_SERVER_URL, serverUrl);
        intent.putExtras(getIntent());
        startActivityForResult(intent, SINGLE_SIGN_ON_AUTH);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        Log.d(DEBUG_TAG, "onActivityResult");

        // pass auth result back to the SeafileAuthenticatorActivity
        if (requestCode == SINGLE_SIGN_ON_AUTH) {
            setResult(resultCode, data);
            finish();
        }
    }
}
