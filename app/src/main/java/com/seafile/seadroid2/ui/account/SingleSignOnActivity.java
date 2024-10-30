package com.seafile.seadroid2.ui.account;

import android.content.Intent;
import android.os.Bundle;

import androidx.appcompat.widget.Toolbar;

import android.text.TextUtils;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

import com.blankj.utilcode.util.RegexUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.BaseActivity;

import java.util.Locale;

/**
 * Single Sign-On welcome page
 * <p/>
 */
public class SingleSignOnActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    public static final String DEBUG_TAG = "SingleSignOnActivity";

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
                if (isServerUrlValid(url)) {
                    openAuthorizePage(url);
                }
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
        if (TextUtils.isEmpty(serverUrl)) {
            ToastUtils.showLong(R.string.shib_server_url_empty);
            return false;
        }

        if (!serverUrl.startsWith(SINGLE_SIGN_ON_HTTPS_PREFIX)) {
            ToastUtils.showLong(getString(R.string.shib_server_incorrect_prefix));
            return false;
        }

        String serverUrl1 = serverUrl
                .toLowerCase(Locale.ROOT)
                .replace("https://", "")
                .replace("http://", "");
        if (TextUtils.isEmpty(serverUrl1)) {
            return false;
        }

        return true;
    }

    private String getServerUrl() {
        String serverUrl = mServerUrlEt.getText().toString().trim();
        return serverUrl;
    }

    private void openAuthorizePage(String serverUrl) {
        Intent intent = new Intent(this, SingleSignOnAuthorizeActivity.class);
        intent.putExtra(SeafileAuthenticatorActivity.SINGLE_SIGN_ON_SERVER_URL, serverUrl);
        intent.putExtras(getIntent());
        startActivityForResult(intent, SINGLE_SIGN_ON_AUTH);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        Log.d(DEBUG_TAG, "onActivityResult");

        // pass auth result back to the SeafileAuthenticatorActivity
        if (requestCode == SINGLE_SIGN_ON_AUTH) {
            setResult(resultCode, data);
            finish();
        }
    }
}
