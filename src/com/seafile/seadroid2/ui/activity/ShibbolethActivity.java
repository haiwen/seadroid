package com.seafile.seadroid2.ui.activity;

import android.content.Intent;
import android.os.Bundle;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.View;
import android.widget.Button;
import android.widget.CheckBox;
import android.widget.EditText;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.MenuItem;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.LogUtils;

/**
 * Shibboleth welcome page
 * <p/>
 */
public class ShibbolethActivity extends SherlockFragmentActivity {
    public static final String DEBUG_TAG = "ShibbolethActivity";

    public static final String SHIBBOLETH_SERVER_URL = "shibboleth server url";
    public static final String SHIBBOLETH_HTTP_PREFIX = "http://";
    public static final String SHIBBOLETH_HTTPS_PREFIX = "https://";

    private Button mNextBtn;
    private CheckBox mHttpPrefixCb;
    private EditText mServerUrlEt;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        setContentView(R.layout.shibboleth_welcome_layout);
        mNextBtn = (Button) findViewById(R.id.shibboleth_next_btn);
        mServerUrlEt = (EditText) findViewById(R.id.shibboleth_server_url_et);
        mHttpPrefixCb = (CheckBox) findViewById(R.id.shibboleth_http_cb);

        mServerUrlEt.setText(SHIBBOLETH_HTTP_PREFIX);
        int prefixLen = SHIBBOLETH_HTTP_PREFIX.length();
        mServerUrlEt.setSelection(prefixLen, prefixLen);
        setupServerText();

        mNextBtn.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                String url = getServerUrl();
                if (isServerUrlValid(url))
                    openAuthorizePage(url);
            }
        });

        getActionBar().setDisplayHomeAsUpEnabled(true);
        getActionBar().setTitle(R.string.shib_actionbar_title);
    }

    public void onHttpsCheckboxClicked(View view) {

        boolean isHttps = mHttpPrefixCb.isChecked();
        String url = mServerUrlEt.getText().toString();
        String prefix = isHttps ? SHIBBOLETH_HTTPS_PREFIX : SHIBBOLETH_HTTP_PREFIX;

        String urlWithoutScheme = url.replace(SHIBBOLETH_HTTPS_PREFIX, "").replace(SHIBBOLETH_HTTP_PREFIX, "");

        int oldOffset = mServerUrlEt.getSelectionStart();

        // Change the text
        mServerUrlEt.setText(prefix + urlWithoutScheme);

        if (mServerUrlEt.hasFocus()) {
            // Change the cursor position since we changed the text
            if (isHttps) {
                int offset = oldOffset + 1;
                mServerUrlEt.setSelection(offset, offset);
            } else {
                int offset = Math.max(0, oldOffset - 1);
                mServerUrlEt.setSelection(offset, offset);
            }
        }
    }

    private void setupServerText() {
        mServerUrlEt.setOnFocusChangeListener(new View.OnFocusChangeListener() {
            @Override
            public void onFocusChange(View v, boolean hasFocus) {
                LogUtils.d(DEBUG_TAG, "serverText has focus: " + (hasFocus ? "yes" : "no"));
            }
        });

        mServerUrlEt.addTextChangedListener(new TextWatcher() {
            private String old;

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
            }

            @Override
            public void beforeTextChanged(CharSequence s, int start, int count,
                                          int after) {
                old = mServerUrlEt.getText().toString();
            }

            @Override
            public void afterTextChanged(Editable s) {
                // Don't allow the user to edit the "https://" or "http://" part of the serverText
                String url = mServerUrlEt.getText().toString();
                boolean isHttps = mHttpPrefixCb.isChecked();
                String prefix = isHttps ? SHIBBOLETH_HTTPS_PREFIX : SHIBBOLETH_HTTP_PREFIX;
                if (!url.startsWith(prefix)) {
                    int oldOffset = Math.max(prefix.length(), mServerUrlEt.getSelectionStart());
                    mServerUrlEt.setText(old);
                    mServerUrlEt.setSelection(oldOffset, oldOffset);
                }
            }
        });
    }

    @Override
    public boolean onMenuItemSelected(int featureId, MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                finish();
                break;
        }
        return true;
    }

    private boolean isServerUrlValid(String serverUrl) {
        if (serverUrl == null || serverUrl.isEmpty()) {
            Toast.makeText(this, getString(R.string.shib_server_url_empty), Toast.LENGTH_SHORT).show();
            return false;
        }

        if (!serverUrl.startsWith(SHIBBOLETH_HTTP_PREFIX) && !serverUrl.startsWith(SHIBBOLETH_HTTPS_PREFIX)) {
            Toast.makeText(this, getString(R.string.shib_server_incorrect_prefix), Toast.LENGTH_SHORT).show();
            return false;
        }

        return true;
    }

    private String getServerUrl() {
        String serverUrl = mServerUrlEt.getText().toString().trim();
        return serverUrl;
    }

    private void openAuthorizePage(String serverUrl) {
        Intent intent = new Intent(ShibbolethActivity.this, ShibbolethAuthorizeActivity.class);
        intent.putExtra(SHIBBOLETH_SERVER_URL, serverUrl);
        startActivity(intent);
        finish();
    }
}
