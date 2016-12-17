package com.seafile.seadroid2.ui.activity;

import android.Manifest;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.os.IBinder;
import android.support.design.widget.Snackbar;
import android.support.v4.app.ActivityCompat;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.content.ContextCompat;
import android.support.v7.widget.Toolbar;
import android.util.Log;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.fragment.SettingsFragment;

import static com.seafile.seadroid2.ui.activity.BrowserActivity.REQUEST_PERMISSIONS_READ_CONTACTS;

public class SettingsActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "SettingsActivity";
    private View mLayout;
    public TransferService txService;
    private SettingsFragment mSettingsFragment;

    public void onCreate(Bundle savedInstanceState) {
        supportRequestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        super.onCreate(savedInstanceState);

        setContentView(R.layout.settings_activity_layout);

        FragmentManager fragmentManager = getSupportFragmentManager();
        FragmentTransaction fragmentTransaction = fragmentManager.beginTransaction();
        mSettingsFragment = new SettingsFragment();
        fragmentTransaction.add(R.id.settings_fragment_container, mSettingsFragment);
        fragmentTransaction.commit();

        Toolbar toolbar = getActionBarToolbar();
        setSupportActionBar(toolbar);
        toolbar.setOnMenuItemClickListener(this);
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setTitle(R.string.settings);
        mLayout = findViewById(R.id.settings_fragment_container);

        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    public boolean onMenuItemClick(MenuItem item) {
        return super.onOptionsItemSelected(item);
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                this.finish();
            default:
                return super.onOptionsItemSelected(item);
        }
    }


    public void uploadContacts(String path) {
        Account camAccount = mSettingsFragment.contactsManager.getContactsAccount();
        if (camAccount != null && mSettingsFragment.settingsMgr.getCameraUploadRepoName() != null) {
            String repoName = mSettingsFragment.settingsMgr.getContactsUploadRepoName();
            String repoId = mSettingsFragment.settingsMgr.getContactsUploadRepoId();
            txService.addTaskToUploadQue(camAccount, repoId, repoName, "/", path, false, true);
        }
    }

    /**
     * If the user is running Android 6.0 (API level 23) or later, the user has to grant your app its permissions while they are running
     * the app
     * <p>
     * Requests the READ_CONTACTS permission.
     */
    public void requestReadContactsPermission() {
        if (ContextCompat.checkSelfPermission(this, Manifest.permission.READ_CONTACTS) != PackageManager.PERMISSION_GRANTED) {
            // Should we show an explanation?
            if (ActivityCompat.shouldShowRequestPermissionRationale(this, Manifest.permission.READ_CONTACTS)) {

                Snackbar.make(mLayout, R.string.permission_read_exteral_storage_rationale, Snackbar.LENGTH_INDEFINITE)
                        .setAction(R.string.settings, new View.OnClickListener() {
                            @Override
                            public void onClick(View view) {
                                ActivityCompat.requestPermissions(SettingsActivity.this, new String[]{Manifest.permission.READ_CONTACTS},
                                        REQUEST_PERMISSIONS_READ_CONTACTS);
                            }
                        })
                        .show();
            } else {
                // No explanation needed, we can request the permission.
                // WRITE_EXTERNAL_STORAGE permission has not been granted yet. Request it directly.
                ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.READ_CONTACTS},
                        REQUEST_PERMISSIONS_READ_CONTACTS);
            }
        }
    }

    /**
     * Callback received when a permissions request has been completed.
     */
    @Override
    public void onRequestPermissionsResult(int requestCode, String permissions[], int[] grantResults) {
        Log.i(DEBUG_TAG, "Received response for permission request.");
        switch (requestCode) {
            case REQUEST_PERMISSIONS_READ_CONTACTS: {
                // Check if the only required permission has been granted
                // If request is cancelled, the result arrays are empty.
                if (grantResults.length > 0 && grantResults[0] == PackageManager.PERMISSION_GRANTED) {
                    // permission was granted
                    mSettingsFragment.showUploadContactsDialog();
                    Log.d(DEBUG_TAG, "permission was granted");
                } else {
                    // permission denied
                }
            }
        }
    }


    ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
            Log.d(DEBUG_TAG, "bind TransferService");

        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }
        super.onDestroy();
    }


}