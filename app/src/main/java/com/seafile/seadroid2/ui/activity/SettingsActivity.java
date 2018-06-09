package com.seafile.seadroid2.ui.activity;

import android.Manifest;
import android.content.pm.PackageManager;
import android.os.Bundle;
import android.support.design.widget.Snackbar;
import android.support.v4.app.ActivityCompat;
import android.support.v4.app.FragmentManager;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.content.ContextCompat;
import android.support.v7.widget.Toolbar;
import android.view.MenuItem;
import android.view.View;
import android.view.Window;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.fragment.SettingsFragment;
import com.seafile.seadroid2.util.Utils;

import java.util.List;

public class SettingsActivity extends BaseActivity implements Toolbar.OnMenuItemClickListener {
    private static final String DEBUG_TAG = "SettingsActivity";
    private View mLayout;
    public TransferService txService;
    private SettingsFragment mSettingsFragment;
    public static final int REQUEST_PERMISSIONS_READ_CONTACTS = 2;
    public static String BASE_DIR = "Contacts Backup";

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

//        Intent bIntent = new Intent(this, TransferService.class);
//        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
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


//    public void uploadContacts(String path) {
//        Account camAccount = mSettingsFragment.contactsManager.getContactsAccount();
//        if (camAccount != null && mSettingsFragment.settingsMgr.getContactsUploadRepoName() != null) {
//            String repoName = mSettingsFragment.settingsMgr.getContactsUploadRepoName();
//            String repoId = mSettingsFragment.settingsMgr.getContactsUploadRepoId();
//
//            DataManager dataManager = new DataManager(camAccount);
//            try {
//                forceCreateDirectory(dataManager, "/", BASE_DIR, repoId);
//                String serverPath = Utils.pathJoin(BASE_DIR, "/");
//                txService.addTaskToUploadQue(camAccount, repoId, repoName, serverPath, path, false, true);
//            } catch (SeafException e) {
//                showShortToast(this, e.getMessage());
//                showShortToast(this, getString(R.string.contacts_backup_fail));
//                e.printStackTrace();
//            }
//        }
//    }

    /**
     * Create a directory, rename a file away if necessary,
     *
     * @param dataManager
     * @param parent      parent dir
     * @param dir         directory to create
     * @throws SeafException
     */
    private void forceCreateDirectory(DataManager dataManager, String parent, String dir, String targetRepoId) throws SeafException {

        List<SeafDirent> dirs = dataManager.getDirentsFromServer(targetRepoId, parent);
        boolean found = false;
        for (SeafDirent dirent : dirs) {
            if (dirent.name.equals(dir) && dirent.isDir()) {
                found = true;
            } else if (dirent.name.equals(dir) && !dirent.isDir()) {
                // there is already a file. move it away.
                String newFilename = getString(R.string.camera_sync_rename_file, dirent.name);
                dataManager.rename(targetRepoId,
                        Utils.pathJoin(Utils.pathJoin("/", parent), dirent.name),
                        newFilename,
                        false);
            }
        }
        if (!found)
            dataManager.createNewDir(targetRepoId, Utils.pathJoin("/", parent), dir);
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
                // READ_CONTACTS permission has not been granted yet. Request it directly.
                ActivityCompat.requestPermissions(this, new String[]{Manifest.permission.READ_CONTACTS},
                        REQUEST_PERMISSIONS_READ_CONTACTS);
            }
        }
    }


//    ServiceConnection mConnection = new ServiceConnection() {
//        @Override
//        public void onServiceConnected(ComponentName className, IBinder service) {
//            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
//            txService = binder.getService();
//            Log.d(DEBUG_TAG, "bind TransferService");
//
//        }
//
//        @Override
//        public void onServiceDisconnected(ComponentName arg0) {
//            txService = null;
//        }
//    };
//
//    @Override
//    protected void onDestroy() {
//        Log.d(DEBUG_TAG, "onDestroy is called");
//        if (txService != null) {
//            unbindService(mConnection);
//            txService = null;
//        }
//        super.onDestroy();
//    }


}