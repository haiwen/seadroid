package com.seafile.seadroid2;

import java.io.File;
import java.net.URISyntaxException;
import java.util.ArrayList;

import android.app.AlertDialog;
import android.app.Dialog;
import android.content.ActivityNotFoundException;
import android.content.BroadcastReceiver;
import android.content.ComponentName;
import android.content.Context;
import android.content.DialogInterface;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.ServiceConnection;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.app.DialogFragment;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.content.LocalBroadcastManager;
import android.util.Log;
import android.webkit.MimeTypeMap;
import android.widget.Toast;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.ActionBar.Tab;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;
import com.ipaulpro.afilechooser.FileChooserActivity;
import com.ipaulpro.afilechooser.utils.FileUtils;
import com.seafile.seadroid2.TransferManager.UploadTaskInfo;
import com.seafile.seadroid2.TransferService.TransferBinder;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.ui.PasswordDialog;
import com.seafile.seadroid2.ui.PasswordDialog.PasswordGetListener;
import com.seafile.seadroid2.ui.ReposFragment;
import com.seafile.seadroid2.ui.UploadTasksFragment;

import com.seafile.seadroid2.gallery.MultipleImageSelectionActivity;


public class BrowserActivity extends SherlockFragmentActivity
        implements ReposFragment.OnFileSelectedListener, OnBackStackChangedListener {

    private static final String DEBUG_TAG = "BrowserActivity";

    private Account account;
    NavContext navContext = null;
    DataManager dataManager = null;
    TransferService txService = null;

    // private boolean twoPaneMode = false;
    ReposFragment reposFragment = null;
    UploadTasksFragment uploadTasksFragment = null;

    private String currentTab;
    private static final String LIBRARY_TAB = "libraries";
    private static final String UPLOAD_TASKS_TAB = "upload-tasks";

    public DataManager getDataManager() {
        return dataManager;
    }

    private class PendingUploadInfo {
        String repoID;
        String repoName;
        String targetDir;
        String localFilePath;
        boolean isUpdate;

        public PendingUploadInfo(String repoID, String repoName,
                                 String targetDir, String localFilePath,
                                 boolean isUpdate) {
            this.repoID = repoID;
            this.repoName = repoName;
            this.targetDir = targetDir;
            this.localFilePath = localFilePath;
            this.isUpdate = isUpdate;
        }
    }

    public void addUpdateTask(String repoID, String repoName, String targetDir, String localFilePath) {
        if (txService != null) {
            txService.addUploadTask(account, repoID, repoName, targetDir, localFilePath, true);
        } else {
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, true);
            pendingUploads.add(info);
        }
    }

    private void addUploadTask(String repoID, String repoName, String targetDir, String localFilePath) {
        if (txService != null) {
            txService.addUploadTask(account, repoID, repoName, targetDir, localFilePath, false);
        } else {
            PendingUploadInfo info = new PendingUploadInfo(repoID, repoName, targetDir, localFilePath, false);
            pendingUploads.add(info);
        }
    }

    private ArrayList<PendingUploadInfo> pendingUploads = new ArrayList<PendingUploadInfo>();

    public TransferService getTransferService() {
        return txService;
    }

    public Account getAccount() {
        return account;
    }

    public NavContext getNavContext() {
        return navContext;
    }

    public class TabListener implements ActionBar.TabListener {

        private final String mTag;

        /** Constructor used each time a new tab is created.
          * @param activity  The host Activity, used to instantiate the fragment
          * @param tag  The identifier tag for the fragment
          * @param clz  The fragment's Class, used to instantiate the fragment
          */
        public TabListener(String tag) {
            mTag = tag;
        }

        @Override
        public void onTabSelected(Tab tab, FragmentTransaction ft) {
            currentTab = mTag;
            if (mTag.equals(LIBRARY_TAB)) {
                showReposFragment(ft);
            } else if (mTag.equals(UPLOAD_TASKS_TAB)) {
                disableUpButton();
                showUploadTasksFragment(ft);
            }
        }

        @Override
        public void onTabUnselected(Tab tab, FragmentTransaction ft) {
            if (mTag.equals(LIBRARY_TAB)) {
                hideReposFragment(ft);
            } else if (mTag.equals(UPLOAD_TASKS_TAB)) {
                hideUploadTasksFragment(ft);
            }
        }

        @Override
        public void onTabReselected(Tab tab, FragmentTransaction ft) {
        }
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        super.onCreate(savedInstanceState);

        // Get the message from the intent
        Intent intent = getIntent();
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        String token = intent.getStringExtra("token");
        account = new Account(server, email, null, token);
        Log.d(DEBUG_TAG, "browser activity onCreate " + server + " " + email);

        if (server == null) {
            Intent newIntent = new Intent(this, AccountsActivity.class);
            startActivity(newIntent);
            finish();
            return;
        }

        dataManager = new DataManager(account);
        navContext = new NavContext();

        //setContentView(R.layout.seadroid_main);
        //setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_PORTRAIT);
        getSupportFragmentManager().addOnBackStackChangedListener(this);

        ActionBar actionBar = getSupportActionBar();
        actionBar.setNavigationMode(ActionBar.NAVIGATION_MODE_TABS);
        actionBar.setDisplayShowTitleEnabled(false);
        unsetRefreshing();

        int cTab = 0;

        if (savedInstanceState != null) {
            // fragment are saved during screen rotation, so do not need to create a new one
            reposFragment = (ReposFragment)
                    getSupportFragmentManager().findFragmentByTag("repos_fragment");
            uploadTasksFragment = (UploadTasksFragment)
                    getSupportFragmentManager().findFragmentByTag("upload_tasks_fragment");
            cTab = savedInstanceState.getInt("tab");

            String repoID = savedInstanceState.getString("repoID");
            String repoName = savedInstanceState.getString("repoName");
            String path = savedInstanceState.getString("path");
            String dirID = savedInstanceState.getString("dirID");
            if (repoID != null) {
                navContext.setRepoID(repoID);
                navContext.setRepoName(repoName);
                navContext.setDir(path, dirID);
            }
        }

        Tab tab = actionBar.newTab()
                .setText(R.string.libraries)
                .setTabListener(new TabListener(LIBRARY_TAB));
        actionBar.addTab(tab);

        tab = actionBar.newTab()
            .setText(R.string.upload_tasks)
            .setTabListener(new TabListener(UPLOAD_TASKS_TAB));
        actionBar.addTab(tab);

        actionBar.setSelectedNavigationItem(cTab);
        if (cTab == 0) {
            currentTab = LIBRARY_TAB;
        } else {
            currentTab = UPLOAD_TASKS_TAB;
        }

        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        IntentFilter filter = new IntentFilter(TransferService.BROADCAST_ACTION);
        TransferReceiver receiver = new TransferReceiver();
        LocalBroadcastManager.getInstance(this).registerReceiver(receiver, filter);

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");
    }

    ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferBinder binder = (TransferBinder) service;
            txService = binder.getService();
            Log.d(DEBUG_TAG, "bind TransferService");

            for (PendingUploadInfo info : pendingUploads) {
                txService.addUploadTask(account, info.repoID,
                                        info.repoName, info.targetDir,
                                        info.localFilePath, info.isUpdate);
            }
            pendingUploads.clear();

            if (currentTab.equals(UPLOAD_TASKS_TAB)
                && uploadTasksFragment != null && uploadTasksFragment.isReady()) {
                uploadTasksFragment.refreshView();
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };

    @Override
    public void onStart() {
        super.onStart();
    }

    @Override
    protected void onNewIntent(Intent intent) {
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        String token = intent.getStringExtra("token");

        String repoID = intent.getStringExtra("repoID");
        String path = intent.getStringExtra("path");
        String objectID = intent.getStringExtra("objectID");
        long size = intent.getLongExtra("size", 0);
        Log.d(DEBUG_TAG, "browser activity onNewIntent " + server + " " + email);
        Log.d(DEBUG_TAG, "repoID " + repoID + ":" + path + ":" + objectID);


        if (getSupportActionBar().getSelectedNavigationIndex() != 0) {
            //
        } else {
            //
        }
    }

    @Override
    protected void onStop() {
        super.onStop();
    }

    @Override
    protected void onDestroy() {
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }
        super.onDestroy();
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putInt("tab", getSupportActionBar().getSelectedNavigationIndex());
        if (navContext.getRepoID() != null) {
            outState.putString("repoID", navContext.getRepoID());
            outState.putString("repoName", navContext.getRepoName());
            outState.putString("path", navContext.getDirPath());
            outState.putString("dirID", navContext.getDirID());
        }
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        MenuInflater inflater = getSupportMenuInflater();
        inflater.inflate(R.menu.browser_menu, menu);
        return true;
    }

    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        MenuItem menuUpload = menu.findItem(R.id.upload);
        MenuItem menuRefresh = menu.findItem(R.id.refresh);

        if (currentTab.equals(LIBRARY_TAB)) {
            menuUpload.setVisible(true);
            if (navContext.inRepo())
                menuUpload.setEnabled(true);
            else
                menuUpload.setEnabled(false);
        } else {
            menuUpload.setVisible(false);
        }

        if (currentTab.equals(LIBRARY_TAB))
            menuRefresh.setVisible(true);
        else
            menuRefresh.setVisible(false);

        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
        case android.R.id.home:
            if (navContext.isRepoRoot()) {
                navContext.setRepoID(null);
            } else {
                String parentPath = Utils
                        .getParentPath(navContext.getDirPath());
                navContext.setDir(parentPath, null);
            }
            reposFragment.refreshView();

            return true;
        case R.id.upload:
            pickFile();
            return true;
        case R.id.refresh:
            if (!Utils.isNetworkOn()) {
                showToast(getString(R.string.network_down));
                return true;
            }
            if (navContext.repoID != null)
                dataManager.invalidateCache(navContext.repoID, navContext.dirPath);
            reposFragment.refreshView();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void showReposFragment(FragmentTransaction ft) {
        //Log.d(DEBUG_TAG, "showReposFragment");

        if (reposFragment == null) {
            reposFragment = new ReposFragment();
            ft.add(android.R.id.content, reposFragment, "repos_fragment");
        } else {
            //Log.d(DEBUG_TAG, "Attach reposFragment");
            ft.attach(reposFragment);
        }
    }

    private void hideReposFragment(FragmentTransaction ft) {
        //Log.d(DEBUG_TAG, "hideReposFragment");
        ft.detach(reposFragment);
    }


    private void showUploadTasksFragment(FragmentTransaction ft) {
        if (uploadTasksFragment == null) {
            uploadTasksFragment = new UploadTasksFragment();
            ft.add(android.R.id.content, uploadTasksFragment, "upload_tasks_fragment");
        } else {
            ft.attach(uploadTasksFragment);
        }
    }

    private void hideUploadTasksFragment(FragmentTransaction ft) {
        //Log.d(DEBUG_TAG, "hideUploadTasksFragment");
        ft.detach(uploadTasksFragment);
    }

    public void setRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.TRUE);
    }

    public void unsetRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.FALSE);
    }

    public void showToast(CharSequence msg) {
        Context context = getApplicationContext();
        Toast toast = Toast.makeText(context, msg, Toast.LENGTH_SHORT);
        toast.show();
    }

    public void enableUpButton() {
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
    }

    public void disableUpButton() {
        getSupportActionBar().setDisplayHomeAsUpEnabled(false);
    }

    /***********  Start other activity  ***************/

    public static final int PICK_FILE_REQUEST = 1;
    public static final int PICK_PHOTOS_VIDEOS_REQUEST = 2;

    public class UploadChoiceDialog extends DialogFragment {
        @Override
        public Dialog onCreateDialog(Bundle savedInstanceState) {

            AlertDialog.Builder builder = new AlertDialog.Builder(getActivity());
            builder.setTitle(R.string.pick_upload_type);
            builder.setItems(R.array.pick_upload_array,
                    new DialogInterface.OnClickListener() {
                        @Override
                        public void onClick(DialogInterface dialog, int which) {
                            switch (which) {
                            case 0:
                                Intent intent = new Intent(BrowserActivity.this, FileChooserActivity.class);
                                getActivity().startActivityForResult(intent, PICK_FILE_REQUEST);
                                break;
                            case 1:
                                // photos
                                intent = new Intent(BrowserActivity.this, MultipleImageSelectionActivity.class);
                                getActivity().startActivityForResult(intent, PICK_PHOTOS_VIDEOS_REQUEST);
                                break;
                            case 2:
                                // thirdparty file chooser
                                Intent target = FileUtils.createGetContentIntent();
                                intent = Intent.createChooser(target, getString(R.string.choose_file));
                                getActivity().startActivityForResult(intent, PICK_FILE_REQUEST);
                                break;
                            default:
                                return;
                            }
                        }
                    });

            return builder.create();
        }
    }

    void pickFile() {
        UploadChoiceDialog dialog = new UploadChoiceDialog();
        dialog.show(getSupportFragmentManager(), "pickFile");
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == PICK_FILE_REQUEST) {
            if (resultCode == RESULT_OK) {
                if (!Utils.isNetworkOn()) {
                    showToast("Network is not connected");
                    return;
                }

                Uri uri = data.getData();
                String path;
                try {
                    path = FileUtils.getPath(this, uri);
                } catch (URISyntaxException e) {
                    e.printStackTrace();
                    return;
                }
                showToast(getString(R.string.upload) + " " + Utils.fileNameFromPath(path));
                addUploadTask(navContext.getRepoID(),
                    navContext.getRepoName(), navContext.getDirPath(), path);
            }
        }

        if (requestCode == PICK_PHOTOS_VIDEOS_REQUEST) {
            if (resultCode == RESULT_OK) {
                ArrayList<String> paths = data.getStringArrayListExtra("photos");
                if (paths == null)
                    return;
                for (String path : paths) {
                    addUploadTask(navContext.getRepoID(),
                        navContext.getRepoName(), navContext.getDirPath(), path);
                }
            }
        }

    }

    /***************  Navigation *************/

    /**
     * When a file is clicked on ReposFragment, only show file if:
     *
     * - There is cache
     * - Cached file id is the same as current file id
     *
     * Otherwise, download the file
     */
    public void onFileSelected(String repoName, String repoID, String path, SeafDirent dirent) {
        SeafCachedFile cf = dataManager.getCachedFile(repoName, repoID, path);
        // If local file is up to date, show it
        if (cf != null && dirent.id.equals(cf.fileID)) {
            File file = dataManager.getLocalRepoFile(repoName, repoID, path);
            if (file.exists()) {
                showFile(repoName, repoID, path);
                return;
            }
        }

        // Add a download task
        if (txService.addDownloadTask(account, repoName, repoID, path, dirent.id, dirent.size)) {
            showToast("Downloading " + Utils.fileNameFromPath(path));
        } else {
            showToast("You are already downloading " + Utils.fileNameFromPath(path));
        }
    }


    @Override
    public void onBackPressed() {
        if (getSupportFragmentManager().getBackStackEntryCount() != 0) {
            getSupportFragmentManager().popBackStack();
            return;
        }

        if (currentTab.equals("libraries")) {
            if (navContext.inRepo()) {
                if (navContext.isRepoRoot()) {
                    navContext.setRepoID(null);
                } else {
                    String parentPath = Utils.getParentPath(navContext
                            .getDirPath());
                    navContext.setDir(parentPath, null);
                }
                reposFragment.refreshView();
            } else
                super.onBackPressed();
        } else if (currentTab.equals("cache")) {
            super.onBackPressed();
        } else
            super.onBackPressed();
    }

    @Override
    public void onBackStackChanged() {
    }


    /************  Files ************/

    private void startMarkdownActivity(String path) {
        Intent intent = new Intent(this, MarkdownActivity.class);
        intent.putExtra("path", path);
        startActivity(intent);
    }

    private boolean showFile(String repoName, String repoID, String path) {
        File file = dataManager.getLocalRepoFile(repoName, repoID, path);
        String name = file.getName();
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();

        if (suffix.length() == 0) {
            showToast(getString(R.string.unknown_file_type));
            return false;
        }

        if (suffix.endsWith("md") || suffix.endsWith("markdown")) {
            startMarkdownActivity(file.getPath());
            return true;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        Intent open = new Intent(Intent.ACTION_VIEW);
        open.setDataAndType((Uri.fromFile(file)), mime);
        try {
            startActivity(open);
            return true;
        } catch (ActivityNotFoundException e) {
            showToast(getString(R.string.activity_not_found));
            return false;
        }
    }

    public void onFileUploadProgress(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        if (uploadTasksFragment != null && uploadTasksFragment.isReady())
            uploadTasksFragment.onTaskProgressUpdate(info);
    }

    public void onFileUploaded(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);

        String repoID = info.repoID;
        String dir = info.parentDir;
        dataManager.invalidateCache(repoID, dir);
        if (currentTab.equals(LIBRARY_TAB)
                && repoID.equals(navContext.getRepoID())
                && dir.equals(navContext.getDirPath())) {
            reposFragment.refreshView();
            String verb = getString(info.isUpdate ? R.string.updated : R.string.uploaded);
            showToast(verb + " " + Utils.fileNameFromPath(info.localFilePath));
        }

        if (uploadTasksFragment != null && uploadTasksFragment.isReady())
            uploadTasksFragment.onTaskFinished(info);

        if (info.isUpdate) {
            File f = new File(info.localFilePath);
            String path = Utils.pathJoin(info.parentDir, f.getName());
            dataManager.addCachedFile(info.repoName, info.repoID, path,
                                      info.newFileID, f);
        }
    }

    public void onFileUploadCancelled(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        if (uploadTasksFragment != null && uploadTasksFragment.isReady())
            uploadTasksFragment.onTaskCancelled(info);
    }

    public void onFileUploadFailed(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        showToast(getString(R.string.upload_failed) + " " + Utils.fileNameFromPath(info.localFilePath));
        if (uploadTasksFragment != null && uploadTasksFragment.isReady())
            uploadTasksFragment.onTaskFailed(info);
    }

    public void onFileDownloaded(String repoID, String path, String fileID) {
        if (currentTab.equals(LIBRARY_TAB)
                && repoID.equals(navContext.getRepoID())
                && Utils.getParentPath(path).equals(navContext.getDirPath())) {
            reposFragment.getAdapter().notifyChanged();
            //showFile(repoID, path, fileID);
        }
    }

    public void onFileDownloadFailed(final String repoName, final String repoID, final String path,
            final String fileID, final long size, SeafException err) {
        if (err != null && err.getCode() == 440) {
            if (currentTab.equals(LIBRARY_TAB)
                    && repoID.equals(navContext.getRepoID())
                    && Utils.getParentPath(path).equals(navContext.getDirPath())) {
                PasswordDialog dialog = new PasswordDialog();
                dialog.setPasswordGetListener(new PasswordGetListener() {
                    @Override
                    public void onPasswordGet(String password) {
                        if (password.length() == 0)
                            return;
                        ConcurrentAsyncTask.execute(
                            new SetPasswordTask(dataManager, repoName, repoID, path, fileID, size), password);
                    }

                });
                dialog.show(getSupportFragmentManager(), "DialogFragment");
                return;
            }
        }
        showToast(getString(R.string.download_failed) + " " + Utils.fileNameFromPath(path));
    }

    private class SetPasswordTask extends AsyncTask<String, Void, Void> {

        String myRepoName;
        String myRepoID;
        String myPath;
        String myFileID;
        long size;
        DataManager dataManager;

        public SetPasswordTask(DataManager dataManager, String repoName, String repoID, String path, String fileID, long size) {
            this.dataManager = dataManager;
            this.myRepoName = repoName;
            this.myRepoID = repoID;
            this.myPath = path;
            this.myFileID = fileID;
            this.size = size;
        }

        @Override
        protected Void doInBackground(String... params) {
            if (params.length != 1) {
                Log.d(DEBUG_TAG, "Wrong params to SetPasswordTask");
                return null;
            }

            String password = params[0];
            dataManager.setPassword(myRepoID, password);
            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            txService.addDownloadTask(account, myRepoName, myRepoID, myPath, myFileID, size);
        }

    }

    // for receive broadcast from TransferService
    private class TransferReceiver extends BroadcastReceiver {

        private TransferReceiver() {}

        public void onReceive(Context context, Intent intent) {
            String type = intent.getStringExtra("type");
            if (type.equals("downloaded")) {
                String repoID = intent.getStringExtra("repoID");
                String path = intent.getStringExtra("path");
                String fileID = intent.getStringExtra("fileID");
                onFileDownloaded(repoID, path, fileID);
            } else if (type.equals("downloadFailed")) {
                String repoName = intent.getStringExtra("repoName");
                String repoID = intent.getStringExtra("repoID");
                String path = intent.getStringExtra("path");
                String fileID = intent.getStringExtra("fileID");
                long size = intent.getLongExtra("size", 0);
                int errCode = intent.getIntExtra("errCode", 0);
                String errMsg = intent.getStringExtra("errMsg");
                onFileDownloadFailed(repoName, repoID, path, fileID,
                        size, new SeafException(errCode, errMsg));
            } else if (type.equals("uploaded")) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploaded(taskID);
            } else if (type.equals("uploadFailed")) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadFailed(taskID);
            } else if (type.equals("uploadProgress")) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadProgress(taskID);
            }
        }

    } // TransferReceiver


}
