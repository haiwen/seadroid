package com.seafile.seadroid2;

import java.io.File;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

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
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.net.Uri;
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
import com.seafile.seadroid2.TransferManager.DownloadTaskInfo;
import com.seafile.seadroid2.TransferManager.UploadTaskInfo;
import com.seafile.seadroid2.TransferService.TransferBinder;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.gallery.MultipleImageSelectionActivity;
import com.seafile.seadroid2.ui.ActivitiesFragment;
import com.seafile.seadroid2.ui.AppChoiceDialog;
import com.seafile.seadroid2.ui.FetchFileDialog;
import com.seafile.seadroid2.ui.NewDirDialog;
import com.seafile.seadroid2.ui.NewFileDialog;
import com.seafile.seadroid2.ui.PasswordDialog;
import com.seafile.seadroid2.ui.ReposFragment;
import com.seafile.seadroid2.ui.TaskDialog;
import com.seafile.seadroid2.ui.UploadTasksFragment;


public class BrowserActivity extends SherlockFragmentActivity
        implements ReposFragment.OnFileSelectedListener, OnBackStackChangedListener {

    private static final String DEBUG_TAG = "BrowserActivity";


    public static final String PKG_NAME = "com.seafile.seadroid2";
    public static final String EXTRA_REPO_NAME = PKG_NAME + ".repoName";
    public static final String EXTRA_REPO_ID = PKG_NAME + ".repoID";
    public static final String EXTRA_FILE_PATH = PKG_NAME + ".filePath";
    public static final String EXTRA_ACCOUT = PKG_NAME + ".filePath";


    private Account account;
    NavContext navContext = null;
    DataManager dataManager = null;
    TransferService txService = null;
    TransferReceiver mTransferReceiver;

    // private boolean twoPaneMode = false;
    ReposFragment reposFragment = null;
    UploadTasksFragment uploadTasksFragment = null;
    ActivitiesFragment activitiesFragment = null;

    FetchFileDialog fetchFileDialog = null;

    private String currentTab;
    private static final String LIBRARY_TAB = "libraries";
    private static final String UPLOAD_TASKS_TAB = "upload-tasks";
    private static final String ACTIVITY_TAB = "activities";

    public static final String REPOS_FRAGMENT_TAG = "repos_fragment";
    public static final String UPLOAD_TASKS_FRAGMENT_TAG = "upload_tasks_fragment";
    public static final String ACTIVITIES_FRAGMENT_TAG = "activities_fragment";
    public static final String OPEN_FILE_DIALOG_FRAGMENT_TAG = "openfile_fragment";
    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "password_fragment";
    public static final String CHOOSE_APP_DIALOG_FRAGMENT_TAG = "choose_app_fragment";
    public static final String PICK_FILE_DIALOG_FRAGMENT_TAG = "pick_file_fragment";

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
            disableActionBarTitle();
            Log.d(DEBUG_TAG, mTag + " is selected");
            currentTab = mTag;
            if (mTag.equals(LIBRARY_TAB)) {
                showReposFragment(ft);
            } else if (mTag.equals(UPLOAD_TASKS_TAB)) {
                disableUpButton();
                showUploadTasksFragment(ft);
            } else if (mTag.equals(ACTIVITY_TAB)) {
                disableUpButton();
                showActivitiesFragment(ft);
            }
        }

        @Override
        public void onTabUnselected(Tab tab, FragmentTransaction ft) {
            Log.d(DEBUG_TAG, mTag + " is unselected");
            if (mTag.equals(LIBRARY_TAB)) {
                hideReposFragment(ft);
            } else if (mTag.equals(UPLOAD_TASKS_TAB)) {
                hideUploadTasksFragment(ft);
            } else if (mTag.equals(ACTIVITY_TAB)) {
                hideActivitiesFragment(ft);
            }
        }

        @Override
        public void onTabReselected(Tab tab, FragmentTransaction ft) {
        }
    }

    public void disableActionBarTitle() {
        getSupportActionBar().setDisplayShowTitleEnabled(false);
    }

    public void setActionBarTitle(String title, String subtitle) {
        // TODO: Before we find a way to make the actionbar title and tabs
        // always appear in different lines, we do not set the titles.

        // ActionBar actionBar = getSupportActionBar();
        // actionBar.setDisplayShowTitleEnabled(true);
        // actionBar.setTitle(title); actionBar.setSubtitle(subtitle);
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
            newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
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
                    getSupportFragmentManager().findFragmentByTag(REPOS_FRAGMENT_TAG);
            uploadTasksFragment = (UploadTasksFragment)
                    getSupportFragmentManager().findFragmentByTag(UPLOAD_TASKS_FRAGMENT_TAG);

            activitiesFragment = (ActivitiesFragment)
                    getSupportFragmentManager().findFragmentByTag(ACTIVITIES_FRAGMENT_TAG);

            fetchFileDialog = (FetchFileDialog)
                    getSupportFragmentManager().findFragmentByTag(OPEN_FILE_DIALOG_FRAGMENT_TAG);

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

        // tab = actionBar.newTab()
        //     .setText(R.string.activities)
        //     .setTabListener(new TabListener(ACTIVITY_TAB));
        // actionBar.addTab(tab);

        actionBar.setSelectedNavigationItem(cTab);
        if (cTab == 0) {
            currentTab = LIBRARY_TAB;
        } else if (cTab == 1) {
            currentTab = UPLOAD_TASKS_TAB;
        } else {
            currentTab = ACTIVITY_TAB;
        }

        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

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
        Log.d(DEBUG_TAG, "onStart");
        super.onStart();

        if (mTransferReceiver == null) {
            mTransferReceiver = new TransferReceiver();
        }

        IntentFilter filter = new IntentFilter(TransferService.BROADCAST_ACTION);
        LocalBroadcastManager.getInstance(this).registerReceiver(mTransferReceiver, filter);
    }

    @Override
    public void onRestart() {
        Log.d(DEBUG_TAG, "onRestart");
        super.onStart();
    }

    @Override
    protected void onNewIntent(Intent intent) {
        Log.d(DEBUG_TAG, "onNewIntent");
    }

    @Override
    protected void onStop() {
        Log.d(DEBUG_TAG, "onStop");
        super.onStop();

        if (mTransferReceiver != null) {
            LocalBroadcastManager.getInstance(this).unregisterReceiver(mTransferReceiver);
        }
    }

    @Override
    protected void onDestroy() {
        Log.d(DEBUG_TAG, "onDestroy is called");
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }


        super.onDestroy();
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        Log.d(DEBUG_TAG, "onSaveInstanceState");
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
        MenuItem menuNewDir = menu.findItem(R.id.newdir);
        MenuItem menuNewFile = menu.findItem(R.id.newfile);

        if (currentTab.equals(LIBRARY_TAB)) {
            menuUpload.setVisible(true);
            if (navContext.inRepo() && hasRepoWritePermission()) {
                menuUpload.setEnabled(true);
            }
            else
                menuUpload.setEnabled(false);
        } else {
            menuUpload.setVisible(false);
        }

        if (currentTab.equals(LIBRARY_TAB)) {
            menuRefresh.setVisible(true);
        } else if (currentTab.equals(ACTIVITY_TAB)) {
            menuRefresh.setVisible(true);
        } else {
            menuRefresh.setVisible(false);
        }

        if (currentTab.equals(LIBRARY_TAB)) {
            if (navContext.inRepo() && hasRepoWritePermission()) {
                menuNewDir.setVisible(true);
                menuNewFile.setVisible(true);
            } else {
                menuNewDir.setVisible(false);
                menuNewFile.setVisible(false);
            }
        } else {
            menuNewDir.setVisible(false);
            menuNewFile.setVisible(false);
        }

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
                showToast(R.string.network_down);
                return true;
            }

            if (currentTab.equals(LIBRARY_TAB)) {
                if (navContext.repoID != null)
                    dataManager.invalidateCache(navContext.repoID, navContext.dirPath);
                reposFragment.refreshView(true);
            } else if (currentTab.equals(ACTIVITY_TAB)) {
                activitiesFragment.refreshView();
            }

            return true;
        case R.id.newdir:
            showNewDirDialog();
            return true;
        case R.id.newfile:
            showNewFileDialog();
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    private void showNewDirDialog() {
        if (!hasRepoWritePermission()) {
            showToast(R.string.library_read_only);
            return;
        }

        final NewDirDialog dialog = new NewDirDialog();
        dialog.init(navContext.getRepoID(), navContext.getDirPath(), account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showToast("Sucessfully created folder " + dialog.getNewDirName());
                if (currentTab.equals(LIBRARY_TAB) && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    private void showNewFileDialog() {
        if (!hasRepoWritePermission()) {
            showToast(R.string.library_read_only);
            return;
        }

        final NewFileDialog dialog = new NewFileDialog();
        dialog.init(navContext.getRepoID(), navContext.getDirPath(), account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showToast("Sucessfully created file " + dialog.getNewFileName());
                if (currentTab.equals(LIBRARY_TAB) && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    private void showReposFragment(FragmentTransaction ft) {
        //Log.d(DEBUG_TAG, "showReposFragment");

        if (reposFragment == null) {
            reposFragment = new ReposFragment();
            ft.add(android.R.id.content, reposFragment, REPOS_FRAGMENT_TAG);
        } else {
            //Log.d(DEBUG_TAG, "Attach reposFragment");
            ft.attach(reposFragment);
        }
    }

    private void hideReposFragment(FragmentTransaction ft) {
        //Log.d(DEBUG_TAG, "hideReposFragment");
        ft.detach(reposFragment);
    }

    private void showActivitiesFragment(FragmentTransaction ft) {
        if (activitiesFragment == null) {
            activitiesFragment = new ActivitiesFragment();
            ft.add(android.R.id.content, activitiesFragment, ACTIVITIES_FRAGMENT_TAG);
        } else {
            //Log.d(DEBUG_TAG, "Attach reposFragment");
            ft.attach(activitiesFragment);
        }
    }

    private void hideActivitiesFragment(FragmentTransaction ft) {
        ft.detach(activitiesFragment);
    }

    private void showUploadTasksFragment(FragmentTransaction ft) {
        if (uploadTasksFragment == null) {
            uploadTasksFragment = new UploadTasksFragment();
            ft.add(android.R.id.content, uploadTasksFragment, UPLOAD_TASKS_FRAGMENT_TAG);
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

    public void showToast(int id) {
        showToast(getString(id));
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

    public boolean hasRepoWritePermission() {
        SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
        if (repo == null) {
            return false;
        }

        if (repo.permission.indexOf('w') == -1) {
            return false;
        }
        return true;
    }

    void pickFile() {
        if (!hasRepoWritePermission()) {
            showToast(R.string.library_read_only);
            return;
        }

        UploadChoiceDialog dialog = new UploadChoiceDialog();
        dialog.show(getSupportFragmentManager(), PICK_FILE_DIALOG_FRAGMENT_TAG);
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

    @Override
    public void onFileSelected(SeafDirent dirent) {
        String fileName= dirent.name;
        final String repoName = navContext.getRepoName();
        final String repoID = navContext.getRepoID();
        final String filePath = Utils.pathJoin(navContext.getDirPath(), fileName);

        File localFile = dataManager.getLocalCachedFile(repoName, repoID, filePath, dirent.id);
        if (localFile != null) {
            showFile(localFile);
            return;
        }

        Intent intent = new Intent(this, FileActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", account);
        startActivity(intent);
        return;
    }


    @Override
    public void onBackPressed() {
        if (getSupportFragmentManager().getBackStackEntryCount() != 0) {
            getSupportFragmentManager().popBackStack();
            return;
        }

        if (currentTab.equals(LIBRARY_TAB)) {
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
        } else {
            super.onBackPressed();
        }
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

    public void showFile(File file) {
        String name = file.getName();
        String suffix = name.substring(name.lastIndexOf('.') + 1).toLowerCase();

        if (suffix.length() == 0) {
            showToast(R.string.unknown_file_type);
            return;
        }

        if (suffix.endsWith("md") || suffix.endsWith("markdown")) {
            startMarkdownActivity(file.getPath());
            return;
        }

        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        Intent open = new Intent(Intent.ACTION_VIEW);
        open.setDataAndType((Uri.fromFile(file)), mime);
        try {
            startActivity(open);
            return;
        } catch (ActivityNotFoundException e) {
            showToast(R.string.activity_not_found);
            return;
        }
    }

    /**
     * Export a file.
     * 1. first ask the user to choose an app
     * 2. then download the latest version of the file
     * 3. start the choosen app
     *
     * @param fileName The name of the file to share in the current navcontext
     */
    public void exportFile(String fileName) {
        String repoName = navContext.getRepoName();
        String repoID = navContext.getRepoID();
        String dirPath = navContext.getDirPath();
        String fullPath = Utils.pathJoin(dirPath, fileName);
        chooseExportApp(repoName, repoID, fullPath);
    }

    private void chooseExportApp(final String repoName, final String repoID, final String path) {
        PackageManager pm = getPackageManager();
        File file = dataManager.getLocalRepoFile(repoName, repoID, path); 
        Uri uri = Uri.fromFile(file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        // Get a list of apps
        List<ResolveInfo> infos = pm.queryIntentActivities(sendIntent, 0);

        
        // Remove seafile app from the list
        String seadroidPackageName = getPackageName();
        ResolveInfo info;
        Iterator<ResolveInfo> iter = infos.iterator();
        while (iter.hasNext()) {
            info = iter.next();
            if (info.activityInfo.packageName.equals(seadroidPackageName)) {
                iter.remove();
            }
        }
        
        if (infos.isEmpty()) {
            showToast(R.string.no_app_available);
            return;
        }

        AppChoiceDialog dialog = new AppChoiceDialog();
        dialog.init(infos, new AppChoiceDialog.OnAppSelectedListener() {
            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                fetchFileAndExport(appInfo, sendIntent, repoName, repoID,
                                   path);
            }

        });
        dialog.show(getSupportFragmentManager(), CHOOSE_APP_DIALOG_FRAGMENT_TAG);
    }

    private void fetchFileAndExport(final ResolveInfo appInfo, final Intent intent,
                                    final String repoName, final String repoID, final String path) {

        fetchFileDialog = new FetchFileDialog();
        fetchFileDialog.init(repoName, repoID, path, new FetchFileDialog.FetchFileListener() {
            @Override
            public void onSuccess() {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;

                intent.setClassName(packageName, className);
                startActivity(intent);
            }

            @Override
            public void onDismiss() {
                fetchFileDialog = null;
            }

            @Override
            public void onFailure(SeafException err) {
            }
        });
        fetchFileDialog.show(getSupportFragmentManager(), OPEN_FILE_DIALOG_FRAGMENT_TAG);
    }

    // /**
    //  * Share a file. Generating a file share link and send the link to someone
    //  * through some app.
    //  * @param fileName
    //  */
    // public void shareFile(String fileName) {
    //     // TODO: share a file
    //     // String repoID = navContext.getRepoID();
    //     // String dirPath = navContext.getDirPath();
    // }

    // public void shareDir(String dirName) {
    //     // TODO: share a dir
    //     // String repoID = navContext.getRepoID();
    //     // String dirPath = navContext.getDirPath();
    //     // Log.d(DEBUG_TAG, "sharing dir: " + dirName);
    // }


    private void onFileUploadProgress(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        if (uploadTasksFragment != null && uploadTasksFragment.isReady())
            uploadTasksFragment.onTaskProgressUpdate(info);
    }

    private void onFileUploaded(int taskID) {
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

    private void onFileUploadCancelled(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        if (uploadTasksFragment != null && uploadTasksFragment.isReady())
            uploadTasksFragment.onTaskCancelled(info);
    }

    private void onFileUploadFailed(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        showToast(getString(R.string.upload_failed) + " " + Utils.fileNameFromPath(info.localFilePath));
        if (uploadTasksFragment != null && uploadTasksFragment.isReady())
            uploadTasksFragment.onTaskFailed(info);
    }

    private void onFileDownloadProgress(int taskID) {
        if (txService == null) {
            return;
        }

        DownloadTaskInfo info = txService.getDownloadTaskInfo(taskID);
        if (fetchFileDialog != null && fetchFileDialog.getTaskID() == taskID) {
                fetchFileDialog.handleDownloadTaskInfo(info);
        }
    }

    private void onFileDownloaded(int taskID) {
        if (txService == null) {
            return;
        }

        DownloadTaskInfo info = txService.getDownloadTaskInfo(taskID);
        if (fetchFileDialog != null && fetchFileDialog.getTaskID() == taskID) {
            fetchFileDialog.handleDownloadTaskInfo(info);
        } else {
            if (currentTab.equals(LIBRARY_TAB)
                && info.repoID.equals(navContext.getRepoID())
                && Utils.getParentPath(info.path).equals(navContext.getDirPath())) {
                reposFragment.getAdapter().notifyChanged();
            }
        }
    }

    private void onFileDownloadFailed(int taskID) {
        if (txService == null) {
            return;
        }

        DownloadTaskInfo info = txService.getDownloadTaskInfo(taskID);
        if (fetchFileDialog != null && fetchFileDialog.getTaskID() == taskID) {
            fetchFileDialog.handleDownloadTaskInfo(info);
            return;
        }

        SeafException err = info.err;
        final String repoName = info.repoName;
        final String repoID = info.repoID;
        final String path = info.path;

        if (err != null && err.getCode() == 440) {
            if (currentTab.equals(LIBRARY_TAB)
                && repoID.equals(navContext.getRepoID())
                && Utils.getParentPath(path).equals(navContext.getDirPath())) {
                showPasswordDialog(repoName, repoID, new TaskDialog.TaskDialogListener() {
                    @Override
                    public void onTaskSuccess() {
                        txService.addDownloadTask(account, repoName, repoID, path);
                    }
                });
                return;
            }
        }

        showToast(getString(R.string.download_failed) + " " + Utils.fileNameFromPath(path));
    }

    public PasswordDialog showPasswordDialog(String repoName, String repoID,
                                             TaskDialog.TaskDialogListener listener) {
        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.setRepo(repoName, repoID, account);
        passwordDialog.setTaskDialogLisenter(listener);
        passwordDialog.show(getSupportFragmentManager(), PASSWORD_DIALOG_FRAGMENT_TAG);
        return passwordDialog;
    }

    // for receive broadcast from TransferService
    private class TransferReceiver extends BroadcastReceiver {

        private TransferReceiver() {}

        public void onReceive(Context context, Intent intent) {
            String type = intent.getStringExtra("type");
            if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_PROGRESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileDownloadProgress(taskID);

            } else if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileDownloaded(taskID);

            } else if (type.equals(TransferService.BROADCAST_FILE_DOWNLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileDownloadFailed(taskID);

            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_SUCCESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploaded(taskID);

            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_FAILED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadFailed(taskID);

            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_PROGRESS)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadProgress(taskID);
            } else if (type.equals(TransferService.BROADCAST_FILE_UPLOAD_CANCELLED)) {
                int taskID = intent.getIntExtra("taskID", 0);
                onFileUploadCancelled(taskID);
            }
        }

    } // TransferReceiver
}
