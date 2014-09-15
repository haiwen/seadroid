package com.seafile.seadroid2;

import java.io.File;
import java.net.URISyntaxException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
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
import android.content.SharedPreferences;
import android.content.pm.PackageManager;
import android.content.pm.ResolveInfo;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.preference.PreferenceManager;
import android.provider.MediaStore;
import android.support.v4.app.DialogFragment;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.content.LocalBroadcastManager;
import android.text.ClipboardManager;
import android.util.Log;
import android.view.KeyEvent;
import android.webkit.MimeTypeMap;
import android.widget.Toast;

import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuInflater;
import com.actionbarsherlock.view.MenuItem;
import com.actionbarsherlock.view.Window;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SeafStarredFile;
import com.seafile.seadroid2.fileschooser.MultiFileChooserActivity;
import com.seafile.seadroid2.gallery.MultipleImageSelectionActivity;
import com.seafile.seadroid2.monitor.FileMonitorService;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.PendingUploadInfo;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.TransferService.TransferBinder;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.ui.AppChoiceDialog;
import com.seafile.seadroid2.ui.AppChoiceDialog.CustomAction;
import com.seafile.seadroid2.ui.CopyMoveDialog;
import com.seafile.seadroid2.ui.DeleteFileDialog;
import com.seafile.seadroid2.ui.FetchFileDialog;
import com.seafile.seadroid2.ui.GestureLockActivity;
import com.seafile.seadroid2.ui.GetShareLinkDialog;
import com.seafile.seadroid2.ui.NewDirDialog;
import com.seafile.seadroid2.ui.NewFileDialog;
import com.seafile.seadroid2.ui.OpenAsDialog;
import com.seafile.seadroid2.ui.PasswordDialog;
import com.seafile.seadroid2.ui.RenameFileDialog;
import com.seafile.seadroid2.ui.ReposFragment;
import com.seafile.seadroid2.ui.SeafilePathChooserActivity;
import com.seafile.seadroid2.ui.SettingsActivity;
import com.seafile.seadroid2.ui.SslConfirmDialog;
import com.seafile.seadroid2.ui.StarredFragment;
import com.seafile.seadroid2.ui.TabsFragment;
import com.seafile.seadroid2.ui.TaskDialog;
import com.seafile.seadroid2.ui.TaskDialog.TaskDialogListener;
import com.seafile.seadroid2.ui.UploadTasksActivity;
import com.seafile.seadroid2.ui.UploadTasksAdapter;
import com.seafile.seadroid2.util.Utils;

public class BrowserActivity extends SherlockFragmentActivity
        implements ReposFragment.OnFileSelectedListener, StarredFragment.OnStarredFileSelectedListener, OnBackStackChangedListener {
    public static final String PKG_NAME = "com.seafile.seadroid2";
    public static final String EXTRA_REPO_NAME = PKG_NAME + ".repoName";
    public static final String EXTRA_REPO_ID = PKG_NAME + ".repoID";
    public static final String EXTRA_FILE_PATH = PKG_NAME + ".filePath";
    public static final String EXTRA_ACCOUT = PKG_NAME + ".account";
    private static final String DEBUG_TAG = "BrowserActivity";
    public static final String ACTIONBAR_PARENT_PATH = "/";
    private Account account;
    NavContext navContext = null;
    DataManager dataManager = null;
    TransferService txService = null;
    TransferReceiver mTransferReceiver;

    // private boolean twoPaneMode = false;
    TabsFragment tabsFragment = null;
    private String currentSelectedItem = FILES_VIEW;

    FetchFileDialog fetchFileDialog = null;

    AppChoiceDialog appChoiceDialog = null;

    private Menu overFlowMenu;

    private static final String UPLOAD_TASKS_VIEW = "UploadTasks";
    private static final String FILES_VIEW = "Files";

    private static final String LIBRARY_TAB = "Libraries";
    private static final String ACTIVITY_TAB = "Activities";
    private static final String STARRED_TAB = "Starred";

    public static final String REPOS_FRAGMENT_TAG = "repos_fragment";
    public static final String UPLOAD_TASKS_FRAGMENT_TAG = "upload_tasks_fragment";
    public static final String SETTINGS_FRAGMENT_TAG = "settings_fragment";
    public static final String TABS_FRAGMENT_TAG = "tabs_main";
    public static final String ACTIVITIES_FRAGMENT_TAG = "activities_fragment";
    public static final String OPEN_FILE_DIALOG_FRAGMENT_TAG = "openfile_fragment";
    public static final String PASSWORD_DIALOG_FRAGMENT_TAG = "password_fragment";
    public static final String CHOOSE_APP_DIALOG_FRAGMENT_TAG = "choose_app_fragment";
    public static final String PICK_FILE_DIALOG_FRAGMENT_TAG = "pick_file_fragment";

    public static final String LOCK = "lock";
    public static final String LOCK_KEY = null;
    public static final String GESTURE_LOCK_SWITCH_KEY = "gesture_lock_switch_key";
    public static final String CAMERA_UPLOAD_SWITCH_KEY = "camera_upload_switch_key";
    public static final String CAMERA_UPLOAD_REPO_KEY = "camera_upload_repo_key";

    private Intent copyMoveIntent;

    private CopyMoveContext copyMoveContext;

    public DataManager getDataManager() {
        return dataManager;
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

    public void disableActionBarTitle() {
        getSupportActionBar().setDisplayShowTitleEnabled(false);
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        requestWindowFeature(Window.FEATURE_INDETERMINATE_PROGRESS);
        super.onCreate(savedInstanceState);

        SharedPreferences settings = PreferenceManager.getDefaultSharedPreferences(this);
        String lockPattenString = settings.getString(LOCK_KEY, null);
        if (lockPattenString != null) {
            Intent intent = new Intent(this, GestureLockActivity.class);
            startActivity(intent);
        }
        // Get the message from the intent
        Intent intent = getIntent();
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");
        String token = intent.getStringExtra("token");
        account = new Account(server, email, null, token);
        Log.d(DEBUG_TAG, "browser activity onCreate " + server + " " + email);

        SharedPreferences sharedPref = getSharedPreferences(AccountsActivity.SHARED_PREF_NAME, Context.MODE_PRIVATE);
        if (server == null) {
            String latest_server = sharedPref.getString(AccountsActivity.SHARED_PREF_SERVER_KEY, null);
            String latest_email = sharedPref.getString(AccountsActivity.SHARED_PREF_EMAIL_KEY, null);
            String latest_token = sharedPref.getString(AccountsActivity.SHARED_PREF_TOKEN_KEY, null);
            if (latest_server != null) {
                account = new Account(latest_server, latest_email, null, latest_token);
            } else {
                Intent newIntent = new Intent(this, AccountsActivity.class);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
                newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
                startActivity(newIntent);
                finish();
                return;
            }

        }

        dataManager = new DataManager(account);
        navContext = new NavContext();

        getSupportFragmentManager().addOnBackStackChangedListener(this);

        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayShowTitleEnabled(true);
        unsetRefreshing();
        disableUpButton();

        if (savedInstanceState != null) {
            Log.d(DEBUG_TAG, "savedInstanceState is not null");
            tabsFragment = (TabsFragment)
                    getSupportFragmentManager().findFragmentByTag(TABS_FRAGMENT_TAG);
            fetchFileDialog = (FetchFileDialog)
                    getSupportFragmentManager().findFragmentByTag(OPEN_FILE_DIALOG_FRAGMENT_TAG);

            appChoiceDialog = (AppChoiceDialog)
                getSupportFragmentManager().findFragmentByTag(CHOOSE_APP_DIALOG_FRAGMENT_TAG);

            if (appChoiceDialog != null) {
                FragmentTransaction ft = getSupportFragmentManager().beginTransaction();
                ft.detach(appChoiceDialog);
                ft.commit();
            }

            SslConfirmDialog sslConfirmDlg = (SslConfirmDialog)
                getSupportFragmentManager().findFragmentByTag(SslConfirmDialog.FRAGMENT_TAG);

            if (sslConfirmDlg != null) {
                Log.d(DEBUG_TAG, "sslConfirmDlg is not null");
                FragmentTransaction ft = getSupportFragmentManager().beginTransaction();
                ft.detach(sslConfirmDlg);
                ft.commit();
            } else {
                Log.d(DEBUG_TAG, "sslConfirmDlg is null");
            }

            String repoID = savedInstanceState.getString("repoID");
            String repoName = savedInstanceState.getString("repoName");
            String path = savedInstanceState.getString("path");
            String dirID = savedInstanceState.getString("dirID");
            if (repoID != null) {
                navContext.setRepoID(repoID);
                navContext.setRepoName(repoName);
                navContext.setDir(path, dirID);
            }
        } else {
            Log.d(DEBUG_TAG, "savedInstanceState is null");
            tabsFragment = new TabsFragment();
            getSupportFragmentManager()
                .beginTransaction()
                .add(R.id.fragment_container,
                    tabsFragment, TABS_FRAGMENT_TAG)
                .commit();
        }

        setContentView(R.layout.seadroid_main);

        // enable ActionBar app icon to behave as action back
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);

        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");

        Intent monitorIntent = new Intent(this, FileMonitorService.class);
        startService(monitorIntent);

    }


    private String getCurrentTabName() {

        int index = tabsFragment.getCurrentTabIndex();

        switch (index) {
        case 0 :
            return LIBRARY_TAB;
        case 1 :
            return STARRED_TAB;
        case 2 :
            return ACTIVITY_TAB;
        default:
            return "";

        }
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
    protected void onPause() {
        Log.d(DEBUG_TAG, "onPause");
      super.onPause();
    }

    @Override
    public void onRestart() {
        Log.d(DEBUG_TAG, "onRestart");
        super.onStart();
    }

    @Override
    protected void onNewIntent(Intent intent) {
        Log.d(DEBUG_TAG, "onNewIntent");
        String server = intent.getStringExtra("server");
        String email = intent.getStringExtra("email");

        Account selectedAccount = new Account(server, email);
        if (!account.equals(selectedAccount)) {
            finish();
            startActivity(intent);
        }
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
        //outState.putInt("tab", getSupportActionBar().getSelectedNavigationIndex());
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
        overFlowMenu = menu;
        return true;
    }
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        MenuItem menuUpload = menu.findItem(R.id.upload);
        MenuItem menuRefresh = menu.findItem(R.id.refresh);
        MenuItem menuNewDir = menu.findItem(R.id.newdir);
        MenuItem menuNewFile = menu.findItem(R.id.newfile);
        MenuItem menuCamera = menu.findItem(R.id.camera);
        MenuItem menuUploadTasks = menu.findItem(R.id.upload_tasks);
        MenuItem menuAccounts = menu.findItem(R.id.accounts);
        MenuItem menuSettings = menu.findItem(R.id.settings);

        if (getCurrentTabName().equals(LIBRARY_TAB)) {
            menuUpload.setVisible(true);
            if (navContext.inRepo() && hasRepoWritePermission()) {
                menuUpload.setEnabled(true);
            }
            else
                menuUpload.setEnabled(false);
        } else {
            menuUpload.setVisible(false);
        }

        if (getCurrentTabName().equals(LIBRARY_TAB)) {
            menuRefresh.setVisible(true);
            menuUploadTasks.setVisible(true);
            menuAccounts.setVisible(true);
            menuSettings.setVisible(true);
        } else if (getCurrentTabName().equals(ACTIVITY_TAB)) {
            menuRefresh.setVisible(true);
            menuUploadTasks.setVisible(true);
            menuAccounts.setVisible(true);
            menuSettings.setVisible(true);
        } else {
            menuRefresh.setVisible(false);
            menuUploadTasks.setVisible(false);
            menuAccounts.setVisible(false);
            menuSettings.setVisible(false);
        }

        if (getCurrentTabName().equals(LIBRARY_TAB)) {
            if (navContext.inRepo() && hasRepoWritePermission()) {
                menuNewDir.setVisible(true);
                menuNewFile.setVisible(true);
                menuCamera.setVisible(true);
            } else {
                menuNewDir.setVisible(false);
                menuNewFile.setVisible(false);
                menuCamera.setVisible(false);
            }
        } else {
            menuNewDir.setVisible(false);
            menuNewFile.setVisible(false);
            menuCamera.setVisible(false);
        }

        if (currentSelectedItem.equals(UPLOAD_TASKS_VIEW)) {
            menuUpload.setVisible(false);
            menuRefresh.setVisible(false);
            menuNewDir.setVisible(false);
            menuNewFile.setVisible(false);
            menuCamera.setVisible(false);
            menuUploadTasks.setVisible(false);
            menuAccounts.setVisible(false);
            menuSettings.setVisible(false);
        }

        if (getCurrentTabName().equals(STARRED_TAB)) {
            menuUpload.setVisible(false);
            menuNewDir.setVisible(false);
            menuNewFile.setVisible(false);
            menuCamera.setVisible(false);
            menuRefresh.setVisible(true);
            menuUploadTasks.setVisible(true);
            menuAccounts.setVisible(true);
            menuSettings.setVisible(true);
        }

        return true;
    }

    public static UploadTasksAdapter uploadTasksAdapter;

    private List<UploadTaskInfo> getUploadTaskInfos() {
        if (txService == null) {
            // In case the service is not ready
            return new ArrayList<UploadTaskInfo>();
        }

        return txService.getAllUploadTaskInfos();
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {

        switch (item.getItemId()) {
        case android.R.id.home:
            if (navContext.inRepo()) {
                onBackPressed();
            }
            return true;
        case R.id.upload:
            pickFile();
            return true;
        case R.id.upload_tasks:
            Intent newIntent = new Intent(this, UploadTasksActivity.class);
            uploadTasksAdapter = new UploadTasksAdapter(this, getUploadTaskInfos());
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(newIntent);
            return true;
        case R.id.accounts:
            newIntent = new Intent(this, AccountsActivity.class);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(newIntent);
            return true;
        case R.id.refresh:
            if (!Utils.isNetworkOn()) {
                showToast(R.string.network_down);
                return true;
            }
            if (getCurrentTabName().equals(LIBRARY_TAB)) {
                if (navContext.inRepo()) {
                    SeafRepo repo = dataManager.getCachedRepoByID(navContext.getRepoID());
                    if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
                        String password = DataManager.getRepoPassword(repo.id);
                        showPasswordDialog(repo.name, repo.id,
                            new TaskDialog.TaskDialogListener() {
                                @Override
                                public void onTaskSuccess() {
                                    tabsFragment.getReposFragment().refreshView(true);
                                }
                            } , password);

                        return true;
                    }
                }

                tabsFragment.getReposFragment().refreshView(true);
            } else if (getCurrentTabName().equals(ACTIVITY_TAB)) {
                tabsFragment.getActivitiesFragment().refreshView();
            } else if (getCurrentTabName().equals(STARRED_TAB)) {
                tabsFragment.getStarredFragment().refreshView();
            }
            return true;
        case R.id.newdir:
            showNewDirDialog();
            return true;
        case R.id.newfile:
            showNewFileDialog();
            return true;
        case R.id.camera:
            CameraTakePhoto();
            return true;
        case R.id.settings:
            Intent settingsIntent = new Intent(BrowserActivity.this,SettingsActivity.class);
            settingsIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(settingsIntent);
            return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);
    }

    @Override
    protected void onPostResume() {
        super.onPostResume();
        // We can't show the CopyMoveDialog in onActivityResult, this is a
        // workaround found in
        // http://stackoverflow.com/questions/16265733/failure-delivering-result-onactivityforresult/18345899#18345899
        if (copyMoveIntent != null) {
            String dstRepoId, dstDir;
            dstRepoId = copyMoveIntent.getStringExtra(SeafilePathChooserActivity.DATA_REPO_ID);
            dstDir = copyMoveIntent.getStringExtra(SeafilePathChooserActivity.DATA_DIR);
            copyMoveContext.setDest(dstRepoId, dstDir);
            doCopyMove();
            copyMoveIntent = null;
        }
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
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
                ReposFragment reposFragment = tabsFragment.getReposFragment();
                if (getCurrentTabName().equals(LIBRARY_TAB) && reposFragment != null) {
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
                ReposFragment reposFragment = tabsFragment.getReposFragment();
                if (getCurrentTabName().equals(LIBRARY_TAB) && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    public void setRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.TRUE);
    }

    public void unsetRefreshing() {
        setSupportProgressBarIndeterminateVisibility(Boolean.FALSE);
    }

    private String strImgPath;

    private void CameraTakePhoto() {
        Intent imageCaptureIntent = new Intent("android.media.action.IMAGE_CAPTURE");

        String strImgDirPath = DataManager.getExternalRootDirectory() + "/myPhotos/";
        String fileName = new SimpleDateFormat("yyyyMMddHHmmss").format(new Date()) + ".jpg";
        strImgPath = strImgDirPath + fileName;

        File ImgDir = new File(strImgDirPath);
        if (!ImgDir.exists()) {
            ImgDir.mkdirs();
        }
        File Img = new File(strImgDirPath, fileName);
        while (Img.exists()) {
            fileName = fileName + "(1)";
            strImgPath = strImgDirPath + fileName;
        }
        Uri photo = Uri.fromFile(Img);
        imageCaptureIntent.putExtra(MediaStore.EXTRA_OUTPUT, photo);
        startActivityForResult(imageCaptureIntent, TAKE_PHOTO_REQUEST);
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

    public void setUpButtonTitle(String title){
        getSupportActionBar().setTitle(title);
    } 
    
    /***********  Start other activity  ***************/

    public static final int PICK_FILES_REQUEST = 1;
    public static final int PICK_PHOTOS_VIDEOS_REQUEST = 2;
    public static final int PICK_FILE_REQUEST = 3;
    public static final int TAKE_PHOTO_REQUEST = 4;
    public static final int CHOOSE_COPY_MOVE_DEST_REQUEST = 5;

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
                                Intent intent = new Intent(BrowserActivity.this, MultiFileChooserActivity.class);
                                getActivity().startActivityForResult(intent, PICK_FILES_REQUEST);
                                break;
                            case 1:
                                // photos
                                intent = new Intent(BrowserActivity.this, MultipleImageSelectionActivity.class);
                                getActivity().startActivityForResult(intent, PICK_PHOTOS_VIDEOS_REQUEST);
                                break;
                            case 2:
                                // thirdparty file chooser
                                Intent target = Utils.createGetContentIntent();
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
        switch (requestCode) {
        case PICK_FILES_REQUEST:
            if (resultCode == RESULT_OK) {
                String[] paths = data.getStringArrayExtra(MultiFileChooserActivity.MULTI_FILES_PATHS);
                if (paths == null)
                    return;
                showToast(getString(R.string.added_to_upload_tasks));
                for (String path : paths) {
                    addUploadTask(navContext.getRepoID(),
                        navContext.getRepoName(), navContext.getDirPath(), path);
                }
            }
            break;
        case PICK_PHOTOS_VIDEOS_REQUEST:
            if (resultCode == RESULT_OK) {
                ArrayList<String> paths = data.getStringArrayListExtra("photos");
                if (paths == null)
                    return;
                showToast(getString(R.string.added_to_upload_tasks));
                for (String path : paths) {
                    addUploadTask(navContext.getRepoID(),
                        navContext.getRepoName(), navContext.getDirPath(), path);
                }
            }
            break;
        case PICK_FILE_REQUEST:
            if (resultCode == RESULT_OK) {
                if (!Utils.isNetworkOn()) {
                    showToast(R.string.network_down);
                    return;
                }

                Uri uri = data.getData();
                String path;
                try {
                    path = Utils.getPath(this, uri);
                } catch (URISyntaxException e) {
                    e.printStackTrace();
                    return;
                }
                if(path == null) {
                    showToast("Unable to upload, no path available");
                    Log.i(DEBUG_TAG, "Pick file request did not return a path");
                    return;
                }
                showToast(getString(R.string.added_to_upload_tasks));
                //showToast(getString(R.string.upload) + " " + Utils.fileNameFromPath(path));
                addUploadTask(navContext.getRepoID(),
                    navContext.getRepoName(), navContext.getDirPath(), path);
            }
            break;
        case CHOOSE_COPY_MOVE_DEST_REQUEST:
            if (resultCode == RESULT_OK) {
                if (!Utils.isNetworkOn()) {
                    showToast(R.string.network_down);
                    return;
                }

                copyMoveIntent = data;
            }
            break;
        case TAKE_PHOTO_REQUEST:
            if (resultCode == RESULT_OK) {
                showToast(getString(R.string.take_photo_successfully));
                if (!Utils.isNetworkOn()) {
                    showToast(R.string.network_down);
                    return;
                }

                if(strImgPath == null) {
                    showToast("Unable to upload, no path available");
                    Log.i(DEBUG_TAG, "Pick file request did not return a path");
                    return;
                }
                showToast(getString(R.string.added_to_upload_tasks));
                addUploadTask(navContext.getRepoID(),
                        navContext.getRepoName(), navContext.getDirPath(), strImgPath);
            }
            break;
        default:
             break;
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

        startFileActivity(repoName, repoID, filePath);
    }

    private void startFileActivity(String repoName, String repoID, String filePath) {
        int taskID = txService.addDownloadTask(account, repoName, repoID, filePath);
        Intent intent = new Intent(this, FileActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", account);
        intent.putExtra("taskID", taskID);
        startActivity(intent);
    }

    @Override
    public void onStarredFileSelected(SeafStarredFile starredFile) {

        final String repoID = starredFile.getRepoID();
        SeafRepo seafRepo = dataManager.getCachedRepoByID(repoID);
        final String repoName = seafRepo.getName();
        final String filePath = starredFile.getPath();

        File localFile = dataManager.getLocalCachedFile(repoName, repoID, filePath, null);
        if (localFile != null) {
            showFile(localFile);
            return;
        }

        startFileActivity(repoName, repoID, filePath);
    }
    
    @Override
    public void onBackPressed() {
        if (getSupportFragmentManager().getBackStackEntryCount() != 0) {
            getSupportFragmentManager().popBackStack();
            return;
        }

        if (currentSelectedItem == FILES_VIEW && getCurrentTabName().equals(LIBRARY_TAB)) {
            if (navContext.inRepo()) {
                if (navContext.isRepoRoot()) {
                    navContext.setRepoID(null);
                    getSupportActionBar().setTitle(R.string.app_name);
                } else {
                    String parentPath = Utils.getParentPath(navContext
                            .getDirPath());
                    navContext.setDir(parentPath, null);
                    if (parentPath.equals(ACTIONBAR_PARENT_PATH)) {
                        getSupportActionBar().setTitle(navContext.getRepoName());
                    }else {
                        getSupportActionBar().setTitle(parentPath.substring(parentPath.lastIndexOf(ACTIONBAR_PARENT_PATH) + 1));
                    }
                }
                tabsFragment.getReposFragment().refreshView();

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
            new OpenAsDialog(file).show(getSupportFragmentManager(), "OpenAsDialog");
            //showToast(R.string.activity_not_found);
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
        final File file = dataManager.getLocalRepoFile(repoName, repoID, path);
        Uri uri = Uri.fromFile(file);

        final Intent sendIntent = new Intent();
        sendIntent.setAction(Intent.ACTION_SEND);
        sendIntent.setType(Utils.getFileMimeType(file));
        sendIntent.putExtra(Intent.EXTRA_STREAM, uri);

        // Get a list of apps
        List<ResolveInfo> infos = getAppsByIntent(sendIntent);

        if (infos.isEmpty()) {
            showToast(R.string.no_app_available);
            return;
        }

        AppChoiceDialog dialog = new AppChoiceDialog();
        dialog.init(getString(R.string.export_file), infos, new AppChoiceDialog.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(CustomAction action) {
            }
            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                sendIntent.setClassName(packageName, className);

                if (!Utils.isNetworkOn() && file.exists()) {
                    startActivity(sendIntent);
                    return;
                }
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

    /**
     * Share a file. Generating a file share link and send the link to someone
     * through some app.
     * @param fileName
     */
    public void shareFile(String repoID, String path) {
        chooseShareApp(repoID, path, false);
    }

    public void shareDir(String repoID, String path) {
        chooseShareApp(repoID, path, true);
    }

    private List<ResolveInfo> getAppsByIntent(Intent intent) {
        PackageManager pm = getPackageManager();
        List<ResolveInfo> infos = pm.queryIntentActivities(intent, 0);

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

        return infos;
    }

    private void chooseShareApp(final String repoID, final String path, final boolean isdir) {
        final Intent shareIntent = new Intent();
        shareIntent.setAction(Intent.ACTION_SEND);
        shareIntent.setType("text/plain");

        // Get a list of apps
        List<ResolveInfo> infos = getAppsByIntent(shareIntent);

        String title = getString(isdir ? R.string.share_dir_link : R.string.share_file_link);

        AppChoiceDialog dialog = new AppChoiceDialog();
        dialog.addCustomAction(0, getResources().getDrawable(R.drawable.copy_link),
                               getString(R.string.copy_link));
        dialog.init(title, infos, new AppChoiceDialog.OnItemSelectedListener() {
            @Override
            public void onCustomActionSelected(CustomAction action) {
                final GetShareLinkDialog gdialog = new GetShareLinkDialog();
                gdialog.init(repoID, path, isdir, account);
                gdialog.setTaskDialogLisenter(new TaskDialogListener() {
                    @Override
                    @SuppressWarnings("deprecation")
                    public void onTaskSuccess() {
                        ClipboardManager clipboard = (ClipboardManager)
                            getSystemService(Context.CLIPBOARD_SERVICE);
                        clipboard.setText(gdialog.getLink());
                        // ClipData clip = ClipData.newPlainText("seafile shared link", gdialog.getLink());
                        // clipboard.setPrimaryClip(clip);
                        showToast(R.string.link_ready_to_be_pasted);
                    }
                });
                gdialog.show(getSupportFragmentManager(), "DialogFragment");
            }

            @Override
            public void onAppSelected(ResolveInfo appInfo) {
                String className = appInfo.activityInfo.name;
                String packageName = appInfo.activityInfo.packageName;
                shareIntent.setClassName(packageName, className);

                final GetShareLinkDialog gdialog = new GetShareLinkDialog();
                gdialog.init(repoID, path, isdir, account);
                gdialog.setTaskDialogLisenter(new TaskDialogListener() {
                    @Override
                    public void onTaskSuccess() {
                        shareIntent.putExtra(Intent.EXTRA_TEXT, gdialog.getLink());
                        startActivity(shareIntent);
                    }
                });
                gdialog.show(getSupportFragmentManager(), "DialogFragment");
            }

        });
        dialog.show(getSupportFragmentManager(), CHOOSE_APP_DIALOG_FRAGMENT_TAG);
    }

    public void renameFile(String repoID, String repoName, String path) {
        doRename(repoID, repoName, path, false);
    }

    public void renameDir(String repoID, String repoName, String path) {
        doRename(repoID, repoName, path, true);
    }

    private void doRename(String repoID, String repoName, String path, boolean isdir) {
        final RenameFileDialog dialog = new RenameFileDialog();
        dialog.init(repoID, path, isdir, account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showToast(R.string.rename_successful);
                ReposFragment reposFragment = tabsFragment.getReposFragment();
                if (getCurrentTabName().equals(LIBRARY_TAB) && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    public void deleteFile(String repoID, String repoName, String path) {
        doDelete(repoID, repoName, path, false);
    }

    public void deleteDir(String repoID, String repoName, String path) {
        doDelete(repoID, repoName, path, true);
    }

    private void doDelete(String repoID, String repoName, String path, boolean isdir) {
        final DeleteFileDialog dialog = new DeleteFileDialog();
        dialog.init(repoID, path, isdir, account);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showToast(R.string.delete_successful);
                ReposFragment reposFragment = tabsFragment.getReposFragment();
                if (getCurrentTabName().equals(LIBRARY_TAB) && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    public void copyFile(String srcRepoId, String srcRepoName, String srcDir, String srcFn, boolean isdir) {
        chooseCopyMoveDest(srcRepoId, srcRepoName, srcDir, srcFn, isdir, CopyMoveContext.OP.COPY);
    }

    public void moveFile(String srcRepoId, String srcRepoName, String srcDir, String srcFn, boolean isdir) {
        chooseCopyMoveDest(srcRepoId, srcRepoName, srcDir, srcFn, isdir, CopyMoveContext.OP.MOVE);
    }

    private void chooseCopyMoveDest(String repoID, String repoName, String path,
                                    String filename, boolean isdir, CopyMoveContext.OP op) {
        copyMoveContext = new CopyMoveContext(repoID, repoName, path, filename,
                                              isdir, op);
        Intent intent = new Intent(this, SeafilePathChooserActivity.class);
        intent.putExtra(SeafilePathChooserActivity.DATA_ACCOUNT, account);
        SeafRepo repo = dataManager.getCachedRepoByID(repoID);
        if (repo.encrypted) {
            intent.putExtra(SeafilePathChooserActivity.ENCRYPTED_REPO_ID, repoID);
        }
        startActivityForResult(intent, CHOOSE_COPY_MOVE_DEST_REQUEST);
        return;
    }

    private void doCopyMove() {
        if (!copyMoveContext.checkCopyMoveToSubfolder()) {
            showToast(copyMoveContext.isCopy()
                      ? R.string.cannot_copy_folder_to_subfolder
                      : R.string.cannot_move_folder_to_subfolder);
            return;
        }
        final CopyMoveDialog dialog = new CopyMoveDialog();
        dialog.init(account, copyMoveContext);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                showToast(copyMoveContext.isCopy()
                          ? R.string.copied_successfully
                          : R.string.moved_successfully);
                if (copyMoveContext.isMove()) {
                    ReposFragment reposFragment = tabsFragment.getReposFragment();
                    if (getCurrentTabName().equals(LIBRARY_TAB) && reposFragment != null) {
                        reposFragment.refreshView();
                    }
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    private void onFileUploaded(int taskID) {
        if (txService == null) {
            return;
        }

        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);

        String repoID = info.repoID;
        String dir = info.parentDir;
        if (getCurrentTabName().equals(LIBRARY_TAB)
                && repoID.equals(navContext.getRepoID())
                && dir.equals(navContext.getDirPath())) {
            tabsFragment.getReposFragment().refreshView(true);
            String verb = getString(info.isUpdate ? R.string.updated : R.string.uploaded);
            showToast(verb + " " + Utils.fileNameFromPath(info.localFilePath));
        }
    }

    private void onFileUploadFailed(int taskID) {
        if (txService == null) {
            return;
        }
        UploadTaskInfo info = txService.getUploadTaskInfo(taskID);
        showToast(getString(R.string.upload_failed) + " " + Utils.fileNameFromPath(info.localFilePath));
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
            if (getCurrentTabName().equals(LIBRARY_TAB)
                && info.repoID.equals(navContext.getRepoID())
                && Utils.getParentPath(info.pathInRepo).equals(navContext.getDirPath())) {
                tabsFragment.getReposFragment().getAdapter().notifyChanged();
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
        final String path = info.pathInRepo;

        if (err != null && err.getCode() == SeafConnection.HTTP_STATUS_REPO_PASSWORD_REQUIRED) {
            if (getCurrentTabName().equals(LIBRARY_TAB)
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
        return showPasswordDialog(repoName, repoID, listener, null);
    }

    public PasswordDialog showPasswordDialog(String repoName, String repoID,
                                             TaskDialog.TaskDialogListener listener, String password) {
        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.setRepo(repoName, repoID, account);
        if (password != null) {
            passwordDialog.setPassword(password);
        }
        passwordDialog.setTaskDialogLisenter(listener);
        passwordDialog.show(getSupportFragmentManager(), PASSWORD_DIALOG_FRAGMENT_TAG);
        return passwordDialog;
    }

    public void setSelectedTab(int index) {
        tabsFragment.setSelectedTab(index);
    }

    @Override
    public boolean onKeyUp(int keycode, KeyEvent e) {
        switch(keycode) {
        case KeyEvent.KEYCODE_MENU:
            if (overFlowMenu !=null) {
                overFlowMenu.performIdentifierAction(R.id.menu_overflow, 0);
            }
        }

        return super.onKeyUp(keycode, e);
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

            }
        }

    } // TransferReceiver

}
