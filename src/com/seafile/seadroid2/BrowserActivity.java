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
import android.content.res.Configuration;
import android.graphics.drawable.Drawable;
import android.net.Uri;
import android.os.Bundle;
import android.os.IBinder;
import android.support.v4.app.DialogFragment;
import android.support.v4.app.Fragment;
import android.support.v4.app.FragmentManager.OnBackStackChangedListener;
import android.support.v4.app.FragmentTransaction;
import android.support.v4.content.LocalBroadcastManager;
import android.text.ClipboardManager;
import android.util.Log;
import android.view.ActionProvider;
import android.view.ContextMenu.ContextMenuInfo;
import android.view.SubMenu;
import android.view.View;
import android.webkit.MimeTypeMap;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.ListView;
import android.widget.Toast;

import com.actionbarsherlock.app.ActionBar;
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
import com.seafile.seadroid2.data.SeafStarredFile;
import com.seafile.seadroid2.gallery.MultipleImageSelectionActivity;
import com.seafile.seadroid2.ui.ActivitiesFragment;
import com.seafile.seadroid2.ui.AppChoiceDialog;
import com.seafile.seadroid2.ui.AppChoiceDialog.CustomAction;
import com.seafile.seadroid2.ui.FetchFileDialog;
import com.seafile.seadroid2.ui.GetShareLinkDialog;
import com.seafile.seadroid2.ui.NewDirDialog;
import com.seafile.seadroid2.ui.NewFileDialog;
import com.seafile.seadroid2.ui.PasswordDialog;
import com.seafile.seadroid2.ui.RenameFileDialog;
import com.seafile.seadroid2.ui.ReposFragment;
import com.seafile.seadroid2.ui.TaskDialog;
import com.seafile.seadroid2.ui.TaskDialog.TaskDialogListener;
import com.seafile.seadroid2.ui.UploadTasksFragment;

import android.support.v4.widget.DrawerLayout;
import android.support.v4.app.ActionBarDrawerToggle;
import android.support.v4.view.GravityCompat;
import com.seafile.seadroid2.ui.TabsFragment;
import com.seafile.seadroid2.ui.StarredFragment;

public class BrowserActivity extends SherlockFragmentActivity
        implements ReposFragment.OnFileSelectedListener, StarredFragment.OnStarredFileSelectedListener, OnBackStackChangedListener {

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
    UploadTasksFragment uploadTasksFragment = null;
    TabsFragment tabsFragment = null;
    private String currentSelectedItem = LIBRARY_TAB;

    FetchFileDialog fetchFileDialog = null;

    AppChoiceDialog appChoiceDialog = null;

    private static final String LIBRARY_TAB = "Libraries";
    private static final String UPLOAD_TASKS_TAB = "upload-tasks";
    private static final String ACTIVITY_TAB = "Activities";
    private static final String STARRED_TAB = "Starred";

    public static final String REPOS_FRAGMENT_TAG = "repos_fragment";
    public static final String UPLOAD_TASKS_FRAGMENT_TAG = "upload_tasks_fragment";
    public static final String TABS_FRAGMENT_TAG = "tabs_main";
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

    public void disableActionBarTitle() {
        getSupportActionBar().setDisplayShowTitleEnabled(false);
    }

    private DrawerLayout mDrawerLayout;
    private ListView mDrawerList;
    private ActionBarDrawerToggle mDrawerToggle;

    private CharSequence mDrawerTitle;
    private CharSequence mTitle;
    private String[] mNavTitles;
    
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

        getSupportFragmentManager().addOnBackStackChangedListener(this);

        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayShowTitleEnabled(false);
        unsetRefreshing();

        if (savedInstanceState != null) {
        	tabsFragment = (TabsFragment)
        			getSupportFragmentManager().findFragmentByTag(TABS_FRAGMENT_TAG);
        	uploadTasksFragment = (UploadTasksFragment)
                    getSupportFragmentManager().findFragmentByTag(UPLOAD_TASKS_FRAGMENT_TAG);
        	
            fetchFileDialog = (FetchFileDialog)
                    getSupportFragmentManager().findFragmentByTag(OPEN_FILE_DIALOG_FRAGMENT_TAG);

            appChoiceDialog = (AppChoiceDialog)
                getSupportFragmentManager().findFragmentByTag(CHOOSE_APP_DIALOG_FRAGMENT_TAG);

            if (appChoiceDialog != null) {
                FragmentTransaction ft = getSupportFragmentManager().beginTransaction();
                ft.detach(appChoiceDialog);
                ft.commit();
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
        }

        setContentView(R.layout.seadroid_main);
        
        mTitle = mDrawerTitle = getTitle();
        mNavTitles = getResources().getStringArray(R.array.nav_array);
        mDrawerLayout = (DrawerLayout) findViewById(R.id.drawer_layout);
        mDrawerList = (ListView) findViewById(R.id.left_drawer);

        // set a custom shadow that overlays the main content when the drawer opens
        mDrawerLayout.setDrawerShadow(R.drawable.drawer_shadow, GravityCompat.START);
        // set up the drawer's list view with items and click listener
        mDrawerList.setAdapter(new ArrayAdapter<String>(this,
                R.layout.drawer_list_item, mNavTitles));
        mDrawerList.setOnItemClickListener(new DrawerItemClickListener());

        // enable ActionBar app icon to behave as action to toggle nav drawer
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        getSupportActionBar().setHomeButtonEnabled(true);

        // ActionBarDrawerToggle ties together the the proper interactions
        // between the sliding drawer and the action bar app icon
        mDrawerToggle = new ActionBarDrawerToggle(
                this,                  /* host Activity */
                mDrawerLayout,         /* DrawerLayout object */
                R.drawable.ic_drawer,  /* nav drawer image to replace 'Up' caret */
                R.string.drawer_open,  /* "open drawer" description for accessibility */
                R.string.drawer_close  /* "close drawer" description for accessibility */
                ) {
            public void onDrawerClosed(View view) {
            	getSupportActionBar().setTitle(mTitle);
            	supportInvalidateOptionsMenu(); // creates call to onPrepareOptionsMenu()
            }

            public void onDrawerOpened(View drawerView) {
            	getSupportActionBar().setTitle(mDrawerTitle);
            	supportInvalidateOptionsMenu(); // creates call to onPrepareOptionsMenu()
            }
        };
        mDrawerLayout.setDrawerListener(mDrawerToggle);

        if (savedInstanceState == null) {
        	tabsFragment = new TabsFragment();
            getSupportFragmentManager().beginTransaction().replace(R.id.content_frame, tabsFragment).commit();
        }
        
        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        Log.d(DEBUG_TAG, "try bind TransferService");

        
    }
    
    private String getCurrentTabName() {
    	
    	int	index = tabsFragment.getCurrentTabIndex();
    	
    	switch (index) {
    	case 0 :
    		return LIBRARY_TAB;
    	case 1 :
    		return ACTIVITY_TAB;
    	case 2 :
    		return STARRED_TAB;
    	default:
    		return new String();
    	
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

            if (currentSelectedItem.equals(UPLOAD_TASKS_TAB)
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
        return true;
    }

    private class DrawerItemClickListener implements ListView.OnItemClickListener {
        @Override
        public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
            selectItem(position);
        }
    }
    
    private void selectItem(int position) {
        switch (position) {
        case 0 :
        	if (uploadTasksFragment == null) {
        		uploadTasksFragment = new UploadTasksFragment();
        	}
        	getSupportFragmentManager().beginTransaction().replace(R.id.content_frame, uploadTasksFragment).commit();
        	currentSelectedItem = UPLOAD_TASKS_TAB;
        	break;
        case 1 :
            Intent newIntent = new Intent(this, AccountsActivity.class);
            newIntent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(newIntent);
            break;
        default:
            break;
        		
        }
        
        mDrawerList.setItemChecked(position, true);
        setTitle(mNavTitles[position]);
        mDrawerLayout.closeDrawer(mDrawerList);
        
    }

    @Override
    public void setTitle(CharSequence title) {
        mTitle = title;
        getSupportActionBar().setTitle(mTitle);
    }
    
    @Override
    public boolean onPrepareOptionsMenu(Menu menu) {
        MenuItem menuUpload = menu.findItem(R.id.upload);
        MenuItem menuRefresh = menu.findItem(R.id.refresh);
        MenuItem menuNewDir = menu.findItem(R.id.newdir);
        MenuItem menuNewFile = menu.findItem(R.id.newfile);
        
        boolean drawerOpen = mDrawerLayout.isDrawerOpen(mDrawerList);
        if (getCurrentTabName().equals(LIBRARY_TAB) && !drawerOpen) {
            menuUpload.setVisible(true);
            if (navContext.inRepo() && hasRepoWritePermission()) {
                menuUpload.setEnabled(true);
            }
            else
                menuUpload.setEnabled(false);
        } else {
            menuUpload.setVisible(false);
        }

        if (getCurrentTabName().equals(LIBRARY_TAB) && !drawerOpen) {
            menuRefresh.setVisible(true);
        } else if (getCurrentTabName().equals(ACTIVITY_TAB) && !drawerOpen) {
            menuRefresh.setVisible(true);
        } else {
            menuRefresh.setVisible(false);
        }

        if (getCurrentTabName().equals(LIBRARY_TAB)) {
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
        
        if (currentSelectedItem.equals(UPLOAD_TASKS_TAB)) {
            menuUpload.setVisible(false);
            menuRefresh.setVisible(false);
            menuNewDir.setVisible(false);
            menuNewFile.setVisible(false);
        }
        
        if (getCurrentTabName().equals(STARRED_TAB)) {
            menuUpload.setVisible(false);
            menuNewDir.setVisible(false);
            menuNewFile.setVisible(false);
            if (drawerOpen) {
                menuRefresh.setVisible(false);
            } else {
                menuRefresh.setVisible(true);
            }
        }
        
        return true;
    }

    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
    	
        if (mDrawerToggle.onOptionsItemSelected(getMenuItem(item))) {
            return true;
        }
        switch (item.getItemId()) {
        case android.R.id.home:
            return true;
        case R.id.upload:
            pickFile();
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
                                	((ReposFragment)tabsFragment.getFragment(0)).refreshView(true);
                                }
                            } , password);

                        return true;
                    }
                }

                ((ReposFragment)tabsFragment.getFragment(0)).refreshView(true);
            } else if (getCurrentTabName().equals(ACTIVITY_TAB)) {
            	((ActivitiesFragment)tabsFragment.getFragment(1)).refreshView();
            } else if (getCurrentTabName().equals(STARRED_TAB)) {
                ((StarredFragment)tabsFragment.getFragment(2)).refreshView();
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

    private android.view.MenuItem getMenuItem(final MenuItem item) {
        return new android.view.MenuItem() {
           @Override
           public int getItemId() {
              return item.getItemId();
           }

           public boolean isEnabled() {
              return true;
           }

           @Override
           public boolean collapseActionView() {
              // TODO Auto-generated method stub
              return false;
           }

           @Override
           public boolean expandActionView() {
              // TODO Auto-generated method stub
              return false;
           }

           @Override
           public ActionProvider getActionProvider() {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public View getActionView() {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public char getAlphabeticShortcut() {
              // TODO Auto-generated method stub
              return 0;
           }

           @Override
           public int getGroupId() {
              // TODO Auto-generated method stub
              return 0;
           }

           @Override
           public Drawable getIcon() {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public Intent getIntent() {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public ContextMenuInfo getMenuInfo() {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public char getNumericShortcut() {
              // TODO Auto-generated method stub
              return 0;
           }

           @Override
           public int getOrder() {
              // TODO Auto-generated method stub
              return 0;
           }

           @Override
           public SubMenu getSubMenu() {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public CharSequence getTitle() {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public CharSequence getTitleCondensed() {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public boolean hasSubMenu() {
              // TODO Auto-generated method stub
              return false;
           }

           @Override
           public boolean isActionViewExpanded() {
              // TODO Auto-generated method stub
              return false;
           }

           @Override
           public boolean isCheckable() {
              // TODO Auto-generated method stub
              return false;
           }

           @Override
           public boolean isChecked() {
              // TODO Auto-generated method stub
              return false;
           }

           @Override
           public boolean isVisible() {
              // TODO Auto-generated method stub
              return false;
           }

           @Override
           public android.view.MenuItem setActionProvider(ActionProvider actionProvider) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setActionView(View view) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setActionView(int resId) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setAlphabeticShortcut(char alphaChar) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setCheckable(boolean checkable) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setChecked(boolean checked) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setEnabled(boolean enabled) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setIcon(Drawable icon) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setIcon(int iconRes) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setIntent(Intent intent) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setNumericShortcut(char numericChar) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setOnActionExpandListener(OnActionExpandListener listener) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setOnMenuItemClickListener(OnMenuItemClickListener menuItemClickListener) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setShortcut(char numericChar, char alphaChar) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public void setShowAsAction(int actionEnum) {
              // TODO Auto-generated method stub

           }

           @Override
           public android.view.MenuItem setShowAsActionFlags(int actionEnum) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setTitle(CharSequence title) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setTitle(int title) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setTitleCondensed(CharSequence title) {
              // TODO Auto-generated method stub
              return null;
           }

           @Override
           public android.view.MenuItem setVisible(boolean visible) {
              // TODO Auto-generated method stub
              return null;
           }
        };
     }
    
    @Override
    protected void onPostCreate(Bundle savedInstanceState) {
        super.onPostCreate(savedInstanceState);
        // Sync the toggle state after onRestoreInstanceState has occurred.
        mDrawerToggle.syncState();
    }

    @Override
    public void onConfigurationChanged(Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        // Pass any configuration change to the drawer toggls
        mDrawerToggle.onConfigurationChanged(newConfig);
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
                ReposFragment reposFragment = (ReposFragment)tabsFragment.getFragment(0);
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
                ReposFragment reposFragment = (ReposFragment)tabsFragment.getFragment(0);
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
                showToast(getString(R.string.added_to_upload_tasks));
                //showToast(getString(R.string.upload) + " " + Utils.fileNameFromPath(path));
                addUploadTask(navContext.getRepoID(),
                    navContext.getRepoName(), navContext.getDirPath(), path);
            }
        }

        if (requestCode == PICK_PHOTOS_VIDEOS_REQUEST) {
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

        if (currentSelectedItem == UPLOAD_TASKS_TAB) {
            navContext.setRepoID(null);
        }
        
        if (getCurrentTabName().equals(LIBRARY_TAB)) {
            if (navContext.inRepo()) {
                if (navContext.isRepoRoot()) {
                    navContext.setRepoID(null);
                } else {
                    String parentPath = Utils.getParentPath(navContext
                            .getDirPath());
                    navContext.setDir(parentPath, null);
                }
                ((ReposFragment)tabsFragment.getFragment(0)).refreshView();

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
                    public void onTaskSuccess() {
                        // TODO: generate a share link through SeafConnection and copy
                        // it to clipboard
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
                ReposFragment reposFragment = (ReposFragment)tabsFragment.getFragment(0);
                if (getCurrentTabName().equals(LIBRARY_TAB) && reposFragment != null) {
                    reposFragment.refreshView();
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }
    
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
        if (getCurrentTabName().equals(LIBRARY_TAB)
                && repoID.equals(navContext.getRepoID())
                && dir.equals(navContext.getDirPath())) {
        	((ReposFragment)tabsFragment.getFragment(0)).refreshView();
            String verb = getString(info.isUpdate ? R.string.updated : R.string.uploaded);
            showToast(verb + " " + Utils.fileNameFromPath(info.localFilePath));
        }

        if (uploadTasksFragment != null && uploadTasksFragment.isReady())
            uploadTasksFragment.onTaskFinished(info);
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
            if (getCurrentTabName().equals(LIBRARY_TAB)
                && info.repoID.equals(navContext.getRepoID())
                && Utils.getParentPath(info.path).equals(navContext.getDirPath())) {
            	((ReposFragment)tabsFragment.getFragment(0)).getAdapter().notifyChanged();
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
