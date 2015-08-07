package com.seafile.seadroid2.ui.activity;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.IBinder;
import android.util.Log;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.*;
import com.actionbarsherlock.app.ActionBar;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.actionbarsherlock.view.ActionMode;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.notification.DownloadNotificationProvider;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.ActionModeCallback;
import com.seafile.seadroid2.ui.ActionModeCallback.ActionModeOperationListener;
import com.seafile.seadroid2.ui.CopyMoveContext;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.adapter.MultipleOperationAdapter;
import com.seafile.seadroid2.ui.dialog.CopyMoveDialog;
import com.seafile.seadroid2.ui.dialog.DeleteFileDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

/**
 * Activity for file (folders) multiple selections and operations
 */
public class MultipleOperationActivity extends SherlockFragmentActivity
        implements ActionModeOperationListener {
    public static final String DEBUG_TAG = "MultiOperationActivity";

    public static final String MULTI_OPERATION_REPOID = "operation_repo_id";
    public static final String MULTI_OPERATION_REPONAME = "operation_repo_name";
    public static final String MULTI_OPERATION_DIR = "operation_dir";
    public static final String MULTI_OPERATION_ACCOUNT = "operation_data_account";
    public static final String MULTI_OPERATION_BUNDLE = "operation_data_bundle";
    public static final int CHOOSE_COPY_MOVE_DEST_REQUEST = 1;
    private MultipleOperationClickListener listener = new MultipleOperationClickListener();
    private MultipleOperationAdapter adapter;
    private ListView mFileList;
    private TextView mEmptyView;
    private View mProgressBarContainer;
    private LinearLayout mTaskActionBar;
    private RelativeLayout deleteView;
    private RelativeLayout copyView;
    private RelativeLayout moveView;
    private RelativeLayout downloadView;
    private CopyMoveContext copyMoveContext;
    private Intent copyMoveIntent;
    private Account mAccount;
    private DataManager manager;
    private TransferService txService;
    private List<SeafDirent> dirents;
    private String repoID;
    private String repoName;
    private String dirPath;
    private ActionMode mActionMode;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.multi_op_layout);

        mFileList = (ListView) findViewById(R.id.multi_op_list);
        mTaskActionBar = (LinearLayout) findViewById(R.id.multi_op_bottom_action_bar);
        mProgressBarContainer = findViewById(R.id.multi_op_progressContainer);
        mEmptyView = (TextView) findViewById(R.id.multi_op_empty);
        deleteView = (RelativeLayout) findViewById(R.id.multi_op_delete_rl);
        copyView = (RelativeLayout) findViewById(R.id.multi_op_copy_rl);
        moveView = (RelativeLayout) findViewById(R.id.multi_op_move_rl);
        downloadView = (RelativeLayout) findViewById(R.id.multi_op_download_rl);
        deleteView.setOnClickListener(listener);
        copyView.setOnClickListener(listener);
        moveView.setOnClickListener(listener);
        downloadView.setOnClickListener(listener);

        Intent intent = getIntent();
        repoID = intent.getStringExtra(MULTI_OPERATION_REPOID);
        repoName = intent.getStringExtra(MULTI_OPERATION_REPONAME);
        dirPath = intent.getStringExtra(MULTI_OPERATION_DIR);
        mAccount = intent.getBundleExtra(MULTI_OPERATION_BUNDLE).getParcelable(MULTI_OPERATION_ACCOUNT);
        manager = new DataManager(mAccount);
        dirents = manager.getCachedDirents(repoID, dirPath);
        adapter = new MultipleOperationAdapter(this, dirents);
        adapter.sortFiles(SettingsManager.instance().getSortFilesTypePref(), SettingsManager.instance().getSortFilesOrderPref());
        mFileList.setAdapter(adapter);
        mFileList.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                if (mActionMode == null) {
                    // no items selected, so perform item click actions
                } else
                    // add or remove selection for current list item
                    onListItemChecked(position);
            }
        });

        ActionBar actionBar = getSupportActionBar();
        actionBar.setDisplayShowTitleEnabled(true);
        actionBar.setDisplayHomeAsUpEnabled(false);

        mActionMode = startActionMode(new ActionModeCallback(this));

        Intent txIntent = new Intent(this, TransferService.class);
        startService(txIntent);
        // Log.d(DEBUG_TAG, "start TransferService");

        // bind transfer service
        Intent bIntent = new Intent(this, TransferService.class);
        bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
        // Log.d(DEBUG_TAG, "try bind TransferService");

    }

    @Override
    protected void onDestroy() {
        // Log.d(DEBUG_TAG, "onDestroy is called");
        if (txService != null) {
            unbindService(mConnection);
            txService = null;
        }
        super.onDestroy();
    }

    public void onListItemChecked(int position) {
        if (adapter == null)
            return;

        adapter.toggleSelection(position);

        /*Log.d(DEBUG_TAG, "itemsChecked "
                + itemsChecked
                + " getCheckedItemCount "
                + adapter.getCheckedItemCount());*/

        if (mActionMode == null) {
            // there are some selected items, start the actionMode
            mActionMode = startActionMode(new ActionModeCallback(this));
        } else {
            // Log.d(DEBUG_TAG, "mActionMode.setTitle " + adapter.getCheckedItemCount());
            mActionMode.setTitle(getResources().getQuantityString(
                    R.plurals.transfer_list_items_selected,
                    adapter.getCheckedItemCount(),
                    adapter.getCheckedItemCount()));
        }
    }

    /**
     *  update state of contextual action bar (CAB)
     */
    public void updateContextualActionBar() {
        /*Log.d(DEBUG_TAG, "itemsChecked "
                + itemsChecked
                + " getCheckedItemCount "
                + adapter.getCheckedItemCount());*/

        if (mActionMode == null) {
            // there are some selected items, start the actionMode
            mActionMode = startActionMode(new ActionModeCallback(this));
        } else {
            // Log.d(DEBUG_TAG, "mActionMode.setTitle " + adapter.getCheckedItemCount());
            mActionMode.setTitle(getResources().getQuantityString(
                    R.plurals.transfer_list_items_selected,
                    adapter.getCheckedItemCount(),
                    adapter.getCheckedItemCount()));
        }

    }

    /**
     * select all items
     */
    @Override
    public void selectItems() {
        if (adapter == null)
            return;

        adapter.selectAllItems();
        updateContextualActionBar();

    }

    /**
     * deselect all items
     */
    @Override
    public void deselectItems() {
        if (adapter == null)
            return;

        adapter.deselectAllItems();
        updateContextualActionBar();
    }

    @Override
    public void onActionModeStarted() {

    }

    @Override
    public void onActionModeDestroy() {
        if (adapter == null)
            return;

        adapter.deselectAllItems();
        //adapter.actionModeOff();
        Animation bottomDown = AnimationUtils.loadAnimation(MultipleOperationActivity.this,
                R.anim.bottom_down);
        mTaskActionBar.startAnimation(bottomDown);
        mTaskActionBar.setVisibility(View.GONE);

        // Here you can make any necessary updates to the activity when
        // the contextual action bar (CAB) is removed. By default, selected items are deselected/unchecked.
        mActionMode = null;

        // finish activity
        finish();
    }

    public void onItemSelected() {
        // update contextual action bar (CAB) title
        updateContextualActionBar();
    }

    class MultipleOperationClickListener implements View.OnClickListener {

        @Override
        public void onClick(View v) {
            final List<SeafDirent> selectedDirents = adapter.getSelectedItemsValues();
            if (selectedDirents.size() == 0
                    || repoID == null
                    || dirPath == null) {
                ToastUtils.show(MultipleOperationActivity.this, R.string.action_mode_no_items_selected);
                return;
            }

            switch (v.getId()) {
                case R.id.multi_op_delete_rl:
                    deleteFile(repoID, dirPath, selectedDirents);
                    break;
                case R.id.multi_op_copy_rl:
                    copyFile(repoID, repoName, dirPath, selectedDirents);
                    break;
                case R.id.multi_op_move_rl:
                    moveFile(repoID, repoName, dirPath, selectedDirents);
                    break;
                case R.id.multi_op_download_rl:
                    downloadFiles(repoID, repoName, dirPath, selectedDirents);
                    break;

            }
        }
    }

    public void deleteFile(final String repoID, String path, List<SeafDirent> dirents) {
        final DeleteFileDialog dialog = new DeleteFileDialog();
        dialog.init(repoID, path, dirents, mAccount);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                ToastUtils.show(MultipleOperationActivity.this, R.string.delete_successful);
                if (manager != null) {
                    List<SeafDirent> cachedDirents = manager.getCachedDirents(repoID, dirPath);
                    adapter.setItems(cachedDirents);
                    adapter.notifyDataSetChanged();
                    if (cachedDirents.size() == 0)
                        mEmptyView.setVisibility(View.VISIBLE);
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    public void copyFile(String srcRepoId, String srcRepoName, String srcDir, List<SeafDirent> dirents) {
        chooseCopyMoveDest(srcRepoId, srcRepoName, srcDir, dirents, CopyMoveContext.OP.COPY);
    }

    public void moveFile(String srcRepoId, String srcRepoName, String srcDir, List<SeafDirent> dirents) {
        chooseCopyMoveDest(srcRepoId, srcRepoName, srcDir, dirents, CopyMoveContext.OP.MOVE);
    }

    private void chooseCopyMoveDest(String repoID, String repoName, String dirPath, List<SeafDirent> dirents, CopyMoveContext.OP op) {
        copyMoveContext = new CopyMoveContext(repoID, repoName, dirPath, dirents, op);
        Intent intent = new Intent(this, SeafilePathChooserActivity.class);
        intent.putExtra(SeafilePathChooserActivity.DATA_ACCOUNT, mAccount);
        SeafRepo repo = manager.getCachedRepoByID(repoID);
        if (repo.encrypted) {
            intent.putExtra(SeafilePathChooserActivity.ENCRYPTED_REPO_ID, repoID);
        }
        startActivityForResult(intent, CHOOSE_COPY_MOVE_DEST_REQUEST);
        return;
    }

    private void doCopyMove() {
        if (!copyMoveContext.checkCopyMoveToSubfolder()) {
            ToastUtils.show(this, copyMoveContext.isCopy()
                    ? R.string.cannot_copy_folder_to_subfolder
                    : R.string.cannot_move_folder_to_subfolder);
            return;
        }
        final CopyMoveDialog dialog = new CopyMoveDialog();
        dialog.init(mAccount, copyMoveContext);
        dialog.setTaskDialogLisenter(new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                ToastUtils.show(MultipleOperationActivity.this, copyMoveContext.isCopy()
                        ? R.string.copied_successfully
                        : R.string.moved_successfully);
                if (copyMoveContext.isMove()
                        || copyMoveContext.isCopy()) {
                    List<SeafDirent> cachedDirents = manager.getCachedDirents(repoID, dirPath);
                    // refresh view
                    if (manager != null) {
                        adapter.setItems(cachedDirents);
                        adapter.notifyDataSetChanged();
                    }

                    if (cachedDirents.size() == 0)
                        mEmptyView.setVisibility(View.VISIBLE);
                }
            }
        });
        dialog.show(getSupportFragmentManager(), "DialogFragment");
    }

    /**
     * Add selected files (folders) to downloading queue,
     * folders with subfolder will be downloaded recursively.
     *
     * @param repoID
     * @param repoName
     * @param dirPath
     * @param dirents
     */
    private void downloadFiles(String repoID, String repoName, String dirPath, List<SeafDirent> dirents) {
        if (!Utils.isNetworkOn()) {
            ToastUtils.show(this, R.string.network_down);
            return;
        }

        DownloadFilesTask task = new DownloadFilesTask(repoID, repoName, dirPath, dirents);
        ConcurrentAsyncTask.execute(task);
    }

    /**
     * Task for asynchronously downloading selected files (folders),
     * files wont be added to downloading queue if they have already been cached locally.
     */
    class DownloadFilesTask extends AsyncTask<Void, Void, Void> {
        private String repoID, repoName, dirPath;
        private List<SeafDirent> dirents;
        private SeafException err;
        private int fileCount;

        public DownloadFilesTask(String repoID, String repoName, String dirPath, List<SeafDirent> dirents) {
            this.repoID = repoID;
            this.repoName = repoName;
            this.dirPath = dirPath;
            this.dirents = dirents;
        }

        @Override
        protected void onPreExecute() {
            showLoading(true);
        }

        @Override
        protected Void doInBackground(Void... params) {
            ArrayList<String> dirPaths = Lists.newArrayList(dirPath);
            for (int i = 0; i < dirPaths.size(); i++) {
                if (i > 0) {
                    try {
                        dirents = manager.getDirentsFromServer(repoID, dirPaths.get(i));
                    } catch (SeafException e) {
                        err = e;
                        Log.e(DEBUG_TAG, e.getMessage() + e.getCode());
                    }
                }

                if (dirents == null)
                    continue;

                for (SeafDirent seafDirent : dirents) {
                    if (seafDirent.isDir()) {
                        // download files recursively
                        dirPaths.add(Utils.pathJoin(dirPaths.get(i), seafDirent.name));
                    } else {
                        File localCachedFile = manager.getLocalCachedFile(repoName,
                                repoID,
                                Utils.pathJoin(dirPaths.get(i),
                                        seafDirent.name),
                                seafDirent.id);
                        if (localCachedFile != null) {
                            continue;
                        }

                        // txService maybe null if layout orientation has changed
                        // e.g. landscape and portrait switch
                        if (txService == null)
                            return null;

                        txService.addTaskToDownloadQue(mAccount,
                                repoName,
                                repoID,
                                Utils.pathJoin(dirPaths.get(i),
                                        seafDirent.name));
                        fileCount++;
                    }

                }
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void aVoid) {
            // update ui
            showLoading(false);

            if (err != null) {
                ToastUtils.show(MultipleOperationActivity.this, R.string.transfer_list_network_error);
                return;
            }

            if (fileCount == 0)
                ToastUtils.show(MultipleOperationActivity.this, R.string.transfer_download_no_task);
            else {
                ToastUtils.show(MultipleOperationActivity.this, getResources().getQuantityString(R.plurals.transfer_download_started, fileCount, fileCount));
                if (!txService.hasDownloadNotifProvider()) {
                    DownloadNotificationProvider provider = new DownloadNotificationProvider(txService.getDownloadTaskManager(),
                            txService);
                    txService.saveDownloadNotifProvider(provider);
                }

            }

            finish();
        }
    }

    private void showLoading(boolean show) {
        if (show) {
            mProgressBarContainer.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_in));
            mProgressBarContainer.setVisibility(View.VISIBLE);
        } else {
            mProgressBarContainer.startAnimation(AnimationUtils.loadAnimation(
                    this, android.R.anim.fade_out));
            mProgressBarContainer.setVisibility(View.GONE);
        }
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        switch (requestCode) {
            case CHOOSE_COPY_MOVE_DEST_REQUEST:
                if (resultCode == RESULT_OK) {
                    if (!Utils.isNetworkOn()) {
                        ToastUtils.show(this, R.string.network_down);
                        return;
                    }
                    copyMoveIntent = data;
                }
                break;
            default:
                break;
        }
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


    private ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            txService = binder.getService();
            // Log.d(DEBUG_TAG, "TransferService was bound");
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            txService = null;
        }
    };

}
