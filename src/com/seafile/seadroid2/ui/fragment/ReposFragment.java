package com.seafile.seadroid2.ui.fragment;

import android.app.Activity;
import android.app.AlertDialog;
import android.content.DialogInterface;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.os.Handler;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.*;
import com.actionbarsherlock.app.SherlockListFragment;
import com.actionbarsherlock.view.ActionMode;
import com.seafile.seadroid2.*;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.*;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.ui.*;
import com.seafile.seadroid2.ui.activity.AccountsActivity;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.adapter.SeafItemAdapter;
import com.seafile.seadroid2.ui.dialog.SslConfirmDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.Utils;
import com.tjerkw.slideexpandable.library.SlideExpandableListAdapter;

import java.net.HttpURLConnection;
import java.util.List;
import java.util.Map;


public class ReposFragment extends SherlockListFragment
        implements ActionModeCallback.ActionModeOperationListener {

    private static final String DEBUG_TAG = "ReposFragment";
    
    private static final int REFRESH_ON_RESUME = 0;
    private static final int REFRESH_ON_PULL = 1;
    private static final int REFRESH_ON_CLICK = 2;
    private static final int REFRESH_ON_OVERFLOW_MENU = 3;
    private static int mRefreshType = -1;
    /** flag to stop refreshing when nav to other directory  */
    private static int mPullToRefreshStopRefreshing = 0;

    private SeafItemAdapter adapter;
    private BrowserActivity mActivity = null;
    private ActionMode mActionMode;
    private LinearLayout mTaskActionBar;
    private RelativeLayout deleteView;
    private RelativeLayout copyView;
    private RelativeLayout moveView;
    private RelativeLayout downloadView;
    private CopyMoveContext copyMoveContext;
    private MultipleOperationClickListener listener = new MultipleOperationClickListener();

    public static final int FILE_ACTION_EXPORT = 0;
    public static final int FILE_ACTION_COPY = 1;
    public static final int FILE_ACTION_MOVE = 2;
    public static final int FILE_ACTION_STAR = 3;

    private CustomActionSlideExpandableListView mListView;
    private ImageView mEmptyView;
    private View mProgressContainer;
    private View mListContainer;
    private TextView mErrorText;

    private boolean isTimerStarted;
    private final Handler mTimer = new Handler();

    private DataManager getDataManager() {
        return mActivity.getDataManager();
    }

    private NavContext getNavContext() {
        return mActivity.getNavContext();
    }

    public SeafItemAdapter getAdapter() {
        return adapter;
    }

    public ImageView getEmptyView() {
        return mEmptyView;
    }

    @Override
    public void selectItems() {
        if (adapter == null) return;

        adapter.selectAllItems();
        updateContextualActionBar();

    }

    @Override
    public void deselectItems() {
        if (adapter == null) return;

        adapter.deselectAllItems();
        updateContextualActionBar();

    }

    @Override
    public void onActionModeStarted() {
        if (adapter == null) return;

        adapter.setActionModeOn(true);
        adapter.notifyDataSetChanged();

        Animation bottomUp = AnimationUtils.loadAnimation(getActivity(),
                R.anim.bottom_up);
        mTaskActionBar.startAnimation(bottomUp);
        mTaskActionBar.setVisibility(View.VISIBLE);
    }

    @Override
    public void onActionModeDestroy() {
        if (adapter == null) return;

        adapter.setActionModeOn(false);
        adapter.deselectAllItems();
        Animation bottomDown = AnimationUtils.loadAnimation(mActivity,
                R.anim.bottom_down);
        mTaskActionBar.startAnimation(bottomDown);
        mTaskActionBar.setVisibility(View.GONE);

        // Here you can make any necessary updates to the activity when
        // the contextual action bar (CAB) is removed. By default, selected items are deselected/unchecked.
        mActionMode = null;

    }

    public interface OnFileSelectedListener {
        void onFileSelected(SeafDirent fileName);
    }

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        Log.d(DEBUG_TAG, "ReposFragment Attached");
        mActivity = (BrowserActivity)activity;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.repos_fragment, container, false);
        mListView = (CustomActionSlideExpandableListView) root.findViewById(android.R.id.list);
        mTaskActionBar = (LinearLayout) root.findViewById(R.id.multi_op_bottom_action_bar);
        deleteView = (RelativeLayout) root.findViewById(R.id.multi_op_delete_rl);
        copyView = (RelativeLayout) root.findViewById(R.id.multi_op_copy_rl);
        moveView = (RelativeLayout) root.findViewById(R.id.multi_op_move_rl);
        downloadView = (RelativeLayout) root.findViewById(R.id.multi_op_download_rl);
        deleteView.setOnClickListener(listener);
        copyView.setOnClickListener(listener);
        moveView.setOnClickListener(listener);
        downloadView.setOnClickListener(listener);
        mEmptyView = (ImageView) root.findViewById(R.id.empty);
        mListContainer =  root.findViewById(R.id.listContainer);
        mErrorText = (TextView)root.findViewById(R.id.error_message);
        mProgressContainer = root.findViewById(R.id.progressContainer);

        mListView.setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
            @Override
            public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
                startContextualActionMode();
                return true;
            }
        });

        // Set a listener to be invoked when the list should be refreshed.
        mListView.setOnRefreshListener(new CustomActionSlideExpandableListView.OnRefreshListener() {

            @Override
            public void onRefresh() {
                mRefreshType = REFRESH_ON_PULL;
                refreshView(true);

            }
        });

        return root;
    }

    /**
     * Start action mode for selecting and process multiple files/folders.
     * The contextual action mode is a system implementation of ActionMode
     * that focuses user interaction toward performing contextual actions.
     * When a user enables this mode by selecting an item,
     * a contextual action bar appears at the top of the screen
     * to present actions the user can perform on the currently selected item(s).
     *
     * While this mode is enabled,
     * the user can select multiple items (if you allow it), deselect items,
     * and continue to navigate within the activity (as much as you're willing to allow).
     *
     * The action mode is disabled and the contextual action bar disappears
     * when the user deselects all items, presses the BACK button, or selects the Done action on the left side of the bar.
     *
     * see http://developer.android.com/guide/topics/ui/menus.html#CAB
     */
    public void startContextualActionMode() {
        NavContext nav = getNavContext();
        if (!nav.inRepo()) return;

        if (mActionMode == null) {
            // start the actionMode
            mActionMode = mActivity.startActionMode(new ActionModeCallback(this));
        }

    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        Log.d(DEBUG_TAG, "ReposFragment onActivityCreated");
        adapter = new SeafItemAdapter(mActivity);

        mListView.setAdapter(
                new SlideExpandableListAdapter(
                        adapter,
                        R.id.expandable_toggle_button,
                        R.id.expandable)
        );

        // A more specific expandable listview in which the expandable area consist of some buttons
        // which are context actions for the item itself.
        // It handles event binding for those buttons
        // and allow for adding a listener that will be invoked if one of those buttons are pressed.
        mListView.setItemActionListener(new SlideExpandableClickListener(),
                R.id.action_share_ll,
                R.id.action_delete_ll,
                R.id.action_copy_ll,
                R.id.action_move_ll,
                R.id.action_rename_ll,
                R.id.action_update_ll,
                R.id.action_download_ll,
                R.id.action_more_ll);
    }

    /**
     * Implementation for callback to be invoked whenever an action is clicked in
     * the expandle area of the list item.
     */
    private class SlideExpandableClickListener implements CustomActionSlideExpandableListView.OnActionClickListener {

        @Override
        public void onClick(View itemView, View buttonview, int position) {
            // listen for click events for each list item.
            // the 'position' param will tell which list item is clicked
            SeafDirent dirent = (SeafDirent) adapter.getItem(position);
            NavContext nav = mActivity.getNavContext();
            String repoName = nav.getRepoName();
            String repoID = nav.getRepoID();
            String dir = nav.getDirPath();
            String path = Utils.pathJoin(dir, dirent.name);
            String filename = dirent.name;
            DataManager dataManager = mActivity.getDataManager();
            String localPath = dataManager.getLocalRepoFile(repoName, repoID, path).getPath();

            switch (buttonview.getId()) {
                case R.id.action_share_ll:
                    mListView.collapse();
                    mActivity.shareFile(repoID, path);
                    break;
                case R.id.action_delete_ll:
                    mListView.collapse();
                    mActivity.deleteFile(repoID, repoName, path);
                    break;
                case R.id.action_copy_ll:
                    mListView.collapse();
                    mActivity.copyFile(repoID, repoName, dir, filename, false);
                    break;
                case R.id.action_move_ll:
                    mListView.collapse();
                    mActivity.moveFile(repoID, repoName, dir, filename, false);
                    break;
                case R.id.action_rename_ll:
                    mListView.collapse();
                    mActivity.renameFile(repoID, repoName, path);
                    break;
                case R.id.action_update_ll:
                    mListView.collapse();
                    mActivity.addUpdateTask(repoID, repoName, dir, localPath);
                    break;
                case R.id.action_download_ll:
                    mListView.collapse();
                    if (dirent.isDir()) {
                        mActivity.downloadDir(dir, dirent.name, true);
                    } else {
                        mActivity.downloadFile(dir, dirent.name);
                    }
                    break;
                case R.id.action_more_ll:
                    mListView.collapse();
                    processMoreOptions(repoID, repoName, dir, filename, dirent, localPath);
                    break;
                default:
                    break;
            }
        }
    }

    private AlertDialog processMoreOptions(final String repoID,
                                           final String repoName,
                                           final String dir,
                                           final String filename,
                                           final SeafDirent dirent,
                                           final String localPath) {
        SeafileStyleDialogBuilder builder =
                new SeafileStyleDialogBuilder(getActivity()).
                        setTitle(getResources().getString(R.string.file_action_more_title)).
                        setItems(R.array.file_action_more_array,
                                new DialogInterface.OnClickListener() {
                                    @Override
                                    public void onClick(DialogInterface dialog, int which) {
                                        switch (which) {
                                            case FILE_ACTION_EXPORT:
                                                mActivity.exportFile(dirent.name);
                                                break;
                                            case FILE_ACTION_COPY:
                                                mActivity.copyFile(repoID, repoName, dir, filename, false);
                                                break;
                                            case FILE_ACTION_MOVE:
                                                mActivity.moveFile(repoID, repoName, dir, filename, false);
                                                break;
                                            case FILE_ACTION_STAR:
                                                mActivity.starFile(repoID, dir, filename);
                                                break;
                                            default:
                                                return;
                                        }
                                    }
                                });
        return builder.show();
    }

    class MultipleOperationClickListener implements View.OnClickListener {

        @Override
        public void onClick(View v) {
            NavContext nav = mActivity.getNavContext();
            String repoID = nav.getRepoID();
            String repoName = nav.getRepoName();
            String dirPath = nav.getDirPath();
            final List<SeafDirent> selectedDirents = adapter.getSelectedItemsValues();
            if (selectedDirents.size() == 0
                    || repoID == null
                    || dirPath == null) {
                ToastUtils.show(mActivity, R.string.action_mode_no_items_selected);
                return;
            }

            switch (v.getId()) {
                case R.id.multi_op_delete_rl:
                    mActivity.deleteFiles(repoID, dirPath, selectedDirents);
                    break;
                case R.id.multi_op_copy_rl:
                    mActivity.copyFiles(repoID, repoName, dirPath, selectedDirents);
                    break;
                case R.id.multi_op_move_rl:
                    mActivity.moveFiles(repoID, repoName, dirPath, selectedDirents);
                    break;
                case R.id.multi_op_download_rl:
                    mActivity.downloadFiles(repoID, repoName, dirPath, selectedDirents);
                    break;

            }
        }
    }

    @Override
    public void onStart() {
        // Log.d(DEBUG_TAG, "ReposFragment onStart");
        super.onStart();
    }

    @Override
    public void onStop() {
        // Log.d(DEBUG_TAG, "ReposFragment onStop");
        super.onStop();
        stopTimer();
    }

    @Override
    public void onResume() {
        super.onResume();
        // Log.d(DEBUG_TAG, "ReposFragment onResume");
        // refresh the view (loading data)
        refreshView();
        mRefreshType = REFRESH_ON_RESUME;
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
    }

    @Override
    public void onDetach() {
        mActivity = null;
        // Log.d(DEBUG_TAG, "ReposFragment detached");
        super.onDetach();
    }
    
    public void refresh() {
        mRefreshType = REFRESH_ON_OVERFLOW_MENU;
        refreshView(true);
    }

    public void refreshView() {
        refreshView(false);
    }

    public void refreshView(boolean forceRefresh) {
        if (mActivity == null)
            return;

        mErrorText.setVisibility(View.GONE);
        mListContainer.setVisibility(View.VISIBLE);

        NavContext navContext = getNavContext();
        if (navContext.inRepo()) {
            mActivity.enableUpButton();
            navToDirectory(forceRefresh);
        } else {
            mActivity.disableUpButton();
            navToReposView(forceRefresh);
        }
        mActivity.supportInvalidateOptionsMenu();
    }

    public void navToReposView(boolean forceRefresh) {
        //stopTimer();

        mPullToRefreshStopRefreshing ++;

        if (mPullToRefreshStopRefreshing >1) {
            mListView.onRefreshComplete();
            mPullToRefreshStopRefreshing = 0;
        }

        forceRefresh = forceRefresh || isReposRefreshTimeOut();
        if (!Utils.isNetworkOn() || !forceRefresh) {
            List<SeafRepo> repos = getDataManager().getReposFromCache();
            if (repos != null) {
                if (mRefreshType == REFRESH_ON_PULL) {
                    mListView.onRefreshComplete();
                    mPullToRefreshStopRefreshing = 0;
                }

                updateAdapterWithRepos(repos);
                return;
            }
        }

        ConcurrentAsyncTask.execute(new LoadTask(getDataManager()));
    }

    public void navToDirectory(boolean forceRefresh) {
        startTimer();

        mPullToRefreshStopRefreshing ++;

        if (mPullToRefreshStopRefreshing > 1) {
            mListView.onRefreshComplete();
            mPullToRefreshStopRefreshing = 0;
        }

        NavContext nav = getNavContext();
        DataManager dataManager = getDataManager();

        mActivity.enableUpButton();

        SeafRepo repo = getDataManager().getCachedRepoByID(nav.getRepoID());
        if (repo != null) {
            adapter.setEncryptedRepo(repo.encrypted);
            if (nav.getDirPath().equals(BrowserActivity.ACTIONBAR_PARENT_PATH)) {
                mActivity.setUpButtonTitle(nav.getRepoName());
            } else

                mActivity.setUpButtonTitle(nav.getDirPath().substring(
                        nav.getDirPath().lastIndexOf(BrowserActivity.ACTIONBAR_PARENT_PATH) + 1));
        }

        forceRefresh = forceRefresh || isDirentsRefreshTimeOut(nav.getRepoID(), nav.getDirPath());
        if (!Utils.isNetworkOn() || !forceRefresh) {
            List<SeafDirent> dirents = dataManager.getCachedDirents(
                    nav.getRepoID(), nav.getDirPath());
            if (dirents != null) {
                if (mRefreshType == REFRESH_ON_PULL) {
                    mListView.onRefreshComplete();
                    mPullToRefreshStopRefreshing = 0;
                }

                updateAdapterWithDirents(dirents);
                return;
            }
        }

        ConcurrentAsyncTask.execute(new LoadDirTask(getDataManager()),
                nav.getRepoName(),
                nav.getRepoID(),
                nav.getDirPath());
    }

    // refresh list by mTimer
    public void startTimer() {
        if (isTimerStarted)
            return;

        isTimerStarted = true;
        Log.d(DEBUG_TAG, "timer started");
        mTimer.postDelayed(new Runnable() {

            @Override
            public void run() {
                TransferService ts = mActivity.getTransferService();
                String repoID = getNavContext().getRepoID();
                String repoName = getNavContext().getRepoName();
                String currentDir = getNavContext().getDirPath();

                adapter.setDownloadTaskList(ts.getDownloadTaskInfosByPath(repoID, currentDir));

                // Log.d(DEBUG_TAG, "timer post refresh signal " + System.currentTimeMillis());
                mTimer.postDelayed(this, 1 * 1000);
            }
        }, 1 * 1000);
    }

    public void stopTimer() {
        Log.d(DEBUG_TAG, "timer stopped");
        mTimer.removeCallbacksAndMessages(null);
        isTimerStarted = false;
    }

    /**
     * calculate if repo refresh time is expired, the expiration is 10 mins 
     */
    private boolean isReposRefreshTimeOut() {
        if (getDataManager().isReposRefreshTimeout()) {
            return true;
        }

        return false;

    }

    /**
     * calculate if dirent refresh time is expired, the expiration is 10 mins 
     * 
     * @param repoID
     * @param path
     * @return true if refresh time expired, false otherwise
     */
    private boolean isDirentsRefreshTimeOut(String repoID, String path) {
        if (getDataManager().isDirentsRefreshTimeout(repoID, path)) {
            return true;
        }

        return false;
    }

    public void sortFiles(int type, int order) {
        adapter.sortFiles(type, order);
        adapter.notifyDataSetChanged();
        // persist sort settings
        SettingsManager.instance().saveSortFilesPref(type, order);
    }

    private void updateAdapterWithRepos(List<SeafRepo> repos) {
        adapter.clear();
        if (repos.size() > 0) {
            addReposToAdapter(repos);
            adapter.sortFiles(SettingsManager.instance().getSortFilesTypePref(),
                    SettingsManager.instance().getSortFilesOrderPref());
            adapter.notifyChanged();
            mListView.setVisibility(View.VISIBLE);
            mEmptyView.setVisibility(View.GONE);
        } else {
            mListView.setVisibility(View.GONE);
            mEmptyView.setVisibility(View.VISIBLE);
        }
        // Collapses the currently open view
        mListView.collapse();
    }

    private void updateAdapterWithDirents(final List<SeafDirent> dirents) {
        adapter.clear();
        if (dirents.size() > 0) {
            for (SeafDirent dirent : dirents) {
                adapter.add(dirent);
            }
            NavContext nav = getNavContext();
            final String repoName = nav.getRepoName();
            final String repoID = nav.getRepoID();
            final String dirPath = nav.getDirPath();

            adapter.sortFiles(SettingsManager.instance().getSortFilesTypePref(),
                    SettingsManager.instance().getSortFilesOrderPref());
            adapter.notifyChanged();
            mListView.setVisibility(View.VISIBLE);
            mEmptyView.setVisibility(View.GONE);
        } else {
            // Directory is empty
            mListView.setVisibility(View.GONE);
            mEmptyView.setVisibility(View.VISIBLE);
        }
        // Collapses the currently open view
        mListView.collapse();
    }

    /**
     *  update state of contextual action bar (CAB)
     */
    public void updateContextualActionBar() {

        if (mActionMode == null) {
            // there are some selected items, start the actionMode
            mActionMode = mActivity.startActionMode(new ActionModeCallback(this));
        } else {
            // Log.d(DEBUG_TAG, "mActionMode.setTitle " + adapter.getCheckedItemCount());
            mActionMode.setTitle(getResources().getQuantityString(
                    R.plurals.transfer_list_items_selected,
                    adapter.getCheckedItemCount(),
                    adapter.getCheckedItemCount()));
        }

    }

    @Override
    public void onListItemClick(final ListView l, final View v, final int position, final long id) {
        // handle action mode selections
        if (mActionMode != null) {
            // add or remove selection for current list item
            if (adapter == null) return;

            adapter.toggleSelection(position - 1);
            updateContextualActionBar();
            return;
        }

        SeafRepo repo = null;
        final NavContext nav = getNavContext();
        if (nav.inRepo()) {
            repo = getDataManager().getCachedRepoByID(nav.getRepoID());
            mActivity.setUpButtonTitle(repo.getName());
        } else {
            SeafItem item = adapter.getItem(position - 1);
            if (item instanceof SeafRepo) {
                repo = (SeafRepo)item;
            }
        }

        if (repo == null) {
            return;
        }

        if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
            String password = DataManager.getRepoPassword(repo.id);
            mActivity.showPasswordDialog(repo.name, repo.id,
                    new TaskDialog.TaskDialogListener() {
                @Override
                public void onTaskSuccess() {
                    onListItemClick(l, v, position, id);
                }
            }, password);

            return;
        }

        mRefreshType = REFRESH_ON_CLICK;
        if (nav.inRepo()) {
            if (adapter.getItem(position - 1) instanceof SeafDirent) {
                final SeafDirent dirent = (SeafDirent) adapter.getItem(position - 1);
                if (dirent.isDir()) {
                    String currentPath = nav.getDirPath();
                    String newPath = currentPath.endsWith("/") ?
                            currentPath + dirent.name : currentPath + "/" + dirent.name;
                    nav.setDir(newPath, dirent.id);
                    refreshView();
                    mActivity.setUpButtonTitle(dirent.name);
                } else {
                    mActivity.onFileSelected(dirent);
                }
            } else
                return;
        } else {
            nav.setRepoID(repo.id);
            nav.setRepoName(repo.getName());
            nav.setDir("/", repo.root);
            refreshView();
        }
    }

    private void addReposToAdapter(List<SeafRepo> repos) {
        if (repos == null)
            return;
        Map<String, List<SeafRepo>> map = Utils.groupRepos(repos);
        List<SeafRepo> personalRepos = map.get(Utils.PERSONAL_REPO);
        if (personalRepos != null) {
            SeafGroup personalGroup = new SeafGroup(mActivity.getResources().getString(R.string.personal));
            adapter.add(personalGroup);
            for (SeafRepo repo : personalRepos)
                adapter.add(repo);
        }

        List<SeafRepo> sharedRepos = map.get(Utils.SHARED_REPO);
        if (sharedRepos != null) {
            SeafGroup sharedGroup = new SeafGroup(mActivity.getResources().getString(R.string.shared));
            adapter.add(sharedGroup);
            for (SeafRepo repo : sharedRepos)
                adapter.add(repo);
        }

        for (Map.Entry<String, List<SeafRepo>> entry : map.entrySet()) {
            String key = entry.getKey();
            if (!key.equals(Utils.PERSONAL_REPO)
                    && !key.endsWith(Utils.SHARED_REPO)) {
                SeafGroup group = new SeafGroup(key);
                adapter.add(group);
                for (SeafRepo repo : entry.getValue()) {
                    adapter.add(repo);
                }
            }
        }
    }

    private class LoadTask extends AsyncTask<Void, Void, List<SeafRepo> > {
        SeafException err = null;
        DataManager dataManager;

        public LoadTask(DataManager dataManager) {
            this.dataManager = dataManager;
        }

        @Override
        protected void onPreExecute() {
            if (mRefreshType == REFRESH_ON_CLICK
                    || mRefreshType == REFRESH_ON_OVERFLOW_MENU
                    || mRefreshType == REFRESH_ON_RESUME) {
                showLoading(true);
            } else if (mRefreshType == REFRESH_ON_PULL) {

            }
        }
        
        @Override
        protected List<SeafRepo> doInBackground(Void... params) {
            try {
                return dataManager.getReposFromServer();
            } catch (SeafException e) {
                err = e;
                return null;
            }
        }

        private void displaySSLError() {
            if (mActivity == null)
                return;

            if (getNavContext().inRepo()) {
                return;
            }

            showError(R.string.ssl_error);
        }

        private void resend() {
            if (mActivity == null)
                return;

            if (getNavContext().inRepo()) {
                return;
            }
            ConcurrentAsyncTask.execute(new LoadTask(dataManager));
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafRepo> rs) {
            if (mActivity == null)
                // this occurs if user navigation to another activity
                return;

            if (mRefreshType == REFRESH_ON_CLICK
                    || mRefreshType == REFRESH_ON_OVERFLOW_MENU
                    || mRefreshType == REFRESH_ON_RESUME) {
                showLoading(false);
            } else if (mRefreshType == REFRESH_ON_PULL) {
                String lastUpdate = getDataManager().getLastPullToRefreshTime(DataManager.PULL_TO_REFRESH_LAST_TIME_FOR_REPOS_FRAGMENT);
                mListView.onRefreshComplete(lastUpdate);
                getDataManager().saveLastPullToRefreshTime(System.currentTimeMillis(), DataManager.PULL_TO_REFRESH_LAST_TIME_FOR_REPOS_FRAGMENT);
                mPullToRefreshStopRefreshing = 0;
            }

            if (getNavContext().inRepo()) {
                // this occurs if user already navigate into a repo
                return;
            }

            // Prompt the user to accept the ssl certificate
            if (err == SeafException.sslException) {
                SslConfirmDialog dialog = new SslConfirmDialog(dataManager.getAccount(),
                new SslConfirmDialog.Listener() {
                    @Override
                    public void onAccepted(boolean rememberChoice) {
                        Account account = dataManager.getAccount();
                        CertsManager.instance().saveCertForAccount(account, rememberChoice);
                        resend();
                    }

                    @Override
                    public void onRejected() {
                        displaySSLError();
                    }
                });
                dialog.show(getFragmentManager(), SslConfirmDialog.FRAGMENT_TAG);
                return;
            }

            if (err != null) {
                if (err.getCode() == HttpURLConnection.HTTP_UNAUTHORIZED) {
                    // Token expired, should login again
                    ToastUtils.show(mActivity, R.string.err_token_expired);
                    logoutWhenTokenExpired();
                } else {
                    Log.e(DEBUG_TAG, "failed to load repos: " + err.getMessage());
                    showError(R.string.error_when_load_repos);
                    return;
                }
            }

            if (rs != null) {
                getDataManager().setReposRefreshTimeStamp();
                updateAdapterWithRepos(rs);
            } else {
                Log.i(DEBUG_TAG, "failed to load repos");
                showError(R.string.error_when_load_repos);
            }
        }
    }

    private void showError(int strID) {
        showError(mActivity.getResources().getString(strID));
    }

    private void showError(String msg) {
        mProgressContainer.setVisibility(View.GONE);
        mListContainer.setVisibility(View.GONE);

        adapter.clear();
        adapter.notifyChanged();

        mErrorText.setText(msg);
        mErrorText.setVisibility(View.VISIBLE);
    }

    public void showLoading(boolean show) {
        mErrorText.setVisibility(View.GONE);
        if (show) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_in));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            mListContainer.setVisibility(View.INVISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_out));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(
                    mActivity, android.R.anim.fade_in));

            mProgressContainer.setVisibility(View.GONE);
            mListContainer.setVisibility(View.VISIBLE);
        }
    }

    private class LoadDirTask extends AsyncTask<String, Void, List<SeafDirent> > {

        SeafException err = null;
        String myRepoName;
        String myRepoID;
        String myPath;

        DataManager dataManager;

        public LoadDirTask(DataManager dataManager) {
            this.dataManager = dataManager;
        }
        
        @Override
        protected void onPreExecute() {
            if (mRefreshType == REFRESH_ON_CLICK
                    || mRefreshType == REFRESH_ON_OVERFLOW_MENU
                    || mRefreshType == REFRESH_ON_RESUME) {
                showLoading(true);
            } else if (mRefreshType == REFRESH_ON_PULL) {
                // mHeadProgress.setVisibility(ProgressBar.VISIBLE);
            }
        }
        
        @Override
        protected List<SeafDirent> doInBackground(String... params) {
            if (params.length != 3) {
                Log.d(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }

            myRepoName = params[0];
            myRepoID = params[1];
            myPath = params[2];
            try {
                return dataManager.getDirentsFromServer(myRepoID, myPath);
            } catch (SeafException e) {
                err = e;
                return null;
            }

        }

        private void resend() {
            if (mActivity == null)
                return;
            NavContext nav = mActivity.getNavContext();
            if (!myRepoID.equals(nav.getRepoID()) || !myPath.equals(nav.getDirPath())) {
                return;
            }

            ConcurrentAsyncTask.execute(new LoadDirTask(dataManager), myRepoName, myRepoID, myPath);
        }

        private void displaySSLError() {
            if (mActivity == null)
                return;

            NavContext nav = mActivity.getNavContext();
            if (!myRepoID.equals(nav.getRepoID()) || !myPath.equals(nav.getDirPath())) {
                return;
            }
            showError(R.string.ssl_error);
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafDirent> dirents) {
            if (mActivity == null)
                // this occurs if user navigation to another activity
                return;

            if (mRefreshType == REFRESH_ON_CLICK
                    || mRefreshType == REFRESH_ON_OVERFLOW_MENU
                    || mRefreshType == REFRESH_ON_RESUME) {
                showLoading(false);
            } else if (mRefreshType == REFRESH_ON_PULL) {
                String lastUpdate = getDataManager().getLastPullToRefreshTime(DataManager.PULL_TO_REFRESH_LAST_TIME_FOR_REPOS_FRAGMENT);
                mListView.onRefreshComplete(lastUpdate);
                getDataManager().saveLastPullToRefreshTime(System.currentTimeMillis(), DataManager.PULL_TO_REFRESH_LAST_TIME_FOR_REPOS_FRAGMENT);
                mPullToRefreshStopRefreshing = 0;
            }

            NavContext nav = mActivity.getNavContext();
            if (!myRepoID.equals(nav.getRepoID()) || !myPath.equals(nav.getDirPath())) {
                return;
            }

            if (err == SeafException.sslException) {
                SslConfirmDialog dialog = new SslConfirmDialog(dataManager.getAccount(),
                new SslConfirmDialog.Listener() {
                    @Override
                    public void onAccepted(boolean rememberChoice) {
                        Account account = dataManager.getAccount();
                        CertsManager.instance().saveCertForAccount(account, rememberChoice);
                        resend();
                    }

                    @Override
                    public void onRejected() {
                        displaySSLError();
                    }
                });
                dialog.show(getFragmentManager(), SslConfirmDialog.FRAGMENT_TAG);
                return;
            }

            if (err != null) {
                if (err.getCode() == SeafConnection.HTTP_STATUS_REPO_PASSWORD_REQUIRED) {
                    showPasswordDialog();
                } else if (err.getCode() == HttpURLConnection.HTTP_UNAUTHORIZED) {
                    // Token expired, should login again
                    ToastUtils.show(mActivity, R.string.err_token_expired);
                    logoutWhenTokenExpired();
                } else if (err.getCode() == HttpURLConnection.HTTP_NOT_FOUND) {
                    ToastUtils.show(mActivity, String.format("The folder \"%s\" was deleted", myPath));
                } else {
                    Log.d(DEBUG_TAG, "failed to load dirents: " + err.getMessage());
                    err.printStackTrace();
                    showError(R.string.error_when_load_dirents);
                }
                return;
            }

            if (dirents == null) {
                showError(R.string.error_when_load_dirents);
                Log.i(DEBUG_TAG, "failed to load dir");
                return;
            }
            getDataManager().setDirsRefreshTimeStamp(myRepoID, myPath);
            updateAdapterWithDirents(dirents);
        }
    }

    /**
     * Token expired, clear current authorized info and redirect user to login page
     */
    private void logoutWhenTokenExpired() {
        AccountManager accountMgr = new AccountManager(mActivity);
        // sign out current account
        accountMgr.signOutCurrentAccount();

        // then redirect to AccountsActivity
        Intent intent = new Intent(mActivity, AccountsActivity.class);
        mActivity.startActivity(intent);

        // finish current Activity
        mActivity.finish();
    }

    private void showPasswordDialog() {
        NavContext nav = mActivity.getNavContext();
        String repoName = nav.getRepoName();
        String repoID = nav.getRepoID();

        mActivity.showPasswordDialog(repoName, repoID, new TaskDialog.TaskDialogListener() {
            @Override
            public void onTaskSuccess() {
                refreshView();
            }
        });
    }
}
