package com.seafile.seadroid2.ui.fragment;

import android.app.Activity;
import android.os.Bundle;

import androidx.fragment.app.ListFragment;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;
import androidx.appcompat.view.ActionMode;

import android.view.LayoutInflater;
import android.view.Menu;
import android.view.MenuInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.AdapterView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SeafStarredFile;
import com.seafile.seadroid2.listener.OnCallback;
import com.seafile.seadroid2.task.StarItemsTask;
import com.seafile.seadroid2.ui.WidgetUtils;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.adapter.StarredItemAdapter;
import com.seafile.seadroid2.ui.dialog.PasswordDialog;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.SupportAsyncTask;
import com.seafile.seadroid2.util.Utils;

import java.util.List;

public class StarredFragment extends ListFragment {
    private StarredItemAdapter adapter;
    private BrowserActivity mActivity = null;

    private SwipeRefreshLayout refreshLayout;
    private ListView mListView;
    private TextView mNoStarredView;
    private View mProgressContainer;
    private View mListContainer;
    private TextView mErrorText;
    private ActionMode mActionMode;
    /*private LinearLayout mTaskActionBar;
    private RelativeLayout mUnstarFiles;*/
    private static final int REFRESH_ON_RESUME = 0;
    private static final int REFRESH_ON_PULL = 1;
    private static final int REFRESH_ON_OVERFLOW_MENU = 2;
    private static int mRefreshType = -1;
    public static final String PASSWORD_DIALOG_STARREDFRAGMENT_TAG = "password_starredfragment";

    private DataManager getDataManager() {
        return mActivity.getDataManager();
    }

    public StarredItemAdapter getAdapter() {
        return adapter;
    }

    public interface OnStarredFileSelectedListener {
        void onStarredFileSelected(SeafStarredFile starredFile);
    }

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        mActivity = (BrowserActivity) activity;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.starred_fragment, container, false);
        refreshLayout = (SwipeRefreshLayout) root.findViewById(R.id.swiperefresh);
        mListView = (ListView) root.findViewById(android.R.id.list);
        mNoStarredView = (TextView) root.findViewById(android.R.id.empty);
        mListContainer = root.findViewById(R.id.listContainer);
        mErrorText = (TextView) root.findViewById(R.id.error_message);
        mProgressContainer = root.findViewById(R.id.progressContainer);

        mListView.setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
            @Override
            public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
                startContextualActionMode(position);
                return true;
            }
        });

        refreshLayout.setColorSchemeResources(R.color.fancy_orange);
        // Set a listener to be invoked when the list should be refreshed.
        refreshLayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                mRefreshType = REFRESH_ON_PULL;
                refreshView();
            }
        });

        return root;
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        adapter = new StarredItemAdapter(mActivity);
        setListAdapter(adapter);

        getListView().setChoiceMode(ListView.CHOICE_MODE_SINGLE);
    }

    @Override
    public void onStart() {
        super.onStart();
    }

    @Override
    public void onStop() {
        super.onStop();
    }

    @Override
    public void onResume() {
        super.onResume();
        mRefreshType = REFRESH_ON_RESUME;
        refreshView();
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
    }

    @Override
    public void onDetach() {
        mActivity = null;
        super.onDetach();
    }

    public void refresh() {
        mRefreshType = REFRESH_ON_OVERFLOW_MENU;
        refreshView();
    }

    public void refreshView() {

        if (mActivity == null)
            return;

        mErrorText.setVisibility(View.GONE);
        mListContainer.setVisibility(View.VISIBLE);
        if (!Utils.isNetworkOn()) {
            refreshLayout.setRefreshing(false);
            Toast.makeText(mActivity, getString(R.string.network_down), Toast.LENGTH_SHORT).show();
        }
        List<SeafStarredFile> starredFiles = getDataManager().getCachedStarredFiles();
        boolean refreshTimeout = getDataManager().isStarredFilesRefreshTimeout();
        if (mRefreshType == REFRESH_ON_PULL || mRefreshType == REFRESH_ON_OVERFLOW_MENU
                || starredFiles == null || refreshTimeout) {
            ConcurrentAsyncTask.execute(new LoadStarredFilesTask(getDataManager()));
        } else {
            updateAdapterWithStarredFiles(starredFiles);
        }
        //mActivity.supportInvalidateOptionsMenu();
    }

    private void showError(String msg) {
        mProgressContainer.setVisibility(View.GONE);
        mListContainer.setVisibility(View.GONE);

        adapter.clear();
        adapter.notifyChanged();

        mErrorText.setText(msg);
        mErrorText.setVisibility(View.VISIBLE);
        mErrorText.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                refreshView();
            }
        });
    }

    private void showLoading(boolean show) {
        mErrorText.setVisibility(View.GONE);
        if (show) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(mActivity, android.R.anim.fade_in));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(mActivity, android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            mListContainer.setVisibility(View.INVISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(mActivity, android.R.anim.fade_out));
            mListContainer.startAnimation(AnimationUtils.loadAnimation(mActivity, android.R.anim.fade_in));

            mProgressContainer.setVisibility(View.GONE);
            mListContainer.setVisibility(View.VISIBLE);
        }
    }

    private void updateAdapterWithStarredFiles(List<SeafStarredFile> starredFiles) {
        adapter.clear();
        if (starredFiles.size() > 0) {
            for (SeafStarredFile starred : starredFiles) {
                adapter.add(starred);
            }
            adapter.notifyChanged();
            mListView.setVisibility(View.VISIBLE);
            mNoStarredView.setVisibility(View.GONE);
        } else {
            mListView.setVisibility(View.GONE);
            mNoStarredView.setVisibility(View.VISIBLE);
        }
    }

    @Override
    public void onListItemClick(final ListView l, final View v, final int position, final long id) {
        // handle action mode selections
        if (mActionMode != null) {
            // add or remove selection for current list item
            if (adapter == null) return;

            adapter.toggleSelection(position);
            updateContextualActionBar();
            return;
        }

        final SeafStarredFile starredFile = (SeafStarredFile) adapter.getItem(position);
        if (starredFile.isDir()) {
            onStarredDirSelected(starredFile);
        } else {
            mActivity.onStarredFileSelected(starredFile);
        }
    }

    public void showPasswordDialog(String repoName, String repoID, TaskDialog.TaskDialogListener listener, String password) {
        PasswordDialog passwordDialog = new PasswordDialog();
        passwordDialog.setRepo(repoName, repoID, mActivity.getAccount());
        if (password != null) {
            passwordDialog.setPassword(password);
        }
        passwordDialog.setTaskDialogLisenter(listener);
        passwordDialog.show(mActivity.getSupportFragmentManager(), PASSWORD_DIALOG_STARREDFRAGMENT_TAG);
    }

    public void onStarredDirSelected(SeafStarredFile starDirent) {
        final String repoID = starDirent.getRepoID();
        final SeafRepo repo = getDataManager().getCachedRepoByID(repoID);
        final String repoName = repo.getRepoName();
        final String filePath = starDirent.getPath();

        if (starDirent.isRepoEncrypted()) {
            if (repo.encrypted && !getDataManager().getRepoPasswordSet(repo.repo_id)) {
                String password = getDataManager().getRepoPassword(repo.repo_id);
                showPasswordDialog(repo.repo_name, repo.repo_id,
                        new TaskDialog.TaskDialogListener() {
                            @Override
                            public void onTaskSuccess() {
                                WidgetUtils.showStarredRepo(mActivity, repoID, repoName, filePath);
                            }
                        }, password);

            } else {
                WidgetUtils.showStarredRepo(mActivity, repoID, repoName, filePath);
            }
        } else {
            WidgetUtils.showStarredRepo(mActivity, repoID, repoName, filePath);
        }
    }

    private void unStarFiles(List<SeafStarredFile> starredFiles) {
        for (SeafStarredFile seafStarredFile : starredFiles) {
            doUnStarFile(seafStarredFile.getRepoID(), seafStarredFile.getPath());
        }
    }

    private void doUnStarFile(String repoID, String path) {
        if (!Utils.isNetworkOn()) {
            mActivity.showShortToast(mActivity, R.string.network_down);
            return;
        }

        StarItemsTask task = new StarItemsTask(mActivity, repoID, path, true);
        task.setOnCallback(new OnCallback() {
            @Override
            public void onFailed() {

            }

            @Override
            public void onSuccess() {
                mRefreshType = REFRESH_ON_RESUME;
                refreshView();
                adapter.deselectAllItems();
                mActionMode.setTitle(getResources().
                        getQuantityString(R.plurals.transfer_list_items_selected, 0, 0));
            }
        });
        ConcurrentAsyncTask.execute(task);
    }

    public void doStarFile(String repoID, String path, String filename) {
        if (!Utils.isNetworkOn()) {
            mActivity.showShortToast(mActivity, R.string.network_down);
            return;
        }

        String p = Utils.pathJoin(path, filename);

        StarItemsTask task = new StarItemsTask(mActivity, repoID, p, false);
        ConcurrentAsyncTask.execute(task);
    }

    private class LoadStarredFilesTask extends SupportAsyncTask<BrowserActivity, Void, Void, List<SeafStarredFile>> {

        SeafException err = null;

        DataManager dataManager;

        public LoadStarredFilesTask(DataManager dataManager) {
            super(mActivity);
            this.dataManager = dataManager;
        }

        @Override
        protected void onPreExecute() {
            if (mRefreshType != REFRESH_ON_PULL)
                showLoading(true);
        }

        @Override
        protected List<SeafStarredFile> doInBackground(Void... params) {

            try {
                return dataManager.getStarredFiles();
            } catch (SeafException e) {
                err = e;
                return null;
            }

        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafStarredFile> starredFiles) {
            if (getContextParam() == null)
                // this occurs if user navigation to another activity
                return;

            if (mRefreshType == REFRESH_ON_RESUME || mRefreshType == REFRESH_ON_OVERFLOW_MENU) {
                showLoading(false);
            } else if (mRefreshType == REFRESH_ON_PULL) {
                // Call onRefreshComplete when the list has been refreshed.
                //mListView.onRefreshComplete(getDataManager().getLastPullToRefreshTime(DataManager.PULL_TO_REFRESH_LAST_TIME_FOR_STARRED_FRAGMENT));
                getDataManager().saveLastPullToRefreshTime(System.currentTimeMillis(), DataManager.PULL_TO_REFRESH_LAST_TIME_FOR_STARRED_FRAGMENT);
                refreshLayout.setRefreshing(false);
            }


            if (err != null) {
                if (err == SeafException.remoteWipedException) {
                    if (getContextParam() != null) {
                        getContextParam().completeRemoteWipe();
                    }
                } else {
                    showError(getString(R.string.error_when_load_starred));
                    return;
                }
            }

            if (starredFiles == null) {
                showError(getString(R.string.error_when_load_starred));
                return;
            }

            updateAdapterWithStarredFiles(starredFiles);
        }
    }

    /**
     * Start action mode for selecting and process multiple files/folders.
     * The contextual action mode is a system implementation of ActionMode
     * that focuses user interaction toward performing contextual actions.
     * When a user enables this mode by selecting an item,
     * a contextual action bar appears at the top of the screen
     * to present actions the user can perform on the currently selected item(s).
     * <p>
     * While this mode is enabled,
     * the user can select multiple items (if you allow it), deselect items,
     * and continue to navigate within the activity (as much as you're willing to allow).
     * <p>
     * The action mode is disabled and the contextual action bar disappears
     * when the user deselects all items, presses the BACK button, or selects the Done action on the left side of the bar.
     * <p>
     * see http://developer.android.com/guide/topics/ui/menus.html#CAB
     */
    public void startContextualActionMode(int position) {
        startContextualActionMode();

        if (adapter == null) return;

        adapter.toggleSelection(position);
        updateContextualActionBar();
    }

    public void startContextualActionMode() {
        if (mActionMode == null) {
            // start the actionMode
            mActionMode = mActivity.startSupportActionMode(new ActionModeCallback());
        }

    }

    /**
     * update state of contextual action bar (CAB)
     */
    public void updateContextualActionBar() {

        if (mActionMode == null) {
            // there are some selected items, start the actionMode
            mActionMode = mActivity.startSupportActionMode(new ActionModeCallback());
        } else {
            // Log.d(DEBUG_TAG, "mActionMode.setTitle " + adapter.getCheckedItemCount());
            mActionMode.setTitle(getResources().getQuantityString(
                    R.plurals.transfer_list_items_selected,
                    adapter.getCheckedItemCount(),
                    adapter.getCheckedItemCount()));
        }

    }

    /**
     * Represents a contextual mode of the user interface.
     * Action modes can be used to provide alternative interaction modes and replace parts of the normal UI until finished.
     * A Callback configures and handles events raised by a user's interaction with an action mode.
     */
    class ActionModeCallback implements ActionMode.Callback {
        private boolean allItemsSelected;

        public ActionModeCallback() {
        }

        @Override
        public boolean onCreateActionMode(ActionMode mode, Menu menu) {
            // Inflate the menu for the contextual action bar (CAB)
            MenuInflater inflater = mode.getMenuInflater();
            inflater.inflate(R.menu.starred_fragment_menu, menu);
            if (adapter == null) return true;

            adapter.setActionModeOn(true);
            adapter.notifyDataSetChanged();

            return true;
        }

        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            // Here you can perform updates to the contextual action bar (CAB) due to
            // an invalidate() request
            return false;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            // Respond to clicks on the actions in the contextual action bar (CAB)
            final List<SeafStarredFile> starredFiles = adapter.getSelectedItemsValues();
            if (CollectionUtils.isEmpty(starredFiles)) {
                if (item.getItemId() != R.id.action_mode_select_all) {
                    mActivity.showShortToast(mActivity, R.string.action_mode_no_items_selected);
                    return true;
                }
            }

            switch (item.getItemId()) {
                case R.id.action_mode_select_all:
                    if (!allItemsSelected) {
                        if (adapter == null) return true;

                        adapter.selectAllItems();
                        updateContextualActionBar();
                    } else {
                        if (adapter == null) return true;

                        adapter.deselectAllItems();
                        updateContextualActionBar();
                    }

                    allItemsSelected = !allItemsSelected;
                    break;
                case R.id.action_mode_delete:
                    unStarFiles(starredFiles);
                    break;

                default:
                    return false;
            }

            return true;
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            if (adapter == null) return;

            adapter.setActionModeOn(false);
            adapter.deselectAllItems();

            // Here you can make any necessary updates to the activity when
            // the contextual action bar (CAB) is removed. By default, selected items are deselected/unchecked.
            mActionMode = null;
        }

    }
}
