package com.seafile.seadroid2.ui.fragment;

import java.util.List;

import android.app.Activity;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.widget.SwipeRefreshLayout;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.widget.*;

import com.actionbarsherlock.app.SherlockListFragment;
import com.actionbarsherlock.view.ActionMode;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafStarredFile;
import com.seafile.seadroid2.ui.ActionModeCallback;
import com.seafile.seadroid2.ui.PullToRefreshListView;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.adapter.StarredItemAdapter;
import com.seafile.seadroid2.util.Utils;

public class StarredFragment extends SherlockListFragment
        implements ActionModeCallback.ActionModeOperationListener {
    private StarredItemAdapter adapter;
    private BrowserActivity mActivity = null;

    private SwipeRefreshLayout refreshLayout;
    private ListView mListView;
    private TextView mNoStarredView;
    private View mProgressContainer;
    private View mListContainer;
    private TextView mErrorText;
    private ActionMode mActionMode;
    private LinearLayout mTaskActionBar;
    private RelativeLayout mUnstarFiles;
    private static final int REFRESH_ON_RESUME = 0;
    private static final int REFRESH_ON_PULL = 1;
    private static final int REFRESH_ON_OVERFLOW_MENU = 2;
    private static int mRefreshType = -1;

    private DataManager getDataManager() {
        return mActivity.getDataManager();
    }

    public StarredItemAdapter getAdapter() {
        return adapter;
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

    public interface OnStarredFileSelectedListener {
        void onStarredFileSelected(SeafStarredFile starredFile);
    }

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        mActivity = (BrowserActivity)activity;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.starred_fragment, container, false);
        refreshLayout = (SwipeRefreshLayout) root.findViewById(R.id.swiperefresh);
        mListView = (ListView) root.findViewById(android.R.id.list);
        mNoStarredView = (TextView) root.findViewById(android.R.id.empty);
        mListContainer =  root.findViewById(R.id.listContainer);
        mErrorText = (TextView)root.findViewById(R.id.error_message);
        mProgressContainer = root.findViewById(R.id.progressContainer);
        mTaskActionBar = (LinearLayout) root.findViewById(R.id.multi_op_bottom_action_bar);
        mUnstarFiles = (RelativeLayout) root.findViewById(R.id.multi_op_unstar_rl);
        mUnstarFiles.setOnClickListener(new UnstarMultiFilesClickListener());

        mListView.setOnItemLongClickListener(new AdapterView.OnItemLongClickListener() {
            @Override
            public boolean onItemLongClick(AdapterView<?> parent, View view, int position, long id) {
                startContextualActionMode(position);
                return true;
            }
        });
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
        if (mRefreshType == REFRESH_ON_PULL
                || mRefreshType == REFRESH_ON_OVERFLOW_MENU
                || starredFiles == null
                || refreshTimeout)  {
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
    }

    private void showLoading(boolean show) {
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

        final SeafStarredFile starredFile = (SeafStarredFile)adapter.getItem(position);
        mActivity.onStarredFileSelected(starredFile);
    }

    private void unStarFiles(List<SeafStarredFile> starredFiles) {
        for (SeafStarredFile seafStarredFile : starredFiles) {
            doUnStarFile(seafStarredFile.getRepoID(), seafStarredFile.getPath());
        }
    }

    private void doUnStarFile(String repoID, String path) {
        if (!Utils.isNetworkOn()) {
            ToastUtils.show(mActivity, R.string.network_down);
            return;
        }

        ConcurrentAsyncTask.execute(new UnStarFileTask(repoID, path));

    }

    public void doStarFile(String repoID, String path, String filename) {

        if (!Utils.isNetworkOn()) {
            ToastUtils.show(mActivity, R.string.network_down);
            return;
        }

        String p = Utils.pathJoin(path, filename);
        ConcurrentAsyncTask.execute(new StarFileTask(repoID, p));
    }

    private class LoadStarredFilesTask extends AsyncTask<Void, Void, List<SeafStarredFile> > {

        SeafException err = null;

        DataManager dataManager;

        public LoadStarredFilesTask(DataManager dataManager) {
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
                List<SeafStarredFile> starredFiles = dataManager.getStarredFiles();
                return starredFiles;
            } catch (SeafException e) {
                err = e;
                return null;
            }

        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafStarredFile> starredFiles) {
            if (mActivity == null)
                // this occurs if user navigation to another activity
                return;

            if (mRefreshType == REFRESH_ON_RESUME || mRefreshType == REFRESH_ON_OVERFLOW_MENU){
                showLoading(false);
            } else if (mRefreshType == REFRESH_ON_PULL) {
                // Call onRefreshComplete when the list has been refreshed.
                //mListView.onRefreshComplete(getDataManager().getLastPullToRefreshTime(DataManager.PULL_TO_REFRESH_LAST_TIME_FOR_STARRED_FRAGMENT));
                getDataManager().saveLastPullToRefreshTime(System.currentTimeMillis(), DataManager.PULL_TO_REFRESH_LAST_TIME_FOR_STARRED_FRAGMENT);
                refreshLayout.setRefreshing(false);
            }


            if (err != null) {
                showError(getString(R.string.error_when_load_starred));
                return;
            }

            if (starredFiles == null) {
                showError(getString(R.string.error_when_load_starred));
                return;
            }

            updateAdapterWithStarredFiles(starredFiles);
        }
    }

    class StarFileTask extends AsyncTask<Void, Void, Void> {
        private String repoId;
        private String path;
        private SeafException err;

        public StarFileTask(String repoId, String path) {
            this.repoId = repoId;
            this.path = path;
        }

        @Override
        protected Void doInBackground(Void... params) {

            try {
                mActivity.getDataManager().star(repoId, path);
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            if (err != null) {
                ToastUtils.show(mActivity, R.string.star_file_failed);
                return;
            }

            ToastUtils.show(mActivity, R.string.star_file_succeed);
        }
    }

    class UnStarFileTask extends AsyncTask<Void, Void, Void> {
        private String repoId;
        private String path;
        private SeafException err;

        public UnStarFileTask(String repoId, String path) {
            this.repoId = repoId;
            this.path = path;
        }

        @Override
        protected Void doInBackground(Void... params) {

            try {
                mActivity.getDataManager().unstar(repoId, path);
            } catch (SeafException e) {
                err = e;
            }

            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            if (err != null) {
                ToastUtils.show(mActivity, R.string.unstar_file_failed);
                return;
            }

            mRefreshType = REFRESH_ON_RESUME;
            refreshView();
            adapter.deselectAllItems();
            mActionMode.setTitle(getResources().
                    getQuantityString(R.plurals.transfer_list_items_selected, 0, 0));
        }
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
    public void startContextualActionMode(int position) {
        startContextualActionMode();

        if (adapter == null) return;

        adapter.toggleSelection(position);
        updateContextualActionBar();

    }

    public void startContextualActionMode() {
        if (mActionMode == null) {
            // start the actionMode
            mActionMode = mActivity.startActionMode(new ActionModeCallback(this));
        }

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

    class UnstarMultiFilesClickListener implements View.OnClickListener {

        @Override
        public void onClick(View v) {
            final List<SeafStarredFile> selectedDirents = adapter.getSelectedItemsValues();
            if (selectedDirents.size() == 0) {
                ToastUtils.show(mActivity, R.string.action_mode_no_items_selected);
                return;
            }

            switch (v.getId()) {
                case R.id.multi_op_unstar_rl:
                    unStarFiles(selectedDirents);
                    break;
            }
        }
    }
}
