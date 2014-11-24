package com.seafile.seadroid2.ui.fragment;

import java.util.List;

import android.app.Activity;
import android.os.AsyncTask;
import android.os.Bundle;
import android.text.format.DateUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.ListView;
import android.widget.TextView;

import com.actionbarsherlock.app.SherlockListFragment;
import com.handmark.pulltorefresh.library.PullToRefreshBase;
import com.handmark.pulltorefresh.library.PullToRefreshBase.OnLastItemVisibleListener;
import com.handmark.pulltorefresh.library.PullToRefreshBase.OnRefreshListener;
import com.handmark.pulltorefresh.library.PullToRefreshListView;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafStarredFile;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.adapter.StarredItemAdapter;

public class StarredFragment extends SherlockListFragment {
    private StarredItemAdapter adapter;
    private BrowserActivity mActivity = null;

    private PullToRefreshListView mPullRefreshListView;
    private TextView mNoStarredView;
    private View mProgressContainer;
    private View mListContainer;
    private TextView mErrorText;


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
        mActivity = (BrowserActivity)activity;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        View root = inflater.inflate(R.layout.starred_fragment, container, false);
        mPullRefreshListView = (PullToRefreshListView) root.findViewById(R.id.pull_refresh_list);
        mNoStarredView = (TextView) root.findViewById(android.R.id.empty);
        mListContainer =  root.findViewById(R.id.listContainer);
        mErrorText = (TextView)root.findViewById(R.id.error_message);
        mProgressContainer = root.findViewById(R.id.progressContainer);
        
        // Set a listener to be invoked when the list should be refreshed.
        mPullRefreshListView.setOnRefreshListener(new OnRefreshListener<ListView>() {
            @Override
            public void onRefresh(PullToRefreshBase<ListView> refreshView) {
                String label = DateUtils.formatDateTime(mActivity, System.currentTimeMillis(),
                        DateUtils.FORMAT_SHOW_TIME | DateUtils.FORMAT_SHOW_DATE | DateUtils.FORMAT_ABBREV_ALL);

                // Update the LastUpdatedLabel
                refreshView.getLoadingLayoutProxy().setLastUpdatedLabel(label);

                // Do work to refresh the list here.
                refreshView();
            }
        });

        // Add an end-of-list listener
        mPullRefreshListView.setOnLastItemVisibleListener(new OnLastItemVisibleListener() {

            @Override
            public void onLastItemVisible() {
                // Toast.makeText(mActivity, "end of list", Toast.LENGTH_SHORT).show();
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
        mPullRefreshListView.setRefreshing(false);
        refreshView();
    }
    
    public void refreshView() {

        if (mActivity == null)
            return;

        mErrorText.setVisibility(View.GONE);
        mListContainer.setVisibility(View.VISIBLE);
        // showLoading(true);
        ConcurrentAsyncTask.execute(new LoadStarredFilesTask(getDataManager()));
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
            mPullRefreshListView.setVisibility(View.VISIBLE);
            mNoStarredView.setVisibility(View.GONE);
        } else {
            mPullRefreshListView.setVisibility(View.GONE);
            mNoStarredView.setVisibility(View.VISIBLE);
        }
    }

    @Override
    public void onListItemClick(final ListView l, final View v, final int position, final long id) {

        final SeafStarredFile starredFile = (SeafStarredFile)adapter.getItem(position - 1);
        mActivity.onStarredFileSelected(starredFile);
    }

    private class LoadStarredFilesTask extends AsyncTask<Void, Void, List<SeafStarredFile> > {

        SeafException err = null;

        DataManager dataManager;

        public LoadStarredFilesTask(DataManager dataManager) {
            this.dataManager = dataManager;
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

            if (err != null) {
                showError(getString(R.string.error_when_load_starred));
                return;
            }

            if (starredFiles == null) {
                showError(getString(R.string.error_when_load_starred));
                return;
            }

            updateAdapterWithStarredFiles(starredFiles);
            // Call onRefreshComplete when the list has been refreshed.
            mPullRefreshListView.onRefreshComplete();
            //showLoading(false);
        }
    }


}
