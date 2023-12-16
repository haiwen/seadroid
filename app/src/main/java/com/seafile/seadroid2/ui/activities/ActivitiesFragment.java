package com.seafile.seadroid2.ui.activities;

import android.app.Activity;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;

import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.swiperefreshlayout.widget.SwipeRefreshLayout;

import android.text.TextUtils;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.ImageView;
import android.widget.ListView;
import android.widget.TextView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.CommitDetails;
import com.seafile.seadroid2.data.EventDetailsFileItem;
import com.seafile.seadroid2.data.EventDetailsTree;
import com.seafile.seadroid2.data.SeafActivities;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafEvent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.ServerInfo;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.ui.BrowserActivity;
import com.seafile.seadroid2.ui.activity.FileActivity;
import com.seafile.seadroid2.ui.adapter.BottomSheetAdapter;
import com.seafile.seadroid2.ui.bottomsheet.BottomSheetListFragment;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONException;

import java.util.List;
import java.util.Locale;

public class ActivitiesFragment extends Fragment {
    private static final String DEBUG_TAG = "ActivitiesFragment";
    public static final int REFRESH_ON_NONE = 0;
    public static final int REFRESH_ON_PULL_DOWN_SWIPE = 1;
    public static final int REFRESH_ON_PULL_DOWN_RESUME = 3;
    public static final int REFRESH_ON_PULL_UP = 2;
    public static final int VERSIONS_NUMBER = 6;
    private static int mRefreshType = REFRESH_ON_NONE;
    private boolean useNewActivity = false;

    private BrowserActivity mActivity;
    private SwipeRefreshLayout refreshLayout;
    private ListView listView;
    private ActivitiesItemAdapter adapter;
    private ImageView mEmptyView;
    private View mProgressContainer;
    private View mListContainer;
    private TextView mErrorText;
    private List<SeafEvent> events;
    private int offset;
    private Account account;
    private AccountManager accountManager;

    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        // Log.d(DEBUG_TAG, "ActivitiesFragment Attached");
        mActivity = (BrowserActivity) getActivity();
    }

    @Override
    public void onDetach() {
        super.onDetach();
        mActivity = null;
    }

    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        return inflater.inflate(R.layout.activities_fragment, container, false);
    }

    @Override
    public void onViewCreated(View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        refreshLayout = (SwipeRefreshLayout) view.findViewById(R.id.swiperefresh);
        listView = (ListView) view.findViewById(R.id.activities_listview);
        mEmptyView = (ImageView) view.findViewById(R.id.empty);
        mListContainer = view.findViewById(R.id.fl_activities_list_container);
        mErrorText = (TextView) view.findViewById(R.id.error_message);
        mProgressContainer = view.findViewById(R.id.progressContainer);

        events = Lists.newArrayList();
    }

    @Override
    public void onActivityCreated(final Bundle savedInstanceState) {
        // Log.d(DEBUG_TAG, "onActivityCreated");

        refreshLayout.setColorSchemeResources(R.color.fancy_orange);
        refreshLayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                mRefreshType = REFRESH_ON_PULL_DOWN_SWIPE;
                offset = 0;
                refreshView();
            }
        });

        adapter = new ActivitiesItemAdapter(mActivity);
        listView.setAdapter(adapter);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> adapterView, View view, int position, long l) {
                final SeafEvent seafEvent = (SeafEvent) adapterView.getItemAtPosition(position);
                if (mActivity == null) return;

                if (TextUtils.isEmpty(seafEvent.getCommit_id()) || TextUtils.equals("null", seafEvent.getCommit_id().toLowerCase(Locale.getDefault()))) {
                    return;
                }

                final String repoId = seafEvent.getRepo_id();
                final String repoName = seafEvent.getRepo_name();

                if (seafEvent.isRepo_encrypted()) {
                    final SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoId);

                    if (repo == null) {
                        mActivity.showShortToast(mActivity, getString(R.string.repo_not_found));
                        return;
                    }

                    if (!mActivity.getDataManager().getRepoPasswordSet(repo.repo_id)) {
                        String password = mActivity.getDataManager().getRepoPassword(repoId);
                        mActivity.showPasswordDialog(repoName, repoId,
                                new TaskDialog.TaskDialogListener() {
                                    @Override
                                    public void onTaskSuccess() {
                                        LoadHistoryChangesTask task = new LoadHistoryChangesTask(seafEvent);
                                        ConcurrentAsyncTask.execute(task);
                                    }
                                }, password);
                        return;
                    }
                }
                LoadHistoryChangesTask task = new LoadHistoryChangesTask(seafEvent);
                ConcurrentAsyncTask.execute(task);
            }
        });

        listView.setOnScrollListener(new AbsListView.OnScrollListener() {
            @Override
            public void onScrollStateChanged(AbsListView view, int i) {
                if (adapter == null || adapter.getCount() == 0) {
                    return;
                }
                boolean scrollEnd = false;
                try {
                    if (view.getPositionForView(adapter.getFooterView()) == view.getLastVisiblePosition())
                        scrollEnd = true;
                } catch (Exception e) {
                    scrollEnd = false;
                }

                if (mRefreshType == REFRESH_ON_NONE && scrollEnd && offset > 0) {
                    refreshView();
                    mRefreshType = REFRESH_ON_PULL_UP;
                    adapter.setFooterViewLoading(true);
                } else {
                    adapter.setFooterViewLoading(false);
                }

                adapter.setState(mRefreshType);
            }

            @Override
            public void onScroll(AbsListView absListView, int i, int i1, int i2) {
            }
        });

        mRefreshType = REFRESH_ON_PULL_DOWN_RESUME;
        offset = 0;
        accountManager = new AccountManager(getActivity());
        refreshView();

        mActivity.supportInvalidateOptionsMenu();

        super.onActivityCreated(savedInstanceState);
    }

    public void refreshView() {
        account = accountManager.getCurrentAccount();
        ServerInfo serverInfo = accountManager.getServerInfo(account);
        String server_version = serverInfo.getVersion();
        if (!TextUtils.isEmpty(server_version)) {
            String[] arr1 = server_version.split("\\.");
            if (arr1 != null && Integer.parseInt(arr1[0]) > VERSIONS_NUMBER) {
                useNewActivity = true;
            } else {
                useNewActivity = false;
            }
        }
        new LoadEventsTask().execute();
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
        mErrorText.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                refreshView();
            }
        });
    }

    public void showLoading(boolean show) {
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

    private void onItemClicked(EventDetailsFileItem fileItem) {
        if (fileItem == null) {
            return;
        }

        if (fileItem.isFileOpenable()) {
            openLocalFile(fileItem);
        }
    }

    private void openLocalFile(EventDetailsFileItem fileItem) {
        if (fileItem.isDir()) {
            viewRepo(fileItem.getEvent().getRepo_id(), fileItem.getPath());
        } else {
            viewFile(fileItem.getEvent().getRepo_id(), fileItem.getPath());
        }
    }

    private class LoadEventsTask extends AsyncTask<Void, Void, SeafActivities> {
        SeafException err;

        @Override
        protected void onPreExecute() {
            if (mRefreshType == REFRESH_ON_PULL_DOWN_RESUME)
                showLoading(true);
        }

        @Override
        protected SeafActivities doInBackground(Void... voids) {
            if (mActivity == null) return null;

            try {
                // Log.d(DEBUG_TAG, "offset " + offset);
                return mActivity.getDataManager().getEvents(offset, useNewActivity);
            } catch (SeafException e) {
                err = e;
                e.printStackTrace();
                return null;
            }
        }

        @Override
        protected void onPostExecute(SeafActivities result) {
            if (mActivity == null)
                // this occurs if user navigation to another activity
                return;

            if (mRefreshType == REFRESH_ON_PULL_DOWN_RESUME) {
                showLoading(false);
            } else if (mRefreshType == REFRESH_ON_PULL_DOWN_SWIPE) {
                refreshLayout.setRefreshing(false);
            }

            if (result == null) {
                if (err != null) {
                    if (err == SeafException.remoteWipedException) {
                        mActivity.completeRemoteWipe();
                    } else {
//                        mActivity.showShortToast(mActivity, err.getMessage());
                        showError(R.string.error_when_load_activities);
                    }
                }
                return;
            }

            if (mRefreshType == REFRESH_ON_PULL_DOWN_SWIPE) {
                events = result.getEvents();
                if (events.isEmpty()) {
                    listView.setVisibility(View.GONE);
                    mEmptyView.setVisibility(View.VISIBLE);
                } else {
                    listView.setVisibility(View.VISIBLE);
                    mEmptyView.setVisibility(View.GONE);
                }
            } else {
                if (offset == result.getOffset()) {
                    // duplicate data
                    // Log.d(DEBUG_TAG, "duplicate data " + offset);
                    return;
                }

                // Log.d(DEBUG_TAG, "return offset " + offset);
                if (result.getEvents() != null) {
                    events.addAll(result.getEvents());
                }
            }

            mRefreshType = REFRESH_ON_NONE;

            offset = result.getOffset();
            if (!result.isMore()) {
                mActivity.showShortToast(mActivity, getString(R.string.no_more_activities));
                return;
            }

            adapter.setState(mRefreshType);
            adapter.setItems(events, useNewActivity);
            adapter.notifyDataSetChanged();
        }
    }

    private class LoadHistoryChangesTask extends AsyncTask<String, Void, CommitDetails> {
        private SeafException err;
        private SeafEvent event;

        public LoadHistoryChangesTask(SeafEvent event) {
            this.event = event;
        }

        @Override
        protected CommitDetails doInBackground(String... params) {
            try {

                final String ret = mActivity.getDataManager().getHistoryChanges(event.getRepo_id(), event.getCommit_id());
                return CommitDetails.fromJson(ret);
            } catch (SeafException e) {
                err = e;
                e.printStackTrace();
                return null;
            } catch (JSONException e) {
                e.printStackTrace();
                return null;
            }
        }

        @Override
        protected void onPostExecute(CommitDetails ret) {
            super.onPostExecute(ret);

            if (ret == null) {
                if (err != null) {
                    Log.e(DEBUG_TAG, err.getCode() + err.getMessage());
                    mActivity.showShortToast(mActivity, err.getMessage());
                }
                return;
            }

            final EventDetailsTree tree = new EventDetailsTree(event);
            final List<EventDetailsFileItem> items = tree.setCommitDetails(ret);

            showChangesDialog2(items);
        }
    }

    private void showChangesDialog2(final List<EventDetailsFileItem> items) {
        BottomSheetListFragment sheetFragment = new BottomSheetListFragment();
        sheetFragment.setOnItemClickListener((parent, view, position, id) -> {
            final EventDetailsFileItem fileItem = items.get(position);
            onItemClicked(fileItem);
        });

        BottomSheetAdapter adapter = new BottomSheetAdapter(mActivity, items);
        sheetFragment.setAdapter(adapter);
        sheetFragment.show(getChildFragmentManager(), BottomSheetListFragment.class.getSimpleName());
    }

    private void viewRepo(final String repoID, final String path) {
        final SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            mActivity.showShortToast(mActivity, getString(R.string.repo_not_found));
            return;
        }

        if (repo.encrypted && !mActivity.getDataManager().getRepoPasswordSet(repo.repo_id)) {
            String password = mActivity.getDataManager().getRepoPassword(repo.repo_id);
            mActivity.showPasswordDialog(repo.repo_name, repo.repo_id,
                    new TaskDialog.TaskDialogListener() {
                        @Override
                        public void onTaskSuccess() {
                            switchTab(repoID, repo.getRepoName(), path);
                        }
                    }, password);

            switchTab(repoID, repo.getRepoName(), path);
        }
    }

    private void viewFile(final String repoID, final String path) {
        final SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            mActivity.showShortToast(mActivity, R.string.library_not_found);
            return;
        }

        if (repo.encrypted && !mActivity.getDataManager().getRepoPasswordSet(repo.repo_id)) {
            String password = mActivity.getDataManager().getRepoPassword(repo.repo_id);
            mActivity.showPasswordDialog(repo.repo_name, repo.repo_id,
                    new TaskDialog.TaskDialogListener() {
                        @Override
                        public void onTaskSuccess() {
                            openFile(repoID, repo.getRepoName(), path);
                        }
                    }, password);

        } else {
            openFile(repoID, repo.getRepoName(), path);
        }
    }

    private void switchTab(String repoID, String repoName, String path) {
        NavContext nav = mActivity.getNavContext();
        nav.setRepoID(repoID);
        nav.setRepoName(repoName);
        if (!path.startsWith("/"))
            path = "/" + path;

        if (!path.endsWith("/"))
            path = path + "/";

        path = Utils.getParentPath(path);

        nav.setDirPath(path);

        // switch to LIBRARY TAB
        mActivity.setCurrentPosition(BrowserActivity.INDEX_LIBRARY_TAB);
    }

    private void openFile(String repoID, String repoName, String filePath) {
        // Log.d(DEBUG_TAG, "open file " + repoName + filePath);
        final String parentPath = Utils.getParentPath(filePath);
        final List<SeafDirent> cachedDirents = mActivity.getDataManager().getCachedDirents(repoID, parentPath);
        long fileSize = -1L;
        if (cachedDirents != null) {
            for (SeafDirent seafDirent : cachedDirents) {
                if (seafDirent.name.equals(filePath)) {
                    fileSize = seafDirent.size;
                }
            }
        }

        // Log.d(DEBUG_TAG, "open file " + repoName + filePath);
        int taskID = mActivity.getTransferService().addDownloadTask(mActivity.getAccount(), repoName, repoID, filePath, fileSize);
        Intent intent = new Intent(getActivity(), FileActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", mActivity.getAccount());
        intent.putExtra("taskID", taskID);
        mActivity.startActivityForResult(intent, BrowserActivity.DOWNLOAD_FILE_REQUEST);
    }
}
