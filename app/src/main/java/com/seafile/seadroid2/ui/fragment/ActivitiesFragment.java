package com.seafile.seadroid2.ui.fragment;

import android.app.Activity;
import android.content.Intent;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.annotation.Nullable;
import android.support.v4.app.Fragment;
import android.support.v4.widget.SwipeRefreshLayout;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.RelativeLayout;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.CommitDetails;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.EventDetailsFileItem;
import com.seafile.seadroid2.data.EventDetailsTree;
import com.seafile.seadroid2.data.SeafActivities;
import com.seafile.seadroid2.data.SeafEvent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.NavContext;
import com.seafile.seadroid2.ui.ToastUtils;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.ui.activity.FileActivity;
import com.seafile.seadroid2.ui.adapter.ActivitiesItemAdapter;
import com.seafile.seadroid2.ui.adapter.BottomSheetAdapter;
import com.seafile.seadroid2.ui.dialog.TaskDialog;
import com.seafile.seadroid2.util.ConcurrentAsyncTask;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONException;

import java.util.List;

public class ActivitiesFragment extends Fragment {
    private static final String DEBUG_TAG = "ActivitiesFragment";
    public static final int REFRESH_ON_NONE = 0;
    public static final int REFRESH_ON_PULL_DOWN = 1;
    public static final int REFRESH_ON_PULL_UP = 2;
    private static int mRefreshType = REFRESH_ON_NONE;

    private BrowserActivity mActivity;
    private SwipeRefreshLayout refreshLayout;
    private ListView listView;
    private ActivitiesItemAdapter adapter;

    private RelativeLayout ppwContainerView;
    private RelativeLayout ppw;
    private View underLine, maskView;

    private List<SeafEvent> events;
    private boolean boolShown = false;
    private int offset;

    public boolean isBottomSheetShown() {
        return boolShown;
    }

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
    public View onCreateView(LayoutInflater inflater, ViewGroup container,
            Bundle savedInstanceState) {
        return inflater.inflate(R.layout.activities_fragment, container, false);
    }

    @Override
    public void onViewCreated(View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        refreshLayout = (SwipeRefreshLayout) view.findViewById(R.id.swiperefresh);
        listView = (ListView) view.findViewById(R.id.activities_listview);
        events = Lists.newArrayList();
    }

    @Override
    public void onActivityCreated(final Bundle savedInstanceState) {
        // Log.d(DEBUG_TAG, "onActivityCreated");

        refreshLayout.setColorSchemeResources(R.color.fancy_orange);
        refreshLayout.setOnRefreshListener(new SwipeRefreshLayout.OnRefreshListener() {
            @Override
            public void onRefresh() {
                mRefreshType = REFRESH_ON_PULL_DOWN;
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

                final String repoId = seafEvent.getRepo_id();
                final String repoName = seafEvent.getRepo_name();

                if (seafEvent.isRepo_encrypted() && !DataManager.getRepoEnckeySet(repoId)) {
                    final SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoId);

                    if (repo == null) {
                        ToastUtils.show(mActivity, getString(R.string.repo_not_found));
                        return;
                    }

                    String password = DataManager.getRepoPassword(repoId);
                    mActivity.showPasswordDialog(repoName, repoId,
                            new TaskDialog.TaskDialogListener() {
                                @Override
                                public void onTaskSuccess() {
                                    switchTab(repoId, repoName, repo.getRootDirID());
                                }
                            }, password);
                } else {
                    LoadHistoryChangesTask task = new LoadHistoryChangesTask(seafEvent);
                    ConcurrentAsyncTask.execute(task);
                }
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
                    if (view.getPositionForView(adapter.getFooterView()) == view.getLastVisiblePosition()) scrollEnd = true;
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
            public void onScroll(AbsListView absListView, int i, int i1, int i2) {}
        });

        mRefreshType = REFRESH_ON_PULL_DOWN;
        offset = 0;
        refreshView();

        mActivity.supportInvalidateOptionsMenu();

        super.onActivityCreated(savedInstanceState);
    }

    public void refreshView() {
        new LoadEventsTask().execute();
    }

    public void hideBottomSheet() {
        switchMenu();
    }

    public void switchMenu() {
        if (mActivity == null || ppw == null || ppwContainerView == null || maskView == null || underLine == null) {
            boolShown = false;
            return;
        }

        final FrameLayout container = mActivity.getContainer();
        if (!boolShown) {
            container.removeView(ppwContainerView);
            container.addView(ppwContainerView);
            ppw.setVisibility(View.VISIBLE);
            ppw.setAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.menu_in));
            underLine.setVisibility(View.VISIBLE);
            maskView.setVisibility(View.VISIBLE);
            maskView.setAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.mask_in));
        } else {
            ppw.setVisibility(View.GONE);
            ppw.setAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.menu_out));
            underLine.setVisibility(View.GONE);
            container.removeView(underLine);
            maskView.setVisibility(View.GONE);
            maskView.setAnimation(AnimationUtils.loadAnimation(getContext(), R.anim.mask_out));
        }

        boolShown = !boolShown;
    }

    private void showChangesDialog(final List<EventDetailsFileItem> items) {
        int maskColor = 0x88888888;

        if (boolShown && ppwContainerView != null) {
            switchMenu();
            return;
        }

        ppwContainerView = new RelativeLayout(mActivity);
        ppwContainerView.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT));

        underLine = new View(getContext());
        underLine.setLayoutParams(new LinearLayout.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, Utils.dip2px(mActivity, 1.0f)));
        underLine.setBackgroundColor(getResources().getColor(R.color.divider_color));
        underLine.setVisibility(View.GONE);
        ppwContainerView.addView(underLine, 0);

        ppw = (RelativeLayout) View.inflate(mActivity, R.layout.ppw_history_changes, null);

        maskView = new View(getContext());
        maskView.setLayoutParams(new FrameLayout.LayoutParams(FrameLayout.LayoutParams.MATCH_PARENT, FrameLayout.LayoutParams.MATCH_PARENT));
        maskView.setBackgroundColor(maskColor);
        maskView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                switchMenu();
            }
        });
        maskView.setVisibility(View.GONE);
        ppwContainerView.addView(maskView, 1);
        ListView listView = (ListView) ppw.findViewById(R.id.lv_history_changes);
        final BottomSheetAdapter adapter = new BottomSheetAdapter(mActivity, items);
        listView.setAdapter(adapter);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                final EventDetailsFileItem fileItem = items.get(position);
                onItemClicked(fileItem);
                switchMenu();
            }
        });

        ppw.setVisibility(View.GONE);
        ppwContainerView.addView(ppw, 2);

        switchMenu();
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
            viewRepo(fileItem.getEvent().getRepo_id());
        } else {
            viewFile(fileItem.getEvent().getRepo_id(), fileItem.getPath());
        }
    }

    class LoadEventsTask extends AsyncTask<Void, Void, SeafActivities> {
        SeafException err;

        @Override
        protected void onPreExecute() {
            super.onPreExecute();
        }

        @Override
        protected SeafActivities doInBackground(Void... voids) {
            if (mActivity == null) return null;

            try {
                // Log.d(DEBUG_TAG, "offset " + offset);
                return mActivity.getDataManager().getEvents(offset);
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
        protected void onPostExecute(SeafActivities result) {
            super.onPostExecute(result);
            refreshLayout.setRefreshing(false);
            if (result == null) {
                if (err != null) {
                    ToastUtils.show(mActivity, err.getMessage());
                }
                return;
            }

            if (mRefreshType == REFRESH_ON_PULL_DOWN) {
                events = result.getEvents();
            } else {
                if (offset == result.getOffset()) {
                    // duplicate data
                    // Log.d(DEBUG_TAG, "duplicate data " + offset);
                    return;
                }

                // Log.d(DEBUG_TAG, "return offset " + offset);
                events.addAll(result.getEvents());
            }

            mRefreshType = REFRESH_ON_NONE;

            offset = result.getOffset();
            if (!result.isMore()) {
                ToastUtils.show(mActivity, getString(R.string.no_more_activities));
                return;
            }

            adapter.setState(mRefreshType);
            adapter.setItems(events);
            adapter.notifyDataSetChanged();
        }
    }

    class LoadHistoryChangesTask extends AsyncTask<String, Void, CommitDetails> {
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
                    ToastUtils.show(mActivity, err.getMessage());
                }
                return;
            }

            final EventDetailsTree tree = new EventDetailsTree(event);
            final List<EventDetailsFileItem> items = tree.setCommitDetails(ret);

            showChangesDialog(items);
        }
    }

    private void viewRepo(final String repoID) {
        final SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            ToastUtils.show(mActivity, getString(R.string.repo_not_found));
            return;
        }

        if (repo.encrypted && !DataManager.getRepoEnckeySet(repo.id)) {
            String encKey = DataManager.getRepoEncKey(repo.id);
            mActivity.showPasswordDialog(repo.name, repo.id,
                    new TaskDialog.TaskDialogListener() {
                        @Override
                        public void onTaskSuccess() {
                            switchTab(repoID, repo.getName(), repo.getRootDirID());
                        }
                    }, encKey);

        } else {
            switchTab(repoID, repo.getName(), repo.getRootDirID());
        }
    }

    private void viewFile(final String repoID, final String path) {
        final SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            ToastUtils.show(mActivity, R.string.library_not_found);
            return;
        }

        if (repo.encrypted && !DataManager.getRepoEnckeySet(repo.id)) {
            String encKey = DataManager.getRepoEncKey(repo.id);
            mActivity.showPasswordDialog(repo.name, repo.id,
                    new TaskDialog.TaskDialogListener() {
                        @Override
                        public void onTaskSuccess() {
                            openFile(repoID, repo.getName(), path);
                        }
                    }, encKey);

        } else {
            openFile(repoID, repo.getName(), path);
        }
    }

    private void switchTab(String repoID, String repoName, String repoDir) {
        NavContext nav = mActivity.getNavContext();
        nav.setRepoID(repoID);
        nav.setRepoName(repoName);
        nav.setDir("/", repoDir);

        // switch to LIBRARY TAB
        mActivity.setCurrentPosition(BrowserActivity.INDEX_LIBRARY_TAB);
    }

    private void openFile(String repoID, String repoName, String filePath) {
        // Log.d(DEBUG_TAG, "open fiel " + repoName + filePath);
        int taskID = mActivity.getTransferService().addDownloadTask(mActivity.getAccount(), repoName, repoID, filePath);
        Intent intent = new Intent(getActivity(), FileActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", mActivity.getAccount());
        intent.putExtra("taskID", taskID);
        mActivity.startActivityForResult(intent, BrowserActivity.DOWNLOAD_FILE_REQUEST);
    }
}
