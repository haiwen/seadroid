package com.seafile.seadroid2.ui.fragment;

import android.app.Activity;
import android.content.Intent;
import android.graphics.drawable.ColorDrawable;
import android.net.http.SslCertificate;
import android.net.http.SslError;
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
import android.webkit.JsResult;
import android.webkit.SslErrorHandler;
import android.webkit.WebChromeClient;
import android.webkit.WebView;
import android.webkit.WebViewClient;
import android.widget.AbsListView;
import android.widget.AdapterView;
import android.widget.FrameLayout;
import android.widget.LinearLayout;
import android.widget.ListView;
import android.widget.RelativeLayout;
import android.widget.Toast;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.CommitDetails;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.EventDetailsTree;
import com.seafile.seadroid2.data.SeafActivities;
import com.seafile.seadroid2.data.SeafEvent;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ssl.CertsManager;
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

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.security.cert.X509Certificate;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
    private View mProgressContainer;

    private RelativeLayout ppwContainerView;
    private RelativeLayout ppw;
    private View underLine, maskView;

    private List<SeafEvent> events;
    private boolean boolShown = false;
    private int offset;


    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        Log.d(DEBUG_TAG, "ActivitiesFragment Attached");
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
        mProgressContainer = view.findViewById(R.id.progressContainer);
        refreshLayout = (SwipeRefreshLayout) view.findViewById(R.id.swiperefresh);
        listView = (ListView) view.findViewById(R.id.activities_listview);
        events = Lists.newArrayList();
    }

    @Override
    public void onActivityCreated(final Bundle savedInstanceState) {
        Log.d(DEBUG_TAG, "onActivityCreated");

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

                LoadHistoryChangesTask task = new LoadHistoryChangesTask();
                ConcurrentAsyncTask.execute(task, seafEvent.getRepo_id(), seafEvent.getCommit_id());
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

    private void showChangesDialog(List<EventDetailsTree.EventDetailsFileItem> items) {
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

        ppw = new RelativeLayout(getContext());

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
        final RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(RelativeLayout.LayoutParams.MATCH_PARENT, RelativeLayout.LayoutParams.WRAP_CONTENT);
        params.addRule(RelativeLayout.ALIGN_PARENT_BOTTOM, ppw.getId());
        ppw.setLayoutParams(params);
        ListView listView = new ListView(mActivity);
        listView.setDivider(new ColorDrawable(getResources().getColor(R.color.divider_color)));
        listView.setDividerHeight(Utils.dip2px(mActivity, 0.6f));
        final BottomSheetAdapter adapter = new BottomSheetAdapter(mActivity, items);
        listView.setAdapter(adapter);
        listView.setOnItemClickListener(new AdapterView.OnItemClickListener() {
            @Override
            public void onItemClick(AdapterView<?> parent, View view, int position, long id) {
                switchMenu();
            }
        });

        ppw.addView(listView);

        ppw.setVisibility(View.GONE);
        ppwContainerView.addView(ppw, 2);

        switchMenu();
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
                Log.d(DEBUG_TAG, "offset " + offset);
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
                    Log.d(DEBUG_TAG, "duplicate data " + offset);
                    return;
                }

                Log.d(DEBUG_TAG, "return offset " + offset);
                events.addAll(result.getEvents());
            }

            mRefreshType = REFRESH_ON_NONE;

            offset = result.getOffset();
            if (!result.isMore()) {
                ToastUtils.show(mActivity, "no more data");
                return;
            }

            adapter.setState(mRefreshType);
            adapter.setItems(events);
            adapter.notifyDataSetChanged();
        }
    }

    class LoadHistoryChangesTask extends AsyncTask<String, Void, CommitDetails> {
        private SeafException err;

        @Override
        protected CommitDetails doInBackground(String... params) {
            if (params == null || params.length != 2) return null;

            String repoId = params[0];
            String commitId = params[1];
            try {
                final String ret = mActivity.getDataManager().getHistoryChanges(repoId, commitId);
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
                    ToastUtils.show(mActivity, err.getMessage());
                }
                return;
            }

            EventDetailsTree tree = new EventDetailsTree();
            final List<EventDetailsTree.EventDetailsFileItem> items = tree.setCommitDetails(ret);

            if (items == null) return;

            showChangesDialog(items);
        }
    }
    private void showPageLoading(boolean pageLoading) {
        if (mActivity == null) {
            return;
        }

        if (!pageLoading) {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(mActivity, android.R.anim.fade_out));
            listView.startAnimation(AnimationUtils.loadAnimation(mActivity, android.R.anim.fade_in));
            mProgressContainer.setVisibility(View.GONE);
            listView.setVisibility(View.VISIBLE);
        } else {
            mProgressContainer.startAnimation(AnimationUtils.loadAnimation(mActivity, android.R.anim.fade_in));
            listView.startAnimation(AnimationUtils.loadAnimation(mActivity, android.R.anim.fade_out));

            mProgressContainer.setVisibility(View.VISIBLE);
            listView.setVisibility(View.INVISIBLE);
        }
    }

    private void viewRepo(final String repoID) {
        final SeafRepo repo = mActivity.getDataManager().getCachedRepoByID(repoID);

        if (repo == null) {
            ToastUtils.show(mActivity, getString(R.string.repo_not_found));
            return;
        }

        if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
            String password = DataManager.getRepoPassword(repo.id);
            mActivity.showPasswordDialog(repo.name, repo.id,
                    new TaskDialog.TaskDialogListener() {
                        @Override
                        public void onTaskSuccess() {
                            switchTab(repoID, repo.getName(), repo.getRootDirID());
                        }
                    }, password);

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

        if (repo.encrypted && !DataManager.getRepoPasswordSet(repo.id)) {
            String password = DataManager.getRepoPassword(repo.id);
            mActivity.showPasswordDialog(repo.name, repo.id,
                    new TaskDialog.TaskDialogListener() {
                        @Override
                        public void onTaskSuccess() {
                            openFile(repoID, repo.getName(), path);
                        }
                    }, password);

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
        int taskID = mActivity.getTransferService().addDownloadTask(mActivity.getAccount(), repoName, repoID, filePath);
        Intent intent = new Intent(getActivity(), FileActivity.class);
        intent.putExtra("repoName", repoName);
        intent.putExtra("repoID", repoID);
        intent.putExtra("filePath", filePath);
        intent.putExtra("account", mActivity.getAccount());
        intent.putExtra("taskID", taskID);
        mActivity.startActivityForResult(intent, BrowserActivity.DOWNLOAD_FILE_REQUEST);
    }

    private class MyWebViewClient extends WebViewClient {
        // Display error messages
        @Override
        public void onReceivedError(WebView view, int errorCode, String description, String failingUrl) {
            if (mActivity != null) {
                Toast.makeText(mActivity, "Error: " + description, Toast.LENGTH_SHORT).show();
                showPageLoading(false);
            }
        }

        // Ignore SSL certificate validate
        @Override
        public void onReceivedSslError(WebView view, SslErrorHandler handler, SslError error) {
            if (mActivity == null) {
                return;
            }

            Account account = mActivity.getAccount();

            SslCertificate sslCert = error.getCertificate();
            X509Certificate savedCert = CertsManager.instance().getCertificate(account);

            if (Utils.isSameCert(sslCert, savedCert)) {
                Log.d(DEBUG_TAG, "trust this cert");
                handler.proceed();
            } else {
                Log.d(DEBUG_TAG, "cert is not trusted");
                ToastUtils.show(mActivity, R.string.ssl_error);
                showPageLoading(false);
            }
        }

        @Override
        public boolean shouldOverrideUrlLoading(WebView listView, String url) {
            Log.d(DEBUG_TAG, "loading url " + url);
            String API_URL_PREFIX= "api://";
            if (!url.startsWith(API_URL_PREFIX)) {
                return false;
            }

            String req = url.substring(API_URL_PREFIX.length(), url.length());

            Pattern REPO_PATTERN = Pattern.compile("repos/([-a-f0-9]{36})/?");
            Pattern REPO_FILE_PATTERN = Pattern.compile("repo/([-a-f0-9]{36})/files/\\?p=(.+)");
            Matcher matcher;

            if ((matcher = fullMatch(REPO_PATTERN, req)) != null) {
                String repoID = matcher.group(1);
                viewRepo(repoID);

            } else if ((matcher = fullMatch(REPO_FILE_PATTERN, req)) != null) {
                String repoID = matcher.group(1);

                try {
                    String path = URLDecoder.decode(matcher.group(2), "UTF-8");
                    viewFile(repoID, path);
                } catch (UnsupportedEncodingException e) {
                    // Ignore
                }
            }

            return true;
        }

        @Override
        public void onPageFinished(WebView listView, String url) {
            Log.d(DEBUG_TAG, "onPageFinished " + url);
            if (mActivity != null) {
                String js = String.format("javascript:setToken('%s')",
                        mActivity.getAccount().getToken());
                listView.loadUrl(js);
            }
            showPageLoading(false);
        }
    }

    private static Matcher fullMatch(Pattern pattern, String str) {
        Matcher matcher = pattern.matcher(str);
        return matcher.matches() ? matcher : null;
    }

    private class MyWebChromeClient extends WebChromeClient {

        // For debug js
        @Override
        public boolean onJsAlert(WebView view, String url, String message, JsResult result) {
            Log.d(DEBUG_TAG, "alert: " + message);
            return super.onJsAlert(view, url, message, result);
        }
    }
}
