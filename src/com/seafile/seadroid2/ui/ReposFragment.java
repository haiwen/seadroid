package com.seafile.seadroid2.ui;

import java.io.File;
import java.net.HttpURLConnection;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import android.app.Activity;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.ListView;
import android.widget.TextView;

import com.actionbarsherlock.app.SherlockListFragment;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.CertsManager;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafConnection;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.data.SeafGroup;
import com.seafile.seadroid2.data.SeafItem;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.util.Utils;


public class ReposFragment extends SherlockListFragment {

    private static final String DEBUG_TAG = "ReposFragment";

    private SeafItemAdapter adapter;
    private BrowserActivity mActivity = null;

    private ListView mList;
    private TextView mEmptyView;
    private View mProgressContainer;
    private View mListContainer;
    private TextView mErrorText;

    private DataManager getDataManager() {
        return mActivity.getDataManager();
    }

    private NavContext getNavContext() {
        return mActivity.getNavContext();
    }

    public SeafItemAdapter getAdapter() {
        return adapter;
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
        mList = (ListView) root.findViewById(android.R.id.list);
        mEmptyView = (TextView) root.findViewById(android.R.id.empty);
        mListContainer =  root.findViewById(R.id.listContainer);
        mErrorText = (TextView)root.findViewById(R.id.error_message);
        mProgressContainer = root.findViewById(R.id.progressContainer);

        return root;
    }

    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        Log.d(DEBUG_TAG, "ReposFragment onActivityCreated");
        adapter = new SeafItemAdapter(mActivity);
        setListAdapter(adapter);

        getListView().setChoiceMode(ListView.CHOICE_MODE_SINGLE);
    }

    @Override
    public void onStart() {
        Log.d(DEBUG_TAG, "ReposFragment onStart");
        super.onStart();
    }

    @Override
    public void onStop() {
        Log.d(DEBUG_TAG, "ReposFragment onStop");
        super.onStop();
    }

    @Override
    public void onResume() {
        super.onResume();
        Log.d(DEBUG_TAG, "ReposFragment onResume");
        // refresh the view (loading data)
        refreshView();
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
    }

    @Override
    public void onDetach() {
        mActivity = null;
        Log.d(DEBUG_TAG, "ReposFragment detached");
        super.onDetach();
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
        //mActivity.disableUpButton();
        if (!Utils.isNetworkOn() || !forceRefresh) {
            List<SeafRepo> repos = getDataManager().getReposFromCache();
            if (repos != null) {
                updateAdapterWithRepos(repos);
                return;
            }
        }

        // load repos in background
        showLoading(true);
        ConcurrentAsyncTask.execute(new LoadTask(getDataManager()));
    }

    public void navToDirectory(final boolean forceRefresh) {
        NavContext nav = getNavContext();
        DataManager dataManager = getDataManager();

        mActivity.enableUpButton();

        SeafRepo repo = getDataManager().getCachedRepoByID(nav.getRepoID());
        if (repo != null) {
            adapter.setEncryptedRepo(repo.encrypted);
            if (nav.getDirPath().equals(BrowserActivity.ACTIONBAE_PARENT_PATH)) {
                mActivity.setUpButtonTitle(nav.getRepoName());
            } else

                mActivity.setUpButtonTitle(nav.getDirPath().substring(
                        nav.getDirPath().lastIndexOf(BrowserActivity.ACTIONBAE_PARENT_PATH) + 1));
        }

        if (!Utils.isNetworkOn() || !forceRefresh) {
            List<SeafDirent> dirents = dataManager.getCachedDirents(
                    nav.getRepoID(), nav.getDirPath());
            if (dirents != null) {
                updateAdapterWithDirents(dirents);
                return;
            }
        }

        showLoading(true);
        ConcurrentAsyncTask.execute(new LoadDirTask(getDataManager()),
                nav.getRepoName(),
                nav.getRepoID(),
                nav.getDirPath());
    }

    private void updateAdapterWithRepos(List<SeafRepo> repos) {
        adapter.clear();
        if (repos.size() > 0) {
            addReposToAdapter(repos);
            adapter.notifyChanged();
            mList.setVisibility(View.VISIBLE);
            mEmptyView.setVisibility(View.GONE);
        } else {
            mList.setVisibility(View.GONE);
            mEmptyView.setText(R.string.no_repo);
            mEmptyView.setVisibility(View.VISIBLE);
        }
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
            // scheduleThumbnailTask(repoName, repoID, dirPath, dirents);
            ConcurrentAsyncTask.execute(new Runnable() {
                @Override
                public void run() {
                    scheduleThumbnailTask(repoName, repoID, dirPath, dirents);
                }
            });

            adapter.notifyChanged();
            mList.setVisibility(View.VISIBLE);
            mEmptyView.setVisibility(View.GONE);
        } else {
            // Directory is empty
            mList.setVisibility(View.GONE);
            mEmptyView.setText(R.string.dir_empty);
            mEmptyView.setVisibility(View.VISIBLE);
        }
    }

    @Override
    public void onListItemClick(final ListView l, final View v, final int position, final long id) {
        SeafRepo repo = null;
        final NavContext nav = getNavContext();
        if (nav.inRepo()) {
            repo = getDataManager().getCachedRepoByID(nav.getRepoID());
            mActivity.setUpButtonTitle(repo.getName());
        } else {
            SeafItem item = adapter.getItem(position);
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

        if (nav.inRepo()) {
            final SeafDirent dirent = (SeafDirent)adapter.getItem(position);
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
        List<SeafRepo> personal = map.get(Utils.NOGROUP);
        SeafGroup group;
        if (personal != null) {
            group = new SeafGroup(mActivity.getResources().getString(R.string.personal));
            adapter.add(group);
            for (SeafRepo repo : personal)
                adapter.add(repo);
        }

        for (Map.Entry<String, List<SeafRepo>> entry : map.entrySet()) {
            String key = entry.getKey();
            if (!key.equals(Utils.NOGROUP)) {
                group = new SeafGroup(key);
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
                err.printStackTrace();
                Log.i(DEBUG_TAG, "failed to load repos: " + err.getMessage());
                showError(R.string.error_when_load_repos);
                return;
            }

            if (rs != null) {
                //Log.d(DEBUG_TAG, "Load repos number " + rs.size());
                updateAdapterWithRepos(rs);
                showLoading(false);
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
                } else if (err.getCode() == HttpURLConnection.HTTP_NOT_FOUND) {
                    mActivity.showToast(String.format("The folder \"%s\" was deleted", myPath));
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

            updateAdapterWithDirents(dirents);
            showLoading(false);
        }
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

    private void scheduleThumbnailTask(String repoName, String repoID,
            String path, List<SeafDirent> dirents) {
        ArrayList<SeafDirent> needThumb = new ArrayList<SeafDirent>();
        for (SeafDirent dirent : dirents) {
            if (dirent.isDir())
                continue;
            if (Utils.isViewableImage(dirent.name)) {
                String p = Utils.pathJoin(path, dirent.name);
                File file = mActivity.getDataManager().getLocalRepoFile(repoName, repoID, p);
                if (file.exists()) {
                    // if (file.length() > 1000000)
                    //     continue;

                    File thumb = DataManager.getThumbFile(dirent.id);
                    if (!thumb.exists())
                        needThumb.add(dirent);
                }
            }
        }
        if (needThumb.size() != 0) {
            ConcurrentAsyncTask.execute(new ThumbnailTask(repoName, repoID, path, needThumb));
        }
    }

    private class ThumbnailTask extends AsyncTask<Void, Void, Void > {

        List<SeafDirent> dirents;
        private String repoName;
        private String repoID;
        private String dir;

        public ThumbnailTask(String repoName, String repoID, String dir, List<SeafDirent> dirents) {
            this.dirents = dirents;
            this.repoName = repoName;
            this.repoID = repoID;
            this.dir = dir;
        }

        @Override
        protected Void doInBackground(Void... params) {
            for (SeafDirent dirent : dirents) {
                String path = Utils.pathJoin(dir, dirent.name);
                mActivity.getDataManager().calculateThumbnail(repoName, repoID, path, dirent.id);
            }
            return null;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(Void v) {
            if (mActivity == null)
                // this occurs if user navigation to another activity
                return;

            adapter.notifyChanged();
        }

    }

}
