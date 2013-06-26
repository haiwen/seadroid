package com.seafile.seadroid2.ui;

import android.app.Activity;
import android.os.Bundle;
import android.os.AsyncTask;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.animation.AnimationUtils;
import android.widget.ListView;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import com.actionbarsherlock.app.SherlockListFragment;
import com.seafile.seadroid2.BrowserActivity;
import com.seafile.seadroid2.ConcurrentAsyncTask;
import com.seafile.seadroid2.NavContext;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.Utils;
import com.seafile.seadroid2.data.*;
import com.seafile.seadroid2.ui.PasswordDialog.PasswordGetListener;


public class ReposFragment extends SherlockListFragment implements PasswordGetListener {

    private static final String DEBUG_TAG = "ReposFragment";

    private SeafItemAdapter adapter;
    boolean mDualPane;
    BrowserActivity mActivity = null;

    public ListView mList;
    boolean mListShown;
    View mProgressContainer;
    View mListContainer;

    public void setListShown(boolean shown, boolean animate) {
        if (mListShown == shown) {
            return;
        }
        mListShown = shown;
        if (shown) {
            if (animate) {
                mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                        mActivity, android.R.anim.fade_out));
                mListContainer.startAnimation(AnimationUtils.loadAnimation(
                        mActivity, android.R.anim.fade_in));
            }
            mProgressContainer.setVisibility(View.GONE);
            mListContainer.setVisibility(View.VISIBLE);
        } else {
            if (animate) {
                mProgressContainer.startAnimation(AnimationUtils.loadAnimation(
                        mActivity, android.R.anim.fade_in));
                mListContainer.startAnimation(AnimationUtils.loadAnimation(
                        mActivity, android.R.anim.fade_out));
            }
            mProgressContainer.setVisibility(View.VISIBLE);
            mListContainer.setVisibility(View.INVISIBLE);
        }
    }

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
        public void onFileSelected(String repoName, String repoID, String path, SeafDirent dirent);
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
        int INTERNAL_EMPTY_ID = 0x00ff0001;
        View root = inflater.inflate(R.layout.repos_fragment, container, false);
        (root.findViewById(R.id.internalEmpty)).setId(INTERNAL_EMPTY_ID);
        mList = (ListView) root.findViewById(android.R.id.list);
        mListContainer =  root.findViewById(R.id.listContainer);
        mProgressContainer = root.findViewById(R.id.progressContainer);
        mListShown = true;

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
        if (mActivity == null)
            return;

        NavContext navContext = getNavContext();
        if (navContext.inRepo()) {
            navToDirectory();
        } else {
            navToReposView();
        }
        mActivity.invalidateOptionsMenu();
    }

    public void navToReposView() {
        setListShown(false, true);
        // show cached repos first
        List<SeafRepo> repos = getDataManager().getReposFromCache();
        if  (repos != null) {
            adapter.clear();
            addReposToAdapter(repos);
            adapter.notifyChanged();
        }

        // load repos in background
        mActivity.disableUpButton();
        ConcurrentAsyncTask.execute(new LoadTask(getDataManager()));
        updateActionBarTitle();
    }

    public void navToDirectory() {
        NavContext navContext = getNavContext();
        setListShown(false, true);
        // refresh.setVisibility(View.INVISIBLE);
        mActivity.enableUpButton();
        ConcurrentAsyncTask.execute(new LoadDirTask(getDataManager()),
                                    navContext.getRepoName(),
                                    navContext.getRepoID(),
                                    navContext.getDirPath(),
                                    navContext.getDirID());

        updateActionBarTitle();
    }

    private void updateActionBarTitle() {
        NavContext navContext = getNavContext();
        if (!navContext.inRepo()) {
            mActivity.setActionBarTitle("", "");
        } else {

            String title = navContext.getRepoName();
            String dirPath = navContext.getDirPath();
            if (dirPath.equals("/")) {
                mActivity.setActionBarTitle(title, "");
            } else {
                mActivity.setActionBarTitle(title, Utils.fileNameFromPath(dirPath));
            }
        }
    }

    @Override
    public void onListItemClick(ListView l, View v, int position, long id) {
        //Log.d(DEBUG_TAG, "click pos " + position + " id " + id);

        NavContext nav = getNavContext();
        if (nav.inRepo()) {
            SeafDirent dirent = (SeafDirent)adapter.getItem(position);
            if (dirent.isDir()) {
                String currentPath = nav.getDirPath();
                String newPath = currentPath.endsWith("/") ?
                        currentPath + dirent.name : currentPath + "/" + dirent.name;
                nav.setDir(newPath, dirent.id);
                refreshView();
            } else {
                String currentPath = nav.getDirPath();
                String newPath = currentPath.endsWith("/") ?
                        currentPath + dirent.name : currentPath + "/" + dirent.name;
                mActivity.onFileSelected(nav.getRepoName(), nav.getRepoID(), newPath, dirent);
            }
        } else {
            SeafItem item = adapter.getItem(position);
            if (!(item instanceof SeafRepo))
                return;
            SeafRepo repo = (SeafRepo)item;
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
                return dataManager.getRepos();
            } catch (SeafException e) {
                err = e;
                return null;
            }
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

            if (rs != null) {
                //Log.d(DEBUG_TAG, "Load repos number " + rs.size());
                adapter.clear();
                addReposToAdapter(rs);
                adapter.notifyChanged();
                setListShown(true, false);
            } else {
                Log.d(DEBUG_TAG, "failed to load repos");
            }
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
            if (params.length != 4) {
                Log.d(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }

            myRepoName = params[0];
            myRepoID = params[1];
            myPath = params[2];
            String objectID = params[3];
            try {
                return dataManager.getDirents(myRepoID, myPath, objectID);
            } catch (SeafException e) {
                err = e;
                return null;
            }

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

            adapter.clear();
            if (dirents != null) {
                for (SeafDirent dirent : dirents) {
                    adapter.add(dirent);
                }
                scheduleThumbnailTask(myRepoName, myRepoID, myPath, dirents);
            } else {
                // refresh.setVisibility(View.VISIBLE);
            }
            adapter.notifyChanged();
            setListShown(true, true);

            if (err != null) {
                if (err.getCode() == 440) {
                    showPasswordDialog();
                } else if (err.getCode() == 404) {
                    mActivity.showToast("The directory may be deleted");
                }
            }

            if (dirents != null) {
                String fn = mActivity.getNavContext().getFileName();
                if (fn != null) {
                    scrollToFile(dirents, fn);
                }
            }
        }

    }

    private void scrollToFile(List<SeafDirent> dirents, String fn) {
        int i = 0, n = dirents.size();
        int id = -1;
        while (i < n) {
            SeafDirent dent = dirents.get(i);
            if (dent.name.equals(fn)) {
                id = i;
                break;
            }
            ++i;
        }

        if (id >= 0) {
            getListView().smoothScrollToPosition(id);
        } else {
            mActivity.showToast("Could not find file " + fn);
        }
    }

    private void showPasswordDialog() {
        PasswordDialog dialog = new PasswordDialog();
        dialog.setPasswordGetListener(this);
        dialog.show(mActivity.getSupportFragmentManager(), "DialogFragment");
    }

    @Override
    public void onPasswordGet(String password) {
        if (password.length() == 0)
            return;
        NavContext navContext = getNavContext();
        if (navContext.getRepoID() == null)
            return;
        ConcurrentAsyncTask.execute(new SetPasswordTask(getDataManager()),
                            navContext.getRepoID(), password);
    }

    private class SetPasswordTask extends AsyncTask<String, Void, Void > {

        DataManager dataManager;

        public SetPasswordTask(DataManager dataManager) {
            this.dataManager = dataManager;
        }

        @Override
        protected Void doInBackground(String... params) {
            if (params.length != 2) {
                Log.d(DEBUG_TAG, "Wrong params to SetPasswordTask");
                return null;
            }

            String repoID = params[0];
            String password = params[1];
            dataManager.setPassword(repoID, password);
            return null;
        }

        @Override
        protected void onPostExecute(Void v) {
            if (mActivity == null)
                // this occurs if user navigation to another activity
                return;
            refreshView();
        }

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
                    if (file.length() > 1000000)
                        continue;

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

