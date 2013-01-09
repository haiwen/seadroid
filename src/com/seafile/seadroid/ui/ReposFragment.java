package com.seafile.seadroid.ui;

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
import com.seafile.seadroid.BrowserActivity;
import com.seafile.seadroid.NavContext;
import com.seafile.seadroid.R;
import com.seafile.seadroid.SeafException;
import com.seafile.seadroid.Utils;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.data.SeafDirent;
import com.seafile.seadroid.data.SeafGroup;
import com.seafile.seadroid.data.SeafItem;
import com.seafile.seadroid.data.SeafRepo;
import com.seafile.seadroid.ui.PasswordDialog.PasswordGetListener;


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
        public void onFileSelected(String repoID, String path, SeafDirent dirent);
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
        adapter = new SeafItemAdapter(getActivity());
        setListAdapter(adapter);

        getListView().setChoiceMode(ListView.CHOICE_MODE_SINGLE);
        
        // refresh the view (loading data)
        refreshView();
    }
    

    @Override
    public void onStart() {
        Log.d(DEBUG_TAG, "ReposFragment onStart");
        super.onStart();
    }
    
    @Override
    public void onResume() {
        Log.d(DEBUG_TAG, "ReposFragment onResume");
        super.onResume();
    }
    
    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
    }
    
    @Override
    public void onStop() {
        Log.d(DEBUG_TAG, "ReposFragment onStop");
        super.onStop();
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
        // show cached repos first
        List<SeafRepo> repos = getDataManager().getReposFromCache();
        if  (repos != null) {
            adapter.clear();
            addReposToAdapter(repos);
            adapter.notifyChanged();
        }
        
        // load repos in background
        mActivity.disableUpButton();
        new LoadTask().execute();
    }

    public void navToDirectory() {
        NavContext navContext = getNavContext();
        setListShown(false, true);
        // refresh.setVisibility(View.INVISIBLE);
        mActivity.enableUpButton();
        new LoadDirTask().execute(navContext.getRepo(), navContext.getDirPath(),
                navContext.getDirID());
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
                mActivity.onFileSelected(nav.getRepo(), newPath, dirent);
            }
        } else {
            SeafItem item = adapter.getItem(position);
            if (!(item instanceof SeafRepo))
                return;
            SeafRepo repo = (SeafRepo)item;
            nav.setRepo(repo.id);
            nav.setDir("/", repo.root);
            refreshView();
        }
    }


    private void addReposToAdapter(List<SeafRepo> repos) {
        if (repos == null)
            return;
        Map<String, List<SeafRepo>> map = Utils.groupRepos(repos);
        List<SeafRepo> personal = map.get(Utils.NOGROUP);
        SeafGroup group = new SeafGroup(mActivity.getResources().getString(R.string.personal));
        adapter.add(group);
        for (SeafRepo repo : personal) {
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
        
        @Override
        protected List<SeafRepo> doInBackground(Void... params) {
            try {
                return getDataManager().getRepos();
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
            } else {
                //Log.d(DEBUG_TAG, "failed to load repos");
            }
        }

    }
    
    private class LoadDirTask extends AsyncTask<String, Void, List<SeafDirent> > {

        SeafException err = null;
        String myRepoID;
        String myPath;
        
        @Override
        protected List<SeafDirent> doInBackground(String... params) {
            if (params.length != 3) {
                Log.d(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }
            
            myRepoID = params[0];
            myPath = params[1];
            String objectID = params[2];
            try {
                return getDataManager().getDirents(myRepoID, myPath, objectID);
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
           
            adapter.clear();
            if (dirents != null) {
                for (SeafDirent dirent : dirents) {
                    adapter.add(dirent);
                }
                scheduleThumbnailTask(myRepoID, myPath, dirents);
            } else {
                // refresh.setVisibility(View.VISIBLE);
            }
            adapter.notifyChanged();
            setListShown(true, true);
            
            if (err != null && err.getCode() == 440) {
                showPasswordDialog();
            }
            
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
        if (navContext.getRepo() == null)
            return;
        new SetPasswordTask().execute(navContext.getRepo(), password);
    }
    
    private class SetPasswordTask extends AsyncTask<String, Void, Void > {
        
        @Override
        protected Void doInBackground(String... params) {
            if (params.length != 2) {
                Log.d(DEBUG_TAG, "Wrong params to SetPasswordTask");
                return null;
            }
            
            String repoID = params[0];
            String password = params[1];
            getDataManager().setPassword(repoID, password);
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
    

    private void scheduleThumbnailTask(String repoID, String path,
            List<SeafDirent> dirents) {
        ArrayList<SeafDirent> needThumb = new ArrayList<SeafDirent>();
        for (SeafDirent dirent : dirents) {
            if (dirent.isDir())
                continue;
            if (Utils.isViewableImage(dirent.name)) {
                File file = DataManager.getFileForFileCache(dirent.name, dirent.id);
                if (file.exists()) {
                    if (file.length() > 1000000)
                        continue;
                    
                    File thumb = DataManager.getThumbFile(dirent.name, dirent.id);
                    if (!thumb.exists())
                        needThumb.add(dirent);
                }
            }
        }
        if (needThumb.size() != 0) {
            new ThumbnailTask(repoID, path, needThumb).execute();
        }
    }
    
    private class ThumbnailTask extends AsyncTask<Void, Void, Void > {

        List<SeafDirent> dirents;
        
        public ThumbnailTask(String repoID, String dir, List<SeafDirent> dirents) {
            this.dirents = dirents;
        }
        
        @Override
        protected Void doInBackground(Void... params) {
            for (SeafDirent dirent : dirents) {
                DataManager.calculateThumbnail(dirent.name, dirent.id);
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

