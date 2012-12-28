package com.seafile.seadroid;

import android.app.Activity;
import android.os.Bundle;
import android.os.AsyncTask;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ListView;

import java.util.List;
import com.actionbarsherlock.app.SherlockListFragment;


public class ReposFragment extends SherlockListFragment {

    private static final String DEBUG_TAG = "ReposFragment";

    private SeafItemAdapter adapter;
    boolean mDualPane;
    View refresh = null;
    BrowserActivity mActivity = null;
    
    private DataManager getDataManager() {
        return mActivity.getDataManager();
    }
    
    private NavContext getNavContext() {
        return mActivity.getNavContext();
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
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        Log.d(DEBUG_TAG, "ReposFragment onActivityCreated");
        adapter = new SeafItemAdapter(getActivity());
        setListAdapter(adapter);

        // Check to see if we have a frame in which to embed the details
        // fragment directly in the containing UI.
        View detailsFrame = getActivity().findViewById(R.id.file_fragment);
        mDualPane = detailsFrame != null && detailsFrame.getVisibility() == View.VISIBLE;
        if (mDualPane) {
            // In dual-pane mode, the list view highlights the selected item.
            getListView().setChoiceMode(ListView.CHOICE_MODE_SINGLE);
            // Make sure our UI is in the correct state.
            //showDetails(mCurCheckPosition);
        }
        
        // set refresh button
        refresh = LayoutInflater.from(getActivity()).inflate(R.layout.refresh, null);
        getListView().setEmptyView(refresh);
        ViewGroup root = (ViewGroup) getActivity().findViewById(android.R.id.content);
        root.addView(refresh);

        NavContext navContext = getNavContext();
        String repoID = navContext.getCurrentDirRepo();
        String path = navContext.getCurrentDirPath();

        if (repoID != null) {
            navToDirectory(repoID, path, null);
        } else
            navToReposView();
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

    @Override 
    public void onListItemClick(ListView l, View v, int position, long id) {
        //Log.d(DEBUG_TAG, "click pos " + position + " id " + id);
        
        NavContext nav = getNavContext();
        if (nav.inRepo()) {
            if (nav.isDir(position)) {
                SeafDirent d = nav.getDirent(position);
                String currentPath = nav.getCurrentDirPath();
                String newPath = currentPath.endsWith("/") ? 
                        currentPath + d.name : currentPath + "/" + d.name;
                navToDirectory(nav.getCurrentDirRepo(), newPath, d.id);
            } else {
                SeafDirent d = nav.getDirent(position);
                String currentPath = nav.getCurrentDirPath();
                String newPath = currentPath.endsWith("/") ? 
                        currentPath + d.name : currentPath + "/" + d.name;
                mActivity.onFileSelected(nav.getCurrentDirRepo(), newPath, d);
            }
        } else {
            SeafRepo repo = getDataManager().getCachedRepo(position);
            navToDirectory(repo.id, "/", repo.root);
        }
    }


    public void navToReposView() {
        // show cached repos first
        List<SeafRepo> repos = getDataManager().getReposFromCache();
        adapter.clear();
        if (repos != null) {
            for (SeafRepo repo : repos) {
                adapter.add(repo);
            }
        }
        adapter.notifyChanged();
        
        // load repos in background
        mActivity.setRefreshing();
        // refresh.setVisibility(View.INVISIBLE);
        mActivity.disableUpButton();
        getNavContext().clear();
        new LoadTask().execute();
    }

    public void navToDirectory(String repoID, String path, String objectID) {
        mActivity.setRefreshing();
        refresh.setVisibility(View.INVISIBLE);
        getListView().setEnabled(false);
        mActivity.enableUpButton();
        getNavContext().setCurrentDirRepo(repoID);
        getNavContext().setCurrentDir(path);
        new LoadDirTask().execute(repoID, path, objectID);
    }
    
    public void navUp() {
        if (getNavContext().inRepo()) {
            if (getNavContext().isRootDir()) {
                navToReposView();
            } else {
                navToDirectory(getNavContext().currentRepo, 
                        getNavContext().getParentPath(), null);
            }
        }
    }

    private class LoadTask extends AsyncTask<Void, Void, List<SeafRepo> > {

        @Override
        protected List<SeafRepo> doInBackground(Void... params) {
            List<SeafRepo> repos = getDataManager().getRepos();
            return repos;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafRepo> rs) {
            if (mActivity == null)
                // this occurs if user navigation to another activity
                return;
            
            if (rs != null) {
                adapter.clear();
                for (SeafRepo repo : rs) {
                    adapter.add(repo);
                }
                adapter.notifyChanged();
            } else {
                Log.d(DEBUG_TAG, "failed to load repos");
            }
            
            mActivity.unsetRefreshing();
        }

    }
    
    private class LoadDirTask extends AsyncTask<String, Void, List<SeafDirent> > {

        @Override
        protected List<SeafDirent> doInBackground(String... params) {
            if (params.length != 3) {
                Log.d(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }
            
            String repoID = params[0];
            String path = params[1];
            String objectID = params[2];
            List<SeafDirent> dirents = getDataManager().getDirents(repoID, path, objectID);
            return dirents;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafDirent> dirents) {
            if (mActivity == null)
                // this occurs if user navigation to another activity
                return;
           
            adapter.clear();
            if (dirents != null) {
                getNavContext().currentDirents = dirents;
                for (SeafDirent dirent : dirents) {
                    adapter.add(dirent);
                }
            }
            refresh.setVisibility(View.VISIBLE);
            mActivity.unsetRefreshing();
            adapter.notifyChanged();
            getListView().setEnabled(true);
        }

    }
    
    

}

