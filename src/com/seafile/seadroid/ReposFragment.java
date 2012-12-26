package com.seafile.seadroid;

import android.os.Bundle;
import android.os.Build;
import android.os.AsyncTask;
import android.util.Log;
import android.view.View;
import android.widget.ListView;
import android.widget.ArrayAdapter;

import java.util.ArrayList;
import java.util.List;
import com.actionbarsherlock.app.SherlockListFragment;


public class ReposFragment extends SherlockListFragment {

    private static final String DEBUG_TAG = "ReposFragment";

    private SeafItemAdapter adapter;
    boolean mDualPane;
    
    private BrowserActivity getMyActivity() {
        return (BrowserActivity) getActivity();
    }
    
    private SeafConnection getConnection() {
        return SeafConnection.getSeafConnection(getMyActivity().getAccount());
    }
    
    private NavContext getNavContext() {
        return getMyActivity().getNavContext();
    }
    
    public interface OnFileSelectedListener {
        public void onFileSelected(String repoID, String path, SeafDirent dirent);
    }
    
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);

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
        
        Log.d(DEBUG_TAG, "onActivityCreated");
        NavContext navContext = getNavContext();
        if (navContext.inRepo())
            navToDirectory(navContext.currentRepo, navContext.currentPath);
        else
            navToReposView();
    }
    
    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
    }

    @Override 
    public void onListItemClick(ListView l, View v, int position, long id) {
        //Log.d(DEBUG_TAG, "click pos " + position + " id " + id);
        
        if (getNavContext().inRepo()) {
            if (getNavContext().isDir(position)) {
                navToDirectory(getNavContext().getCurrentRepo(),
                        getNavContext().getPathAtPosition(position));
            } else {
                getMyActivity().onFileSelected(getNavContext().getCurrentRepo(), 
                        getNavContext().getPathAtPosition(position),
                        getNavContext().getDirent(position));
            }
        } else {
            navToDirectory(getNavContext().getRepoAtPosition(position), "/");
        }
    }

    
    public void navToReposView() {
        getMyActivity().setRefreshing();
        getListView().setEnabled(false);
        getMyActivity().disableUpButton();
        new LoadTask().execute();
    }

    public void navToDirectory(String repoID, String path) {
        getMyActivity().setRefreshing();
        getListView().setEnabled(false);
        getMyActivity().enableUpButton();
        getNavContext().currentRepo = repoID;
        getNavContext().currentPath = path;
        new LoadDirTask().execute(repoID, path);
    }
    
    public void navUp() {
        if (getNavContext().inRepo()) {
            if (getNavContext().isRootDir()) {
                navToReposView();
            } else {
                navToDirectory(getNavContext().currentRepo, getNavContext().getParentPath());
            }
        }
    }
    
    

    private class LoadTask extends AsyncTask<Void, Void, List<SeafRepo> > {

        @Override
        protected List<SeafRepo> doInBackground(Void... params) {
            List<SeafRepo> repos = getConnection().getRepos();
            return repos;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafRepo> rs) {   
            getNavContext().clear();
            getNavContext().repos = rs;

            adapter.clear();
            if (rs != null) {
                Log.d(DEBUG_TAG, "load repos " + rs.size());
                for (SeafRepo repo : rs) {
                    adapter.add(repo);
                }
            } else {
                Log.d(DEBUG_TAG, "failed to load repos");
            }
            getListView().setEnabled(true);
            adapter.notifyChanged();
            getMyActivity().unsetRefreshing();
        }

    }
    
    private class LoadDirTask extends AsyncTask<String, Void, List<SeafDirent> > {

        @Override
        protected List<SeafDirent> doInBackground(String... params) {
            if (params.length != 2) {
                Log.d(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }
            
            String repoID = params[0];
            String path = params[1];
            List<SeafDirent> dirents = getConnection().getDirents(repoID, path);
            return dirents;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafDirent> dirents) {
            if (getMyActivity() == null)
                // this occurs if user navigation to another activity
                return;
            
            adapter.clear();
            if (dirents != null) {
                getNavContext().currentDirents = dirents;
                for (SeafDirent dirent : dirents) {
                    adapter.add(dirent);
                }
            }
            getMyActivity().unsetRefreshing();
            adapter.notifyChanged();
            getListView().setEnabled(true);
        }

    }
    
    

}

