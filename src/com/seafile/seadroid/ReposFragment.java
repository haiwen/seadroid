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

    private ArrayList<String> libraries = new ArrayList<String>();
    private ArrayAdapter<String> adapter;
    boolean mDualPane;
    
    private BrowserActivity getMyActivity() {
        return (BrowserActivity) getActivity();
    }
    
    private SeafConnection getConnection() {
        return SeafConnection.getSeafConnection(getMyActivity().getServer());
    }
    
    private NavContext getNavContext() {
        return getMyActivity().getNavContext();
    }
    
    public interface OnFileSelectedListener {
        public void onFileSelected(String repoID, String path, String objectID);
    }
    
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);

        int layout = Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB ?
                android.R.layout.simple_list_item_activated_1 : android.R.layout.simple_list_item_1;
        adapter = new ArrayAdapter<String>(getActivity(), layout, libraries);
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
                        getNavContext().getObjectIDAtPosition(position));
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
            adapter.clear();
            getNavContext().clear();
            getNavContext().repos = rs;
            
            if (rs != null) {
                for (SeafRepo repo : rs) {
                    adapter.add(repo.name);
                }
            }
            getListView().setEnabled(true);
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
            adapter.clear();
            
            if (dirents != null) {
                getNavContext().currentDirents = dirents;
                for (SeafDirent dirent : dirents) {
                    adapter.add(dirent.name);
                }
            }
            getMyActivity().unsetRefreshing();
            getListView().setEnabled(true);
        }

    }
    
    

}

