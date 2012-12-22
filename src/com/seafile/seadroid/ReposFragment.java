package com.seafile.seadroid;

import android.os.Bundle;
import android.os.Build;
import android.os.AsyncTask;
import android.util.Log;
import android.view.View;
import android.widget.ListView;
import android.widget.ArrayAdapter;

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import com.actionbarsherlock.app.SherlockListFragment;


public class ReposFragment extends SherlockListFragment {

    private static final String DEBUG_TAG = "FilesActivity";

    private ArrayList<String> libraries = new ArrayList<String>();
    private ArrayAdapter<String> adapter;
    boolean mDualPane;
    
    private String server;
    private SeafConnection sc;
    
    List<SeafRepo> repos = null;
    NavContext navContext;
    
    BrowserActivity activity = null;
    
    public interface OnFileSelectedListener {
        public void onFileSelected(SeafRepo repo, String path);
    }
    
    public class NavContext {
        
        SeafRepo currentRepo;
        List<SeafDirent> currentDirents;
        String currentPath;
        
        public NavContext() {
            currentRepo = null;
            currentDirents = null;
            currentPath = null;
        }
     
        public void clear() {
            currentRepo = null;
            currentDirents = null;
            currentPath = null;
        }
        
        public boolean inRepo() {
            return currentRepo != null;
        }
        
        public SeafRepo getCurrentRepo() {
            return currentRepo;
        }
        
        public SeafDirent getDirent(int position) {
            return currentDirents.get(position);
        }
        
        public boolean isDir(int position) {
            return currentDirents.get(position).isDir();
        }
        
        public String getPathAtPosition(int position) {
            return currentPath + "/" + currentDirents.get(position).name;
        }
        
        public boolean isRootDir() {
            return currentPath.equals("/");
        }
        
        public String getParentPath() {
            return currentPath.substring(0, currentPath.lastIndexOf("/"));
        }

        public String getObjectIDAtPosition(int position) {
            return currentDirents.get(position).id;
        }
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
        
        activity = (BrowserActivity) getActivity();
        server = activity.getServer();
        sc = SeafConnection.getSeafConnection(server);
        navContext = new NavContext();
        
        navToReposView();
    }

    @Override 
    public void onListItemClick(ListView l, View v, int position, long id) {
        //Log.d(DEBUG_TAG, "click pos " + position + " id " + id);
        
        if (navContext.inRepo()) {
            if (navContext.isDir(position)) {
                navToDirectory(navContext.getCurrentRepo(),
                        navContext.getPathAtPosition(position));
            } else {
                showFile(navContext.getCurrentRepo(), 
                        navContext.getPathAtPosition(position),
                        navContext.getObjectIDAtPosition(position));
            }
        } else {
            SeafRepo repo = repos.get(position);
            navToDirectory(repo, "/");
        }
    }

    
    public void navToReposView() {
        activity.setRefreshing();
        getListView().setEnabled(false);
        new LoadTask().execute();
    }

    public void navToDirectory(SeafRepo repo, String path) {
        activity.setRefreshing();
        getListView().setEnabled(false);
        navContext.currentRepo = repo;
        navContext.currentPath = path;
        new LoadDirTask().execute(repo.id, path);
    }
    
    public void navUp() {
        if (navContext.inRepo()) {
            if (navContext.isRootDir()) {
                navToReposView();
            } else {
                navToDirectory(navContext.currentRepo, navContext.getParentPath());
            }
        }
    }
    
    private void showFile(SeafRepo repo, String path, String oid) {
        activity.setRefreshing();
        getListView().setEnabled(false);
        new LoadFileTask().execute(repo.id, path, oid);
    }

    private class LoadTask extends AsyncTask<Void, Void, List<SeafRepo> > {

        @Override
        protected List<SeafRepo> doInBackground(Void... params) {
            List<SeafRepo> repos = sc.getRepos();
            return repos;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafRepo> rs) {
            adapter.clear();
            repos = rs;
            navContext.clear();
            if (repos != null) {
                for (SeafRepo repo : repos) {
                    adapter.add(repo.name);
                }
            }
            getListView().setEnabled(true);
            activity.unsetRefreshing();
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
            List<SeafDirent> dirents = sc.getDirents(repoID, path);
            return dirents;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(List<SeafDirent> dirents) {
            adapter.clear();
            
            if (dirents != null) {
                navContext.currentDirents = dirents;
                for (SeafDirent dirent : navContext.currentDirents) {
                    adapter.add(dirent.name);
                }
            }
            activity.unsetRefreshing();
            getListView().setEnabled(true);
        }

    }
    
    private class LoadFileTask extends AsyncTask<String, Void, File> {

        @Override
        protected File doInBackground(String... params) {
            if (params.length != 3) {
                Log.d(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }
            
            String repoID = params[0];
            String path = params[1];
            String oid = params[2];
            File file = sc.getFile(repoID, path, oid);
            return file;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(File file) {
            CharSequence text;
            if (file != null) {
                text = file.getName();
            } else {
                text = "Failed to load file";
            }
            activity.showToast(text);
            activity.unsetRefreshing();
            getListView().setEnabled(true);
        }

    }

}

