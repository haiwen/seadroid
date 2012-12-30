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
import java.util.Map;

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

        getListView().setChoiceMode(ListView.CHOICE_MODE_SINGLE);
 
        // set refresh button
        refresh = LayoutInflater.from(getActivity()).inflate(R.layout.refresh, null);
        getListView().setEmptyView(refresh);
        ViewGroup root = (ViewGroup) getActivity().findViewById(android.R.id.content);
        root.addView(refresh);
        
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
        NavContext navContext = getNavContext();
        if (navContext.inRepo()) {
            navToDirectory();
        } else {
            navToReposView();
        }
    }
    
    public void navToReposView() {
        // show cached repos first
        List<SeafRepo> repos = getDataManager().getReposFromCache();
        adapter.clear();
        addReposToAdapter(repos);
        adapter.notifyChanged();
        
        // load repos in background
        mActivity.setRefreshing();
        // refresh.setVisibility(View.INVISIBLE);
        mActivity.disableUpButton();
        new LoadTask().execute();
    }

    public void navToDirectory() {
        NavContext navContext = getNavContext();
        mActivity.setRefreshing();
        refresh.setVisibility(View.INVISIBLE);
        getListView().setEnabled(false);
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
                navToDirectory();
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
            navToDirectory();
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
            
            if (getNavContext().inRepo()) {
                // this occurs if user already navigate into a repo
                return;
            }
            
            if (rs != null) {
                Log.d(DEBUG_TAG, "Load repos number " + rs.size());
                adapter.clear();
                addReposToAdapter(rs);
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

