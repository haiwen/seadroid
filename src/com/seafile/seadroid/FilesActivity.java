package com.seafile.seadroid;

import android.os.Bundle;
import android.os.Build;
import android.os.AsyncTask;
import android.util.Log;
import android.view.View;
import android.widget.ListView;
import android.widget.ArrayAdapter;
import android.content.Intent;

import java.util.ArrayList;
import java.util.List;
import com.actionbarsherlock.app.SherlockListActivity;
import com.actionbarsherlock.view.Menu;
import com.actionbarsherlock.view.MenuItem;

public class FilesActivity extends SherlockListActivity {

    private static final String DEBUG_TAG = "FilesActivity";

    private ArrayList<String> libraries = new ArrayList<String>();
    private ArrayAdapter<String> adapter;
    
    private String server;
    private SeafConnection sc;
    
    List<SeafRepo> repos = null;
    NavContext navContext;
    
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
    }
    
    

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Get the message from the intent
        Intent intent = getIntent();
        server = intent.getStringExtra("server");
        sc = SeafConnection.getSeafConnection(server);
        navContext = new NavContext();
        
        getSupportActionBar().setDisplayHomeAsUpEnabled(true);
        
        // // Create the text view
        // TextView textView = new TextView(this);
        // textView.setTextSize(40);

        // // Set the text view as the activity layout
        // setContentView(textView);

        // We need to use a different list item layout for devices older than Honeycomb
        int layout = Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB ?
            android.R.layout.simple_list_item_activated_1 : android.R.layout.simple_list_item_1;
        adapter = new ArrayAdapter<String>(this, layout, libraries);
        setListAdapter(adapter);
            
        navToReposView();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        return true;
    }
    
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
        switch (item.getItemId()) {
            case android.R.id.home:
                navUp();
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

    @Override 
    public void onListItemClick(ListView l, View v, int position, long id) {
        Log.d(DEBUG_TAG, "click pos " + position + " id " + id);
        
        if (navContext.inRepo()) {
            if (navContext.isDir(position)) {
                navToDirectory(navContext.getCurrentRepo(),
                        navContext.getPathAtPosition(position));
            }
        } else {
            SeafRepo repo = repos.get(position);
            navToDirectory(repo, "/");
        }
    }
    
    private void navToReposView() {
        getListView().setEnabled(false);
        new LoadTask().execute();
    }

    private void navToDirectory(SeafRepo repo, String path) {
        getListView().setEnabled(false);
        navContext.currentRepo = repo;
        navContext.currentPath = path;
        new LoadDirTask().execute(repo.id, path);
    }
    
    private void navUp() {
        if (navContext.inRepo()) {
            if (navContext.isRootDir()) {
                navToReposView();
            } else {
                navToDirectory(navContext.currentRepo, navContext.getParentPath());
            }
        }
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
            
            getListView().setEnabled(true);
        }

    }

}
