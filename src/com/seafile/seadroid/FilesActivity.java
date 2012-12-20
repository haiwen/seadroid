package com.seafile.seadroid;

import android.app.ListActivity;
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


public class FilesActivity extends ListActivity {

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
            
        new LoadTask().execute();
    }


    @Override 
    public void onListItemClick(ListView l, View v, int position, long id) {
        Log.d(DEBUG_TAG, "click pos " + position + " id " + id);
        
        if (navContext.inRepo()) {
            
        } else {
            getListView().setEnabled(false);
            SeafRepo repo = repos.get(position);
            new LoadDirTask().execute(repo.id, "/");
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
            for (SeafRepo repo : repos) {
                adapter.add(repo.name);
            }
        }

    }
    
    private class LoadDirTask extends AsyncTask<String, Void, List<SeafDirent> > {

        @Override
        protected List<SeafDirent> doInBackground(String... params) {
            if (params.length != 2) {
                Log.d(DEBUG_TAG, "Wrong params to LoadDirTask");
                return null;
            }
            
            String repo_id = params[0];
            String path = params[1];
            List<SeafDirent> dirents = sc.getDirents(repo_id, path);
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
