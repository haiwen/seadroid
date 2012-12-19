package com.seafile.seadroid;

import android.app.ListActivity;
import android.os.Bundle;
import android.os.Build;
import android.os.AsyncTask;
import android.view.View;
import android.widget.ListView;
import android.widget.ArrayAdapter;
import android.content.Intent;

import java.util.ArrayList;
import java.util.List;


public class FilesActivity extends ListActivity {


    private ArrayList<String> libraries = new ArrayList<String>();
    private ArrayAdapter<String> adapter;
    
    private String server;
    private SeafConnection sc;
    
    List<SeafRepo> repos = null;

    /** Called when the activity is first created. */
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Get the message from the intent
        Intent intent = getIntent();
        server = intent.getStringExtra("server");
        sc = SeafConnection.getSeafConnection(server);
        
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
        // Do something when a list item is clicked
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
            for (SeafRepo repo : repos) {
                adapter.add(repo.name);
            }
        }

    }

}
