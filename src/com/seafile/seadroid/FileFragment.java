package com.seafile.seadroid;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;

import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import com.actionbarsherlock.app.SherlockFragment;

public class FileFragment extends SherlockFragment {

    private static final String DEBUG_TAG = "FileFragment";

    String repoID;
    String path;
    String objectID;
    File file;
    String name;
    ViewGroup rootView = null;
    
    private BrowserActivity getMyActivity() {
        return (BrowserActivity) getActivity();
    }
    
    private SeafConnection getConnection() {
        return SeafConnection.getSeafConnection(getMyActivity().getServer());
    }
    
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, 
        Bundle savedInstanceState) {

        // If activity recreated (such as from screen rotate), restore
        // the previous article selection set by onSaveInstanceState().
        // This is primarily necessary when in the two-pane layout.
        if (savedInstanceState != null) {
            //
        }

        // Inflate the layout for this fragment
        rootView = new LinearLayout(getActivity());
        return rootView;
    }
    
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
    }
    
    @Override
    public void onStart() {
        super.onStart();
        
        Bundle args = getArguments();
        if (args != null) {
            repoID = args.getString("repoID");
            path = args.getString("path");
            objectID = args.getString("objectID");
            downloadFile(repoID, path, objectID);
        }
        
    }
    
    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);

        // Save the current article selection in case we need to recreate the fragment
        outState.putString("repoID", repoID);
        outState.putString("path", path);
        outState.putString("objectID", objectID);
    }
    
    
    
    public void updateFileView(String repoID, String path, String objectID) {
        downloadFile(repoID, path, objectID);
    }
    
    private void downloadFile(String repoID, String path, String oid) {
        getMyActivity().setRefreshing();
        new LoadFileTask().execute(repoID, path, oid);
    }
    
    private void showFile(File file) {
        this.file = file;
        String suffix = path.substring(path.lastIndexOf('.') + 1);
        if (suffix.length() > 0) {
            if (suffix.equals("md")) {
                showMarkdown(file);
            }
        }
    }
    
    private void showMarkdown(File file) {
        InputStream in = null;
        try {
            in = new FileInputStream(file);
            String content = Utils.readIt(in);
            ScrollView scroller = new ScrollView(getActivity());
            TextView text = new TextView(getActivity());
            int padding = (int)TypedValue.applyDimension(TypedValue.COMPLEX_UNIT_DIP,
                    4, getActivity().getResources().getDisplayMetrics());
            text.setPadding(padding, padding, padding, padding);
            scroller.addView(text);
            text.setText(content);
            rootView.removeAllViews();
            rootView.addView(scroller);
        } catch (FileNotFoundException e) {
            Log.d(DEBUG_TAG, e.getMessage());
        } catch (IOException e) {
            Log.d(DEBUG_TAG, e.getMessage());
        } finally {
            try {
                if (in != null) in.close();
            } catch (IOException e) {
                // ignore
            }
        }
        
    }

    private class LoadFileTask extends AsyncTask<String, Void, File> {

        @Override
        protected File doInBackground(String... params) {
            if (params.length != 3) {
                Log.d(DEBUG_TAG, "Wrong params to LoadFileTask");
                return null;
            }
            
            String repoID = params[0];
            String path = params[1];
            String oid = params[2];
            File file = getConnection().getFile(repoID, path, oid);
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
            showFile(file);
            getMyActivity().showToast(text);
            getMyActivity().unsetRefreshing();
        }

    }
}
