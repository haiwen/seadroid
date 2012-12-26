package com.seafile.seadroid;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.MimeTypeMap;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;

import com.actionbarsherlock.app.SherlockFragment;

public class FileFragment extends SherlockFragment {

    private static final String DEBUG_TAG = "FileFragment";

    String repoID;
    String path;
    String objectID;
    long size;
    File file;
    String name;
    ViewGroup rootView = null;
    
    private ImageView ivFileIcon;
    private TextView tv_filename;
    private TextView tv_file_size;
    
    private BrowserActivity getMyActivity() {
        return (BrowserActivity) getActivity();
    }
    
    private SeafConnection getConnection() {
        return SeafConnection.getSeafConnection(getMyActivity().getAccount());
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
        if (args == null)
            return;
        
        repoID = args.getString("repoID");
        path = args.getString("path");
        objectID = args.getString("objectID");
        size = args.getLong("size");

        if (shouldDownloadInstantly(path))
            downloadFile();
        else {
            LayoutInflater inflater = (LayoutInflater) getActivity().getSystemService
                    (Context.LAYOUT_INFLATER_SERVICE);
            inflater.inflate(R.layout.file_details, rootView, true);
                
            ivFileIcon = (ImageView) getActivity().findViewById(R.id.iv_file_icon);
            tv_filename = (TextView) getActivity().findViewById(R.id.tv_file_name);
            tv_file_size = (TextView) getActivity().findViewById(R.id.tv_file_size);
            tv_filename.setText(Utils.fileNameFromPath(path));
            tv_file_size.setText(Utils.readableFileSize(size));
        }
    }
    
    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);

        // Save the current article selection in case we need to recreate the fragment
        outState.putString("repoID", repoID);
        outState.putString("path", path);
        outState.putString("objectID", objectID);
        outState.putLong("size", size);
    }
    
    private boolean shouldDownloadInstantly(String path) {
        String suffix = path.substring(path.lastIndexOf('.') + 1);
        
        if (suffix == null)
            return false;
        if (suffix.length() == 0)
            return false;
        if (suffix.equals("md"))
            return true;
        return false;
    }
    
    public void updateFileView(String repoID, String path, SeafDirent dirent) {
        String suffix = path.substring(path.lastIndexOf('.') + 1);
        this.repoID = repoID;
        this.path = path;
        this.objectID = dirent.id;
        this.size = dirent.size;
        
        if (shouldDownloadInstantly(suffix))
            downloadFile();
        else {
            //
        }
    }
    
    private void downloadFile() {
        getMyActivity().setRefreshing();
        new LoadFileTask().execute(repoID, path, objectID);
    }
    
    public void openFile() {
        // login
        downloadFile();
    }
    
    private void showFile(File file) {
        this.file = file;
        String suffix = path.substring(path.lastIndexOf('.') + 1);
        
        if (suffix.length() == 0) {
            getMyActivity().showToast(getString(R.string.unknown_file_type));
            return;
        }
        
        if (suffix.equals("md")) {
            showMarkdown(file);
            return;
        }
     
        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        Intent open = new Intent(Intent.ACTION_VIEW, Uri.parse(file.getAbsolutePath()));
        open.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        open.setAction(android.content.Intent.ACTION_VIEW);
        open.setDataAndType((Uri.fromFile(file)), mime);
        try {
            startActivity(open);
        } catch (ActivityNotFoundException e) {
            getMyActivity().showToast(getString(R.string.activity_not_found));
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
            Log.d(DEBUG_TAG, "load file " + repoID + ":" + path);
            File file = DataManager.getFile(path, oid);
            if (file.exists())
                return file;
            
            file = getConnection().getFile(repoID, path, oid);
            return file;
        }

        // onPostExecute displays the results of the AsyncTask.
        @Override
        protected void onPostExecute(File file) {
            if (getMyActivity() == null)
                return;
            
            CharSequence text;
            if (file != null) {
                text = file.getName();
                showFile(file);
            } else {
                text = "Failed to load file";
                getMyActivity().showToast(text);
            }

            getMyActivity().unsetRefreshing();
        }

    }
}
