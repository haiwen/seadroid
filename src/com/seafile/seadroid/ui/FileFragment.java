package com.seafile.seadroid.ui;

import java.io.File;
import java.util.ArrayList;

import android.app.Activity;
import android.app.Notification;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.AsyncTask;
import android.os.Bundle;
import android.support.v4.app.NotificationCompat;
import android.util.Log;
import android.util.TypedValue;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.webkit.MimeTypeMap;
import android.widget.Button;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ScrollView;
import android.widget.TextView;
import android.widget.RemoteViews;

import com.actionbarsherlock.app.SherlockFragment;
import com.seafile.seadroid.BrowserActivity;
import com.seafile.seadroid.NavContext;
import com.seafile.seadroid.R;
import com.seafile.seadroid.Utils;
import com.seafile.seadroid.R.drawable;
import com.seafile.seadroid.R.id;
import com.seafile.seadroid.R.layout;
import com.seafile.seadroid.R.string;
import com.seafile.seadroid.account.Account;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.data.DataManager.ProgressMonitor;

public class FileFragment extends SherlockFragment {

    private static final String DEBUG_TAG = "FileFragment";
    
    private ArrayList<LoadFileTask> onGoingTasks = new ArrayList<LoadFileTask>();

    ViewGroup rootView = null;
    BrowserActivity mActivity = null;
    
    private ImageView ivFileIcon;
    private TextView tv_filename;
    private TextView tv_file_size;
    
    private DataManager getDataManager() {
        return mActivity.getDataManager();
    }
    
    private NavContext getNavContext() {
        return mActivity.getNavContext();
    }
    
    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        Log.d(DEBUG_TAG, "FileFragment Attached");
        mActivity = (BrowserActivity)activity;
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
        
        NavContext navContext = getNavContext();
        String repoID = navContext.getRepo();
        String path = navContext.getFilePath();
        String objectID = navContext.getFileID();
        long size = navContext.getFileSize();
        
        LayoutInflater inflater = (LayoutInflater) getActivity()
                .getSystemService(Context.LAYOUT_INFLATER_SERVICE);
        inflater.inflate(R.layout.file_details, rootView, true);

        ivFileIcon = (ImageView) getActivity().findViewById(R.id.iv_file_icon);
        tv_filename = (TextView) getActivity().findViewById(R.id.tv_file_name);
        tv_file_size = (TextView) getActivity().findViewById(R.id.tv_file_size);
        tv_filename.setText(Utils.fileNameFromPath(path));
        tv_file_size.setText(Utils.readableFileSize(size));
        
        String suffix = path.substring(path.lastIndexOf('.') + 1);
        String mime = MimeTypeMap.getSingleton().getMimeTypeFromExtension(suffix);
        int id = Utils.getResIdforMimetypeLarge(mime);
        ivFileIcon.setImageResource(id);

        for (LoadFileTask task : onGoingTasks) {
            if (task.getObjectID().equals(objectID)) {
                Log.d(DEBUG_TAG, "Task is going");
                switchVisibilityOnDownload();
            }
        }
    }
    
    
    @Override
    public void onStop() {
        Log.d(DEBUG_TAG, "FileFragment onStop");
        super.onStop();
    }
    
    @Override
    public void onDestroyView() {
        rootView = null;
        Log.d(DEBUG_TAG, "FileFragment onDestroyView");
        super.onDestroyView();
    }
    
    
    @Override
    public void onDetach() {
        mActivity = null;
        Log.d(DEBUG_TAG, "FileFragment detached");
        super.onDetach();
    }
    
    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
    }

    public void updateFileView(String repoID, String path, String objectID, long size) {

    }
    
    public void cancelDownload() {
        NavContext navContext = mActivity.getNavContext();
        for (LoadFileTask task : onGoingTasks) {
            if (task.getObjectID().equals(navContext.getFileID())) {
                task.cancel(true);
                break;
            }
        }
    }
    
    public void openFile() {
        NavContext navContext = getNavContext();
        String repoID = navContext.getRepo();
        String path = navContext.getFilePath();
        String objectID = navContext.getFileID();
        long size = navContext.getFileSize();
        
        downloadFile(repoID, path, objectID, size);
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
    
    private void switchVisibilityOnDownload() {
        Button btn = (Button)getActivity().findViewById(R.id.btn_toggle_open_download_file);
        btn.setVisibility(View.INVISIBLE);
        Button cancelBtn = (Button)getActivity().findViewById(R.id.btn_cancel_download);
        cancelBtn.setVisibility(View.VISIBLE);
    }
    
    private void switchVisibilityOnStop() {
        Button btn = (Button)getActivity().findViewById(R.id.btn_toggle_open_download_file);
        btn.setVisibility(View.VISIBLE);
        Button cancelBtn = (Button)getActivity().findViewById(R.id.btn_cancel_download);
        cancelBtn.setVisibility(View.INVISIBLE);
    }
    
    private void downloadFile(String repoID, String path, String objectID, long size) {
        mActivity.setRefreshing();
        switchVisibilityOnDownload();
        new LoadFileTask(repoID, path, objectID, size).execute();
    }
    
    private void showFile(File file) {
        String name = file.getName();
        String suffix = name.substring(name.lastIndexOf('.') + 1);
        
        if (suffix.length() == 0) {
            mActivity.showToast(getString(R.string.unknown_file_type));
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
            mActivity.showToast(getString(R.string.activity_not_found));
        }
    }
    
    /*
    private void showMarkdown(File file) {
        String content = Utils.readFile(file);
        if (content == null)
            return;

        ScrollView scroller = new ScrollView(getActivity());
        TextView text = new TextView(getActivity());
        int padding = (int) TypedValue.applyDimension(
                TypedValue.COMPLEX_UNIT_DIP, 4, getActivity().getResources()
                        .getDisplayMetrics());
        text.setPadding(padding, padding, padding, padding);
        scroller.addView(text);
        text.setText(content);
        rootView.removeAllViews();
        rootView.addView(scroller);
    }
    */
    
    private static int notificationID = 0;

    private class LoadFileTask extends AsyncTask<String, Integer, File> {

        Notification notification;
        NotificationManager notificationManager;
        private int showProgressThreshold = 1024 * 100; // 100KB
        private int myNtID;
        
        private String myRepoID;
        private String myPath;
        private String myObjectID;
        private long mySize;
        
        public LoadFileTask(String repoID, String path, 
                String objectID, long size) {
            this.myRepoID = repoID;
            this.myPath = path;
            this.myObjectID = objectID;
            this.mySize = size;
            // Log.d(DEBUG_TAG, "stored object is " + myPath + myObjectID);
            onGoingTasks.add(this);
        }
        
        public String getObjectID() {
            return myObjectID;
        }
        
        @Override
        protected void onPreExecute() {
            if (mySize <= showProgressThreshold)
                return;
            myNtID = ++notificationID;
            notificationManager = (NotificationManager) mActivity.getSystemService(Context.NOTIFICATION_SERVICE);
            Intent notificationIntent = new Intent(mActivity, BrowserActivity.class);
            
            Account account = mActivity.getAccount();
            notificationIntent.putExtra("server", account.server);
            notificationIntent.putExtra("email", account.email);
            notificationIntent.putExtra("token", account.token);
            notificationIntent.putExtra("repoID", myRepoID);
            notificationIntent.putExtra("objectID", myObjectID);
            notificationIntent.putExtra("path", myPath);
            notificationIntent.putExtra("size", mySize);
            
            PendingIntent intent = PendingIntent.getActivity(mActivity, myNtID, notificationIntent, 0);

            notification = new Notification(R.drawable.ic_stat_download, "", System.currentTimeMillis());
            notification.flags = notification.flags | Notification.FLAG_ONGOING_EVENT;
            notification.contentView = new RemoteViews(mActivity.getApplicationContext().getPackageName(),
                    R.layout.download_progress);
            notification.contentView.setCharSequence(R.id.tv_download_title, "setText",
                    Utils.fileNameFromPath(myPath));
            notification.contentIntent = intent; 
        }
        
        @Override
        protected void onProgressUpdate(Integer... values) {
            int progress = values[0];
            notification.contentView.setProgressBar(R.id.pb_download_progressbar,
                    (int)mySize, progress, false);
            notificationManager.notify(myNtID, notification);
        }

        class FileLoadMonitor implements ProgressMonitor  {
            LoadFileTask task;
            
            public FileLoadMonitor(LoadFileTask task) {
                this.task = task;
            }
            
            @Override
            public void onProgressNotify(long total) {
                publishProgress((int)total);
            }
            
            @Override
            public boolean isCancelled() {
                return task.isCancelled();
            }
        }
        
        @Override
        protected File doInBackground(String... params) {
            if (params.length != 0) {
                Log.d(DEBUG_TAG, "Wrong params to LoadFileTask");
                return null;
            }

            if (mySize <= showProgressThreshold)
                return getDataManager().getFile(myRepoID, myPath, myObjectID, null);
            else
                return getDataManager().getFile(myRepoID, myPath, myObjectID,
                        new FileLoadMonitor(this));
        }

        @Override
        protected void onPostExecute(File file) {
            if (mySize > showProgressThreshold)
                notificationManager.cancel(myNtID);
            onGoingTasks.remove(this);
            
            if (rootView == null)
                return;
            
            CharSequence text;
            if (file != null) {
                text = file.getName();
                showFile(file);
            } else {
                text = "Failed to load file";
                mActivity.showToast(text);
            }
            mActivity.unsetRefreshing();
            switchVisibilityOnStop();
        }
        
        @Override
        protected void onCancelled() {
            if (mySize > showProgressThreshold)
                notificationManager.cancel(myNtID);
            onGoingTasks.remove(this);
            
            if (rootView == null)
                return;
            
            mActivity.unsetRefreshing();
            switchVisibilityOnStop();
        }

    }
}
