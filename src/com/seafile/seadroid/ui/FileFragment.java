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
import android.widget.TextView;
import android.widget.RemoteViews;
import android.widget.Toast;

import com.actionbarsherlock.app.SherlockFragment;
import com.actionbarsherlock.app.SherlockFragmentActivity;
import com.seafile.seadroid.BrowserActivity;
import com.seafile.seadroid.FileActivity;
import com.seafile.seadroid.MarkdownActivity;
import com.seafile.seadroid.NavContext;
import com.seafile.seadroid.R;
import com.seafile.seadroid.SeafException;
import com.seafile.seadroid.Utils;
import com.seafile.seadroid.account.Account;
import com.seafile.seadroid.data.DataManager;
import com.seafile.seadroid.data.DataManager.ProgressMonitor;
import com.seafile.seadroid.ui.PasswordDialog.PasswordGetListener;

public class FileFragment extends SherlockFragment implements PasswordGetListener {

    private static final String DEBUG_TAG = "FileFragment";
    
    private ArrayList<LoadFileTask> onGoingTasks = new ArrayList<LoadFileTask>();

    SherlockFragmentActivity mActivity = null;
    private DataManager dataManager;
    
    private ImageView ivFileIcon;
    private TextView tv_filename;
    private TextView tv_file_size;
    
    private String repo;
    private String path;
    private String fileID;
    private long size;
    
    public void setFile(String repo, String path, String fileID, long size) {
        this.repo = repo;
        this.path = path;
        this.fileID = fileID;
        this.size = size;
    }
    
    public void setDataManager(DataManager manager) {
        dataManager = manager;
    }
    
    public String getRepo() {
        return repo;
    }
    
    public String getPath() {
        return path;
    }
    
    @Override
    public void onAttach(Activity activity) {
        super.onAttach(activity);
        Log.d(DEBUG_TAG, "FileFragment Attached");
        mActivity = (SherlockFragmentActivity)activity;
    }
    
    @Override
    public View onCreateView(LayoutInflater inflater, ViewGroup container, 
        Bundle savedInstanceState) {

        Log.d(DEBUG_TAG, "On Create view ");
        View view = inflater.inflate(R.layout.file_details, container, false);

        ivFileIcon = (ImageView) view.findViewById(R.id.iv_file_icon);
        tv_filename = (TextView) view.findViewById(R.id.tv_file_name);
        tv_file_size = (TextView) view.findViewById(R.id.tv_file_size);

        return view;
    }
    
    @Override
    public void onActivityCreated(Bundle savedInstanceState) {
        super.onActivityCreated(savedInstanceState);
        
        Log.d(DEBUG_TAG, "tv_filename " + tv_filename);
        tv_filename.setText(Utils.fileNameFromPath(path));
        tv_file_size.setText(Utils.readableFileSize(size));
        
        String name = path.substring(path.lastIndexOf('/') + 1);
        int id = Utils.getFileIcon(name);
        ivFileIcon.setImageResource(id);

        for (LoadFileTask task : onGoingTasks) {
            if (task.getFileID().equals(fileID)) {
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
        for (LoadFileTask task : onGoingTasks) {
            if (task.getFileID().equals(fileID)) {
                task.cancel(true);
                break;
            }
        }
    }
    
    public void openFile() {        
        downloadFile(repo, path, fileID, size);
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
        switchVisibilityOnDownload();
        new LoadFileTask(repoID, path, objectID, size).execute();
    }
    
    private void showToast(CharSequence msg) {
        Context context = getActivity().getApplicationContext();
        Toast toast = Toast.makeText(context, msg, Toast.LENGTH_SHORT);
        toast.show();
    }
    
    private void showFile(File file) {
        String name = file.getName();
        String suffix = name.substring(name.lastIndexOf('.') + 1);
        
        if (suffix.length() == 0) {
            showToast(getString(R.string.unknown_file_type));
            return;
        }
        
        if (suffix.endsWith("md") || suffix.endsWith("markdown")) {
            startMarkdownActivity(repo, path, fileID);
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
            showToast(getString(R.string.activity_not_found));
        }
    }
    
    private void startMarkdownActivity(String repoID, String path, String fileID) {
        Intent intent = new Intent(mActivity, MarkdownActivity.class);
        intent.putExtra("repoID", repoID);
        intent.putExtra("path", path);
        intent.putExtra("fileID", fileID);
        startActivity(intent);
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
        private String myFileID;
        private long mySize;
        SeafException err;
        
        public LoadFileTask(String repoID, String path, 
                String fileID, long size) {
            this.myRepoID = repoID;
            this.myPath = path;
            this.myFileID = fileID;
            this.mySize = size;
            // Log.d(DEBUG_TAG, "stored object is " + myPath + myObjectID);
            onGoingTasks.add(this);
            err = null;
        }
        
        public String getFileID() {
            return myFileID;
        }
        
        @Override
        protected void onPreExecute() {
            if (mySize <= showProgressThreshold)
                return;
            myNtID = ++notificationID;
            notificationManager = (NotificationManager) mActivity.getSystemService(Context.NOTIFICATION_SERVICE);
            Intent notificationIntent = new Intent(mActivity, BrowserActivity.class);
            
            Account account = dataManager.getAccount();
            notificationIntent.putExtra("server", account.server);
            notificationIntent.putExtra("email", account.email);
            notificationIntent.putExtra("token", account.token);
            notificationIntent.putExtra("repoID", myRepoID);
            notificationIntent.putExtra("objectID", myFileID);
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
            
            notification.contentView.setProgressBar(R.id.pb_download_progressbar,
                    (int)mySize, 0, false);
            notificationManager.notify(myNtID, notification);
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

            try {
                if (mySize <= showProgressThreshold)
                    return dataManager.getFile(myRepoID, myPath, myFileID, null);
                else
                    return dataManager.getFile(myRepoID, myPath, myFileID,
                            new FileLoadMonitor(this));
            } catch (SeafException e) {
                err = e;
                return null;
            }
        }

        @Override
        protected void onPostExecute(File file) {
            if (mySize > showProgressThreshold)
                notificationManager.cancel(myNtID);
            onGoingTasks.remove(this);
            
            if (mActivity == null)
                return;
            
            CharSequence text;
            if (file != null) {
                text = file.getName();
                showFile(file);
            } else {
                if (err == null) {
                    text = "Failed to load file";
                    showToast(text);
                } else {
                    if (err.getCode() == 440) {
                        showPasswordDialog();
                    }
                }
            }
            switchVisibilityOnStop();
        }
        
        @Override
        protected void onCancelled() {
            if (mySize > showProgressThreshold)
                notificationManager.cancel(myNtID);
            onGoingTasks.remove(this);
            
            if (mActivity == null)
                return;
            
            switchVisibilityOnStop();
        }

    }
    
    private void showPasswordDialog() {
        PasswordDialog dialog = new PasswordDialog();
        dialog.setPasswordGetListener(this);
        dialog.show(mActivity.getSupportFragmentManager(), "DialogFragment");
    }

    @Override
    public void onPasswordGet(String password) {
        if (password.length() == 0)
            return;

        // TODO: this will block the main thread 
        dataManager.setPassword(repo, password);
        downloadFile(repo, path, fileID, size);
    }
    
}
