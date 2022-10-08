package com.seafile.seadroid2.loopimages;

import android.app.AlarmManager;
import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.appwidget.AppWidgetManager;
import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.graphics.Color;
import android.graphics.ImageDecoder;
import android.graphics.Matrix;
import android.os.AsyncTask;
import android.os.Build;
import android.os.Bundle;
import android.os.Debug;
import android.os.Handler;
import android.os.IBinder;
import android.os.Message;
import android.os.SystemClock;
import android.support.annotation.RequiresApi;
import android.support.v4.app.NotificationCompat;
import android.telephony.mbms.FileInfo;
import android.util.Log;
import android.view.SurfaceControl;
import android.widget.ImageView;
import android.widget.RemoteViews;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.cameraupload.CameraSyncAdapter;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.data.DirentCache;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.gallery.Image;
import com.seafile.seadroid2.transfer.DownloadTask;
import com.seafile.seadroid2.transfer.DownloadTaskInfo;
import com.seafile.seadroid2.transfer.DownloadTaskManager;
import com.seafile.seadroid2.transfer.TaskState;
import com.seafile.seadroid2.transfer.TransferService;
import com.seafile.seadroid2.transfer.UploadTaskInfo;
import com.seafile.seadroid2.util.Utils;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.BitSet;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;
import java.util.Random;
import java.util.Set;


public class LoopImagesWidgetService extends Service {

    private static final int ALARM_DURATION = 2 * 60 * 1000;
    private static final int UPDATE_DURATION = 12 * 1000;
    private static final int UPDATE_MESSAGE = 1000;
    private static final int CHECKT_DOWNLOAD_STATE = 1001;
    private static final int CHECKT_DOWNLOAD_STATE_DURATION = 5*1000;
    private static final int UPDATE_IMAGE_DELAY = 60 * 60 * 1000;
    private static final int ONCE_UPLOAD_IMAGE_NUM = 3;
    private static final int LEFT_PREVIOUS_IMAGES_NUM = 1000;
    private static final int LEFT_FOLLOW_UP_ULTIMATE_MIN_IMAGES_NUM = 100;
    private static final int LEFT_FOLLOW_UP_MIN_IMAGES_NUM = 2000;
    private static final int LEFT_FOLLOW_UP_MAX_IMAGES_NUM = 3000;
    public static final String UPDATE_WIDGETS_KEY = "update_widgets_key";
    public static final int UPDATE_ALL_WIDGETS = -1;
    public static final int UPDATE_NONE_WIDGETS = -2;
    public static final String DELETE_WIDGETS_KEY = "delete_widgets_key";
    public static final int DELETE_ALL_WIDGETS = -1;
    public static final int DELETE_NONE_WIDGETS = -2;
    public static final String UPDATE_IMAGE_INFO_SIGNAL = "update_image_info_signal";
    public static final String DELETE_IMAGE_INFO_SIGNAL = "delete_image_info_signal";
    public static final String DELAY_UPDATE_ALL_SIGNAL = "delay_update_signal";
    private static final String DEBUG_TAG = "LoopImagesWidgetService";

    private Object imageInfosLock = new Object();
    private Map<Integer, List<ImageInfo>> imageInfos;
    private Map<Integer, LazyQueue> queues;
    private LinkedList<TaskInfo> tasksInProgress = Lists.newLinkedList();
    private Map<Integer, Boolean> shouldDownloads;
    private Map<String, AutoCleanDirentCache> caches;
    private Random rand = new Random();

    private boolean isUpdatingInfo = false;
    private Object updatingInfoLock = new Object();

    private int imageInfoDelayTimes = 0;

    private static final String[] FILE_SUFIX = new String[]{
            "gif",
            "png",
            "bmp",
            "jpeg",
            "jpg",
            "webp"
    };

    private TransferService txService = null;

    private UpdateHandler updateHandler;

    ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName className, IBinder service) {
            TransferService.TransferBinder binder = (TransferService.TransferBinder) service;
            synchronized (LoopImagesWidgetService.this) {
                txService = binder.getService();
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {

            synchronized (LoopImagesWidgetService.this) {
                txService = null;
            }
        }
    };

    private synchronized void startTransferService() {
        if (txService != null)
            return;

        Intent bIntent = new Intent(getApplicationContext(), TransferService.class);
        getApplicationContext().bindService(bIntent, mConnection, Context.BIND_AUTO_CREATE);
    }

    private synchronized void stopTransferService(){
        if(mConnection == null){
            return;
        }
        getApplicationContext().unbindService(mConnection);
    }

    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    private void sendAlaramMessage(){
        AlarmManager manager = (AlarmManager) getSystemService(Context.ALARM_SERVICE);
        Intent alarmIntent = new Intent(getBaseContext(), LoopImagesWidgetService.class);
        PendingIntent pendingIntent = PendingIntent.getService(getBaseContext(), 0,
                alarmIntent, PendingIntent.FLAG_UPDATE_CURRENT);
        manager.set(AlarmManager.ELAPSED_REALTIME_WAKEUP,
                SystemClock.elapsedRealtime() + ALARM_DURATION, pendingIntent);
    }

    private void addUpdateInfoTask(int updateSignal, int deleteSignal){
        synchronized (updatingInfoLock){
            if(!isUpdatingInfo) {
                isUpdatingInfo = true;
                new UpdateImageInfoTask().execute(updateSignal, deleteSignal);
            }
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand");

        int updateAppWidgetSignal = UPDATE_NONE_WIDGETS;
        if(intent!=null) {
            updateAppWidgetSignal = intent.getIntExtra(UPDATE_IMAGE_INFO_SIGNAL, UPDATE_NONE_WIDGETS);
        }
        if(updateAppWidgetSignal != UPDATE_NONE_WIDGETS){
            addUpdateInfoTask(updateAppWidgetSignal, DELETE_NONE_WIDGETS);
            return START_STICKY;
        }

        int deleteAppWidgetSignal = DELETE_NONE_WIDGETS;
        if(intent != null){
            deleteAppWidgetSignal = intent.getIntExtra(DELETE_IMAGE_INFO_SIGNAL, DELETE_NONE_WIDGETS);
        }
        if(deleteAppWidgetSignal != DELETE_NONE_WIDGETS){
            addUpdateInfoTask(UPDATE_NONE_WIDGETS, deleteAppWidgetSignal);
            return START_STICKY;
        }

        boolean delayUpdateAll = false;
        if(intent != null) {
            delayUpdateAll = intent.getBooleanExtra(DELAY_UPDATE_ALL_SIGNAL, false);
        }
        if(delayUpdateAll){
            AlarmManager manager = (AlarmManager) getSystemService(Context.ALARM_SERVICE);
            Intent alarmIntent = new Intent(getBaseContext(), LoopImagesWidgetService.class);
            alarmIntent.putExtra(UPDATE_IMAGE_INFO_SIGNAL, UPDATE_ALL_WIDGETS);
            PendingIntent pendingIntent = PendingIntent.getService(getBaseContext(), 0,
                    alarmIntent, PendingIntent.FLAG_UPDATE_CURRENT);
            manager.set(AlarmManager.ELAPSED_REALTIME_WAKEUP,
                    SystemClock.elapsedRealtime() + 60*1000, pendingIntent);
            return START_STICKY;
        }

        if(imageInfoDelayTimes < UPDATE_IMAGE_DELAY / ALARM_DURATION){
            ++imageInfoDelayTimes;
        }else{
            imageInfoDelayTimes = 0;
        }

        if(imageInfoDelayTimes == 1){
            addUpdateInfoTask(UPDATE_ALL_WIDGETS, DELETE_NONE_WIDGETS);
        }

        sendAlaramMessage();
        return START_STICKY;
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        stopTransferService();
    }

    @Override
    public void onCreate() {
        super.onCreate();

        Log.d(DEBUG_TAG, "onCreate");

        if(updateHandler == null) {
            updateHandler = new UpdateHandler();
        }

        Message message = updateHandler.obtainMessage();
        message.what = UPDATE_MESSAGE;
        updateHandler.sendMessageDelayed(message, UPDATE_DURATION);

        startTransferService();
        createNotificationChannel();
        sendAlaramMessage();
    }

    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= 26) {
            String CHANNEL_ID = "seafile_channel";
            NotificationChannel channel = new NotificationChannel(CHANNEL_ID,
                    "seafile notification",
                    NotificationManager.IMPORTANCE_DEFAULT);
            ((NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE)).createNotificationChannel(channel);
            Notification notification = new NotificationCompat.Builder(this, CHANNEL_ID)
                    .setContentTitle("")
                    .setContentText("").build();
            startForeground(1, notification);
        }
    }

    private void downloadImages(int appWidgetId, ImageInfo imageInfo){
        File file = imageInfo.getFile();
        SeafDirent seafDirent = imageInfo.getSeafDirent();
        String filePath = imageInfo.getFilePath();
        if(seafDirent == null || filePath == null){
            return;
        }
        if(file != null && file.exists()){
            if(getFileSize(imageInfo.getFile()) == seafDirent.getFileSize()) {
                queues.get(appWidgetId).push(imageInfo);
            }else{
                imageInfo.deleteStorage();
            }
            return;
        }
        int taskID = txService.addDownloadTask(imageInfo.getDirInfo().getAccount(),
                imageInfo.getDirInfo().getRepoName(),
                imageInfo.getDirInfo().getRepoId(),
                filePath,
                seafDirent.size);
        tasksInProgress.add(new TaskInfo(taskID, appWidgetId, imageInfo));
    }

    private long getFileSize(File file) {
        long size = 0;
        try {
            if (file.exists()) {
                FileInputStream fis = null;
                fis = new FileInputStream(file);
                size = fis.available();
            }
        }catch (Exception e){
            return 0;
        }
        return size;
    }

    private void removeLeftImages(int appWidgetId){
        if (queues.get(appWidgetId).getRemoveCount() >= LEFT_PREVIOUS_IMAGES_NUM) {
            ImageInfo deleteItem = queues.get(appWidgetId).pop();
            while (deleteItem != null) {
                deleteItem.deleteStorage();
                String filePath = deleteItem.getFilePath();
                if(filePath != null){
                    Log.d(DEBUG_TAG, "Delete file that exceed storage space: " + filePath + "!!!");
                    Utils.utilsLogInfo(true, "Delete file that exceed storage space: " + filePath + "!!!");
                }
                if (queues.get(appWidgetId).getRemoveCount() < LEFT_PREVIOUS_IMAGES_NUM) {
                    break;
                }
                deleteItem = queues.get(appWidgetId).pop();
            }
        }
    }

    private void checkDownloadTasks(){
        Log.d(DEBUG_TAG, "checkDownloadTasks");
        int retryTimes = 0;
        while(tasksInProgress.size() > 0 && retryTimes < 3) {
            Utils.utilsLogInfo(true, "======="+ retryTimes +" Retry checking download tasks!!!");
            ++retryTimes;
            int removeNum = 0;
            for (int i=0;i<tasksInProgress.size();++i) {
                TaskInfo taskInfo = tasksInProgress.get(i);
                DownloadTaskInfo info = txService.getDownloadTaskInfo(taskInfo.taskID);
                ImageInfo item = taskInfo.imageInfo;
                if (info.err != null) {
                    ++removeNum;
                    item.deleteStorage();
                    Utils.utilsLogInfo(true, "=======Task " + Integer.toString(taskInfo.taskID) + " failed to download " + info.localFilePath + "!!!");
//                throw info.err;
                    continue;
                }
                if (info.state == TaskState.INIT || info.state == TaskState.TRANSFERRING) {
                    break;
                }
                ++removeNum;
                SeafDirent seafDirent = item.getSeafDirent();
                if(seafDirent == null){
                    continue;
                }
                boolean haveSameSize = getFileSize(item.getFile())
                        == seafDirent.getFileSize();
                if (info.state == TaskState.FINISHED && haveSameSize) {
                    queues.get(taskInfo.appWidgetId).push(taskInfo.imageInfo);
                    if (queues.get(taskInfo.appWidgetId).getCount() >= LEFT_FOLLOW_UP_MAX_IMAGES_NUM) {
                        shouldDownloads.put(taskInfo.appWidgetId, false);
                    }
                } else {
                    item.deleteStorage();
                }
            }
            while(removeNum > 0){
                tasksInProgress.removeFirst();
                --removeNum;
            }
            if(tasksInProgress.size() > 0){
                try {
                    Thread.sleep(100);
                }catch (InterruptedException e){
                    continue;
                }
            }
        }
        tasksInProgress.clear();
    }

    private RemoteViews setRemoteViewOnClickActivity(RemoteViews views, int appWidgetId, String dirInfo, String imageName){
        Intent activityIntent = new Intent(getApplicationContext(), LoopImagesWidgetConfigureActivity.class);
        activityIntent.putExtra(AppWidgetManager.EXTRA_APPWIDGET_ID, appWidgetId);
        activityIntent.putExtra(LoopImagesWidget.DIR_INFO, dirInfo);
        activityIntent.putExtra(LoopImagesWidget.IMAGE_NAME, imageName);
        PendingIntent widgetIntent = PendingIntent.getActivity(getApplicationContext(), appWidgetId, activityIntent, PendingIntent.FLAG_CANCEL_CURRENT);
        views.setOnClickPendingIntent(R.id.loopimages_widget_relative_layout, widgetIntent);
        return views;
    }

    private RemoteViews getDefalutRemoteViews(int appWidgetId){
        Context context = getApplicationContext();
        RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.loop_images_widget);
        views.setImageViewResource(R.id.loopimages_imageview, R.drawable.rem);
        views = setRemoteViewOnClickActivity(views, appWidgetId, null, null);
        return views;
    }

    @RequiresApi(api = Build.VERSION_CODES.N)
    private void updateWidget() {
        synchronized (imageInfosLock) {
            checkDownloadTasks();
            Utils.utilsLogInfo(true, "=======Update Widget.");
            Context context = getApplicationContext();
            AppWidgetManager appWidgetManager = AppWidgetManager.getInstance(context);
            int appWidgetIds[] = appWidgetManager.getAppWidgetIds(new ComponentName(context, LoopImagesWidget.class));
            if (imageInfos == null || queues == null || appWidgetIds.length <= 0) {
                addUpdateInfoTask(UPDATE_ALL_WIDGETS, DELETE_NONE_WIDGETS);
                Message message = updateHandler.obtainMessage();
                message.what = UPDATE_MESSAGE;
                updateHandler.sendMessageDelayed(message, UPDATE_DURATION);
                return;
            }
            int n = appWidgetIds.length;
            for (int i = 0; i < n; ++i) {
                int appWidgetId = appWidgetIds[i];
                if (i >= imageInfos.size() || i >= queues.size()) {
                    break;
                }
                List<ImageInfo> imageInfo = imageInfos.get(appWidgetId);
                int m = imageInfo.size();
                if (m <= 0) {
                    addUpdateInfoTask(appWidgetId, DELETE_NONE_WIDGETS);
                    RemoteViews views = getDefalutRemoteViews(appWidgetId);
                    appWidgetManager.updateAppWidget(appWidgetId, views);
                    continue;
                }
                boolean enableDownload = Utils.isWiFiOn() || LoopImagesWidgetConfigureActivity.getDataPlanAllowed(appWidgetId);
                if (Utils.isNetworkOn() && enableDownload && tasksInProgress.size() < ONCE_UPLOAD_IMAGE_NUM * 2 * n) {
                    if (queues.get(appWidgetId).getCount() < LEFT_FOLLOW_UP_MIN_IMAGES_NUM) {
                        shouldDownloads.put(appWidgetId, true);
                    }
                    if (queues.get(appWidgetId).getCount() < LEFT_FOLLOW_UP_MAX_IMAGES_NUM && shouldDownloads.get(appWidgetId)) {
                        Log.d(DEBUG_TAG, "Download new images.");
                        int addTaskNum = ONCE_UPLOAD_IMAGE_NUM;
                        while (addTaskNum > 0) {

                            downloadImages(appWidgetId, imageInfo.get(rand.nextInt(m)));
                            --addTaskNum;
                        }
                    }
                }
                if (queues.get(appWidgetId).isEmpty()) {
                    RemoteViews views = getDefalutRemoteViews(appWidgetId);
                    appWidgetManager.updateAppWidget(appWidgetId, views);
                    continue;
                }
                boolean flag = false;
                ImageInfo item = queues.get(appWidgetId).next();
                while (item != null) {
                    Bitmap image = item.getBitMap();
                    if (image == null) {
                        item.deleteStorage();
                        queues.get(appWidgetId).removeLast();
                        item = queues.get(appWidgetId).next();
                        continue;
                    }
                    Log.d(DEBUG_TAG, "Set new images.");
                    RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.loop_images_widget);
                    views.setImageViewBitmap(R.id.loopimages_imageview, image);
                    SeafDirent seafDirent = item.getSeafDirent();
                    if(seafDirent == null){
                        views = setRemoteViewOnClickActivity(views, appWidgetId,null, null);
                    }else {
                        views = setRemoteViewOnClickActivity(views, appWidgetId, item.getDirInfo().toString(), seafDirent.name);
                    }
                    appWidgetManager.updateAppWidget(appWidgetId, views);
                    flag = true;
                    break;
                }
                if (!flag) {
                    RemoteViews views = getDefalutRemoteViews(appWidgetId);
                    appWidgetManager.updateAppWidget(appWidgetId, views);
                }
                removeLeftImages(appWidgetId);
            }
        }
//        RemoteViews remoteViews = new RemoteViews(getApplicationContext().getPackageName(), R.layout.loop_images_widget);
//        appWidgetManager.updateAppWidget(new ComponentName(context, LoopImagesWidget.class), remoteViews);
//        checkDownloadTasks();
        Message message = updateHandler.obtainMessage();
        message.what = UPDATE_MESSAGE;
        updateHandler.sendMessageDelayed(message, UPDATE_DURATION);
    }

    private void updateImageInfo(int updateWidgetSignal){
        if(updateWidgetSignal == UPDATE_NONE_WIDGETS){
            return;
        }
        synchronized (imageInfosLock) {
            Context context = getApplicationContext();
            AppWidgetManager appWidgetManager = AppWidgetManager.getInstance(context);

            if(caches == null){
                caches = new HashMap<String, AutoCleanDirentCache>();
            }

            if(queues == null){
                queues = new HashMap<Integer, LazyQueue>();
            }
            if (imageInfos == null || shouldDownloads == null) {
                updateWidgetSignal = UPDATE_ALL_WIDGETS;
                imageInfos = new HashMap<Integer, List<ImageInfo>>();
                shouldDownloads = new HashMap<Integer, Boolean>();
            }

            Map<String, Boolean> notUsedDir = new HashMap<String, Boolean>();
            for(String dirStr: caches.keySet()){
                notUsedDir.put(dirStr, true);
            }

            int appWidgetIds[];
            if (updateWidgetSignal != UPDATE_ALL_WIDGETS) {
                appWidgetIds = new int[]{updateWidgetSignal};
            } else {
                appWidgetIds = appWidgetManager.getAppWidgetIds(new ComponentName(context, LoopImagesWidget.class));
            }

            Map<String, List<ImageInfo>> dirImageInfoMapping = new HashMap<String, List<ImageInfo>>();

            Map<Integer, List<DirInfo>> widgetDirInfos = new HashMap<Integer, List<DirInfo>>();
            for (int appWidgetId : appWidgetIds) {
                widgetDirInfos.put(appWidgetId, LoopImagesWidgetConfigureActivity.getDirInfo(getApplicationContext(), appWidgetId));
            }
            for (int i = 0; i < appWidgetIds.length; ++i) {
                boolean haveUpdated = false;
                int appWidgetId = appWidgetIds[i];
                if(!queues.containsKey(appWidgetId)){
                    queues.put(appWidgetId, new LazyQueue());
                }
                List<DirInfo> dirInfos = widgetDirInfos.get(appWidgetId);
                List<ImageInfo> nImageInfo = Lists.newArrayList();
                for (DirInfo info : dirInfos) {
                    if (dirImageInfoMapping.containsKey(info.toString())) {
                        nImageInfo.addAll(dirImageInfoMapping.get(info.toString()));
                        continue;
                    }
                    String dirID = LoopImagesWidgetConfigureActivity.getDataManager(info.getAccount()).getDirID(info.getRepoId(), info.getDirPath());
                    notUsedDir.put(info.toString(), false);

                    if(info.getDirId() != dirID) {
                        caches.remove(info.toString());
                        info.setDirId(dirID);
                        haveUpdated = true;
                    }
                    AutoCleanDirentCache cache = null;
                    if(caches.containsKey(info.toString())){
                        cache = caches.get(info.toString());
                    }else {
                        try {
                            List<SeafDirent> seafDirents = LoopImagesWidgetConfigureActivity.getDataManager(info.getAccount()).getCachedDirents(info.getRepoId(), info.getDirPath());
                            cache = new AutoCleanDirentCache("LoopImages-dirent-" + dirID, seafDirents);
                        } catch (IOException e) {
                            Log.e(DEBUG_TAG, "Error to save cache.", e);
                        }
                    }
                    if(cache == null){
                        continue;
                    }
                    caches.put(info.toString(), cache);
                    List<ImageInfo> files = Lists.newArrayList();
                    for(int k=0;k<cache.getCount();++k){
                        boolean flag = false;
                        SeafDirent seafDirent = cache.get(k);
                        for (int j = 0; j < FILE_SUFIX.length; ++j) {
                            if (seafDirent.name.toLowerCase().endsWith(FILE_SUFIX[j])) {
                                flag = true;
                                break;
                            }
                        }
                        if (flag) {
                            ImageInfo item = new ImageInfo(info, cache, k);
//                            File itemFile = item.getFile();
//                            if(itemFile != null && itemFile.exists()){
//                                queues.get(appWidgetId).push(item);
//                            }
                            files.add(item);
                        }
                    }
                    nImageInfo.addAll(files);
                }
                if(!haveUpdated || imageInfos.containsKey(appWidgetId) && imageInfos.get(appWidgetId).equals(nImageInfo)){
                    continue;
                }
                imageInfos.put(appWidgetId, nImageInfo);
                shouldDownloads.put(appWidgetId, true);
                if(haveUpdated){
                    List<String> dirInfoStrs = new ArrayList<String>();
                    for(DirInfo dirInfo: dirInfos){
                        dirInfoStrs.add(dirInfo.toString());
                    }
                    LoopImagesWidgetConfigureActivity.getSettingsManager().setLoopImagesWidgetDirInfo(appWidgetId, dirInfoStrs);
                }
            }
            if(updateWidgetSignal == UPDATE_ALL_WIDGETS) {
                for (String dirStr : notUsedDir.keySet()) {
                    if (notUsedDir.get(dirStr)) {
                        notUsedDir.remove(dirStr);
                    }
                }
            }
        }
    }

    public void deleteAppWidgetInfo(int deleteAppWidgetSignal){
        if(deleteAppWidgetSignal == DELETE_NONE_WIDGETS){
            return;
        }
        synchronized (imageInfosLock) {
            Context context = getApplicationContext();
            AppWidgetManager appWidgetManager = AppWidgetManager.getInstance(context);
            int appWidgetIds[];
            if (deleteAppWidgetSignal != DELETE_ALL_WIDGETS) {
                appWidgetIds = new int[]{deleteAppWidgetSignal};
            } else {
                appWidgetIds = appWidgetManager.getAppWidgetIds(new ComponentName(context, LoopImagesWidget.class));
            }
            for(int appWidgetId: appWidgetIds) {
                if (imageInfos != null && imageInfos.containsKey(appWidgetId)) {
                    imageInfos.remove(appWidgetId);
                }
                if (queues != null && queues.containsKey(appWidgetId)) {
                    queues.remove(appWidgetId);
                }
                if (shouldDownloads != null && shouldDownloads.containsKey(appWidgetId)) {
                    shouldDownloads.remove(appWidgetId);
                }
            }
        }
    }

    protected class TaskInfo{
        public int taskID;
        public int appWidgetId;
        public ImageInfo imageInfo;

        public TaskInfo(int taskID, int appWidgetId, ImageInfo imageInfo){
            this.taskID = taskID;
            this.appWidgetId = appWidgetId;
            this.imageInfo = imageInfo;
        }
    }

    protected class LazyQueue{
        private LinkedList<ImageInfo> data;
        private LinkedList<ImageInfo> remove;
        private Set<String> store;

        public LazyQueue() {
            store = new HashSet<String>();
            data = Lists.newLinkedList();
            remove = Lists.newLinkedList();
        }

        public void push(ImageInfo info){
            if(!store.contains(info.getFilePath())) {
                store.add(info.getFilePath());
                data.add(info);
            }
        }

        public ImageInfo pop(){
            return removeFirst();
        }

        public ImageInfo removeFirst(){
            if(remove.size() > 0){
                ImageInfo item = remove.removeFirst();
                store.remove(item.getFilePath());
                return item;
            }
            return null;
        }

        public ImageInfo removeLast(){
            if(remove.size() > 0){
                ImageInfo item = remove.removeLast();
                store.remove(item.getFilePath());
                return item;
            }
            return null;
        }

        public int getCount(){
            return data.size();
        }

        public int getRemoveCount(){
            return remove.size();
        }

        public boolean isEmpty(){
            return data.isEmpty() && remove.isEmpty();
        }

        public ImageInfo next(){
            if(data.size() <= 0 && remove.size() <= 0){
                return null;
            }
            if(data.size() <= 0){
                ImageInfo res = remove.pop();
                remove.add(res);
                return res;
            }
            ImageInfo res = data.pop();
            remove.add(res);
            return res;
        }

        public ImageInfo back(){
            if(data.size() <= 0){
                return null;
            }
            return data.getLast();
        }

        public void recoverAll(){
            for(int i = (int)remove.size()-1; i>=0; --i){
                data.add(remove.get(i));
            }
            remove.clear();
        }

    }

    protected final class UpdateHandler extends Handler {

        @RequiresApi(api = Build.VERSION_CODES.N)
        @Override
        public void handleMessage(Message msg) {
            switch (msg.what) {
                case UPDATE_MESSAGE:
                    updateWidget();
                    break;
                default:
                    break;
            }
        }
    }

    protected class UpdateImageInfoTask extends AsyncTask<Integer,Integer,Integer>{
        @Override
        protected void onPreExecute() {
            super.onPreExecute();
        }

        @Override
        protected Integer doInBackground(Integer... integers) {
            if(integers.length != 2){
                return -1;
            }
            int updateSignal = integers[0];
            int deleteSignal = integers[1];
            if(updateSignal != UPDATE_NONE_WIDGETS){
                updateImageInfo(updateSignal);
            }
            if(deleteSignal != DELETE_NONE_WIDGETS){
                deleteAppWidgetInfo(deleteSignal);
            }
            synchronized (updatingInfoLock){
                isUpdatingInfo = false;
            }
            return 0;
        }

        @Override
        protected void onProgressUpdate(Integer... values) {
            super.onProgressUpdate(values);
        }

        @Override
        protected void onPostExecute(Integer value) {
            super.onPostExecute(value);
        }
    }

    class ImageInfo{
        private DirInfo dirInfo;
        private int index;
        private DirentCache cache;
        private static final int MAX_BITMAP_EDGE = 1024;

        public ImageInfo(DirInfo dirInfo, DirentCache cache, int index){
            this.dirInfo = dirInfo;
            this.cache = cache;
            this.index = index;
        }

        public DirInfo getDirInfo() {
            return dirInfo;
        }

        public SeafDirent getSeafDirent() {
            if(this.cache == null){
                return null;
            }
            return cache.get(index);
        }

        public String getFilePath(){
            SeafDirent seafDirent = getSeafDirent();
            if(seafDirent == null){
                return null;
            }
            return Utils.pathJoin(getDirInfo().getDirPath(), seafDirent.name);
        }

        public File getFile(){
            DataManager dataManager = new DataManager(getDirInfo().getAccount());
            String filePath = getFilePath();
            if(filePath == null){
                return null;
            }
            File file = dataManager.getLocalRepoFile(getDirInfo().getRepoName(), getDirInfo().getRepoId(), filePath);
            return file;
        }

        public boolean exist(){
            return getFile() != null;
        }

        public Bitmap getBitMap(){
            File file = getFile();
            if(file == null || !file.exists()){
                return null;
            }
            Bitmap bitmap = BitmapFactory.decodeFile(file.getAbsolutePath());
            if(bitmap == null){
                return null;
            }
            if(bitmap.getHeight() > MAX_BITMAP_EDGE || bitmap.getWidth() > MAX_BITMAP_EDGE){
                float scale = MAX_BITMAP_EDGE/(float)Math.max(bitmap.getHeight(), bitmap.getWidth());
                Matrix matrix = new Matrix();
                matrix.postScale(scale, scale);
                bitmap = Bitmap.createBitmap(bitmap, 0, 0, bitmap.getWidth(), bitmap.getHeight(), matrix, true);
            }
            return bitmap;
        }

        public boolean deleteStorage(){
            File file = getFile();
            if(file == null){
                return true;
            }
            return file.delete();
        }
    }

    protected class AutoCleanDirentCache extends DirentCache{
        public AutoCleanDirentCache(String name) throws IOException {
            super(name);
        }

        public AutoCleanDirentCache(String name, List<SeafDirent> caches) throws IOException{
            super(name, caches);
        }

        @Override
        protected void finalize() throws Throwable {
            super.finalize();
            super.delete();
        }
    }
}