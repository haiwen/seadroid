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
import android.graphics.Matrix;
import android.os.Build;
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
import com.seafile.seadroid2.cameraupload.CameraSyncAdapter;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.DatabaseHelper;
import com.seafile.seadroid2.data.DirentCache;
import com.seafile.seadroid2.data.SeafCachedFile;
import com.seafile.seadroid2.data.SeafDirent;
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
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.ListIterator;
import java.util.Map;


public class LoopImagesWidgetService extends Service {

    private static final int ALARM_DURATION = 3 * 60 * 1000;
    private static final int UPDATE_DURATION = 10 * 1000;
    private static final int UPDATE_MESSAGE = 1000;
    private static final int CHECKT_DOWNLOAD_STATE = 1001;
    private static final int CHECKT_DOWNLOAD_STATE_DURATION = 5*1000;
    private static final int UPDATE_IMAGE_DELAY = 24 * 60 * 60 * 1000;
    private static final int ONCE_UPLOAD_IMAGE_NUM = 3;
    private static final int LEFT_PREVIOUS_IMAGES_NUM = 1000;
    private static final int LEFT_FOLLOW_UP_ULTIMATE_MIN_IMAGES_NUM = 100;
    private static final int LEFT_FOLLOW_UP_MIN_IMAGES_NUM = 2000;
    private static final int LEFT_FOLLOW_UP_MAX_IMAGES_NUM = 3000;
    public static final int DELETE_ALL_WIDGETS = -1;
    public static final int DELETE_NONE_WIDGETS = -2;
    public static final int UPDATE_ALL_WIDGETS = -1;
    public static final int UPDATE_NONE_WIDGETS = -2;
    public static final String UPDATE_IMAGE_INFO_SIGNAL = "update_image_info_signal";
    public static final String DELETE_IMAGE_INFO_SIGNAL = "delete_image_info_signal";
    private static final String DEBUG_TAG = "LoopImagesWidgetService";

    private Object imageInfosLock = new Object();
    private Map<Integer, List<ImageInfo>> imageInfos;
    private Map<Integer, LazyQueue> queues;
    private LinkedList<TaskInfo> tasksInProgress = Lists.newLinkedList();
    private Map<Integer, Boolean> shouldDownloads;

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

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        Log.d(DEBUG_TAG, "onStartCommand");

        int updateAppWidgetSignal = UPDATE_NONE_WIDGETS;
        if(intent!=null) {
            updateAppWidgetSignal = intent.getIntExtra(UPDATE_IMAGE_INFO_SIGNAL, UPDATE_NONE_WIDGETS);
        }
        if(updateAppWidgetSignal != UPDATE_NONE_WIDGETS){
            updateImageInfo(updateAppWidgetSignal);
            return START_STICKY;
        }

        int deleteAppWidgetSignal = DELETE_NONE_WIDGETS;
        if(intent != null){
            deleteAppWidgetSignal = intent.getIntExtra(DELETE_IMAGE_INFO_SIGNAL, DELETE_NONE_WIDGETS);
        }
        if(deleteAppWidgetSignal != DELETE_NONE_WIDGETS){
            deleteAppWidgetInfo(deleteAppWidgetSignal);
            return START_STICKY;
        }

        if(imageInfoDelayTimes < UPDATE_IMAGE_DELAY / ALARM_DURATION){
            ++imageInfoDelayTimes;
        }else{
            imageInfoDelayTimes = 0;
        }

        if(imageInfoDelayTimes == 0){
            updateImageInfo(UPDATE_ALL_WIDGETS);
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

    private void downloadImages(int appWidgetId, int position){
        ImageInfo imageInfo = imageInfos.get(appWidgetId).get(position);
        File file = imageInfo.getFile();
        if(file != null && file.exists()){
            if(getFileSize(imageInfo.getFile()) == imageInfo.getSeafDirent().getFileSize()) {
                queues.get(appWidgetId).push(position);
            }else{
                imageInfo.deleteStorage();
            }
            return;
        }
        int taskID = txService.addDownloadTask(imageInfo.getDirInfo().getAccount(),
                imageInfo.getDirInfo().getRepoName(),
                imageInfo.getDirInfo().getRepoId(),
                imageInfo.getFilePath(),
                imageInfo.getSeafDirent().size);
        tasksInProgress.add(new TaskInfo(taskID, appWidgetId, position));
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
            int deleteIndex = queues.get(appWidgetId).pop();
            while (deleteIndex >= 0) {
                if (deleteIndex >= imageInfos.get(appWidgetId).size()) {
                    continue;
                }
                ImageInfo imageInfo = imageInfos.get(appWidgetId).get(deleteIndex);
                imageInfo.deleteStorage();
                Log.d(DEBUG_TAG, "Delete file that exceed storage space: " + imageInfo.getFilePath() + "!!!");
                Utils.utilsLogInfo(true, "Delete file that exceed storage space: " + imageInfo.getFilePath() + "!!!");
                if (queues.get(appWidgetId).getRemoveCount() < LEFT_PREVIOUS_IMAGES_NUM) {
                    break;
                }
                deleteIndex = queues.get(appWidgetId).pop();
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
                ImageInfo item = imageInfos.get(taskInfo.appWidgetId).get(taskInfo.position);
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
                boolean haveSameSize = getFileSize(item.getFile())
                        == item.getSeafDirent().getFileSize();
                if (info.state == TaskState.FINISHED && haveSameSize) {
                    queues.get(taskInfo.appWidgetId).push(taskInfo.position);
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
        PendingIntent widgetIntent = PendingIntent.getActivity(getApplicationContext(), 0, activityIntent, PendingIntent.FLAG_CANCEL_CURRENT);
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
                    RemoteViews views = getDefalutRemoteViews(appWidgetId);
                    appWidgetManager.updateAppWidget(appWidgetId, views);
                    continue;
                }
                if (Utils.isNetworkOn() && tasksInProgress.size() < ONCE_UPLOAD_IMAGE_NUM * 2) {
                    if (queues.get(appWidgetId).getCount() < LEFT_FOLLOW_UP_MIN_IMAGES_NUM) {
                        shouldDownloads.put(appWidgetId, true);
                    }
                    if (queues.get(appWidgetId).getCount() < LEFT_FOLLOW_UP_MAX_IMAGES_NUM && shouldDownloads.get(appWidgetId)) {
                        Log.d(DEBUG_TAG, "Download new images.");
                        int addTaskNum = ONCE_UPLOAD_IMAGE_NUM;
                        while (addTaskNum > 0) {
                            downloadImages(appWidgetId, queues.get(appWidgetId).nextBoundary());
                            --addTaskNum;
                        }
                    }
                }
                if (queues.get(appWidgetId).isEmpty()) {
                    continue;
                }
                boolean flag = false;
                int index = queues.get(appWidgetId).next();
                while (index > 0 && index < imageInfo.size()) {
                    ImageInfo item = imageInfo.get(index);
                    Bitmap image = item.getBitMap();
                    if (image == null) {
                        item.deleteStorage();
                        index = queues.get(appWidgetId).next();
                        continue;
                    }
                    Log.d(DEBUG_TAG, "Set new images.");
                    RemoteViews views = new RemoteViews(context.getPackageName(), R.layout.loop_images_widget);
                    views.setImageViewBitmap(R.id.loopimages_imageview, image);
                    views = setRemoteViewOnClickActivity(views, appWidgetId, item.getDirInfo().toString(), item.getSeafDirent().name);
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

            if (imageInfos == null || queues == null || shouldDownloads == null) {
                updateWidgetSignal = UPDATE_ALL_WIDGETS;
            }

            int tAppWidgetIds[];
            if (updateWidgetSignal != UPDATE_ALL_WIDGETS) {
                tAppWidgetIds = new int[]{updateWidgetSignal};
            } else {
                tAppWidgetIds = appWidgetManager.getAppWidgetIds(new ComponentName(context, LoopImagesWidget.class));
            }

            if (updateWidgetSignal == UPDATE_ALL_WIDGETS) {
                imageInfos = new HashMap<Integer, List<ImageInfo>>();
                queues = new HashMap<Integer, LazyQueue>();
                shouldDownloads = new HashMap<Integer, Boolean>();
            }

            Map<String, List<ImageInfo>> dirImageInfoMapping = new HashMap<String, List<ImageInfo>>();

            Map<Integer, List<DirInfo>> widgetDirInfos = new HashMap<Integer, List<DirInfo>>();
            for (int appWidgetId : tAppWidgetIds) {
                widgetDirInfos.put(appWidgetId, LoopImagesWidgetConfigureActivity.getDirInfo(getApplicationContext(), appWidgetId));
            }

            for (int i = 0; i < tAppWidgetIds.length; ++i) {
                int appWidgetId = tAppWidgetIds[i];

                imageInfos.put(appWidgetId, Lists.newArrayList());
                queues.put(appWidgetId, new LazyQueue(1));
                shouldDownloads.put(appWidgetId, true);

                List<DirInfo> dirInfos = widgetDirInfos.get(appWidgetId);
                for (DirInfo info : dirInfos) {
                    if (dirImageInfoMapping.containsKey(info.toString())) {
                        imageInfos.get(appWidgetId).addAll(dirImageInfoMapping.get(info.toString()));
                        continue;
                    }
                    DataManager dataManager = new DataManager(info.getAccount());
                    List<ImageInfo> files = Lists.newArrayList();
                    DirentCache cache = dataManager.getDirentCache(info.getDirId());

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
                            files.add(new ImageInfo(info, k));
                        }
                    }
                    imageInfos.get(appWidgetId).addAll(files);
                    queues.get(appWidgetId).updateSize(imageInfos.get(appWidgetId).size());
                    dirImageInfoMapping.put(info.toString(), files);
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
        public int position;

        public TaskInfo(int taskID, int appWidgetId, int position){
            this.taskID = taskID;
            this.appWidgetId = appWidgetId;
            this.position = position;
        }
    }

    protected class LazyQueue{
        private LinkedList<Integer> data;
        private LinkedList<Integer> remove;
        private int size;
        private int boundary;

        public LazyQueue(int size) {
            this.size = size;
            boundary = 0;
            data = Lists.newLinkedList();
            remove = Lists.newLinkedList();
        }

        public void push(int i){
            data.add(i);
        }

        public int pop(){
            if(remove.size() > 0){
                return remove.pop();
            }
            return -1;
        }

        public int nextBoundary(){
            int res = boundary;
            boundary = (boundary + 1)%size;
            return res;
        }

        public int getCount(){
            return data.size();
        }

        public int getRemoveCount(){
            return remove.size();
        }

        public boolean isEmpty(){
            return data.isEmpty();
        }

        public int next(){
            if(data.size() <= 0 && remove.size() <= 0){
                return -1;
            }
            if(data.size() <= 0){
                int res = remove.pop();
                remove.add(res);
                return res;
            }
            int res = data.pop();
            remove.add(res);
            return res;
        }

        public int back(){
            if(data.size() <= 0){
                return -1;
            }
            return data.getLast();
        }

        public void recoverAll(){
            for(int i = (int)remove.size()-1; i>=0; --i){
                data.add(remove.get(i));
            }
            remove.clear();
        }

        public void updateSize(int size){
            this.size = size;
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


    class ImageInfo{
        private DirInfo dirInfo;
        private int index;
        private static final int MAX_BITMAP_EDGE = 1024;

        public ImageInfo(DirInfo dirInfo, int index){
            this.dirInfo = dirInfo;
            this.index = index;
        }

        public DirInfo getDirInfo() {
            return dirInfo;
        }

        public SeafDirent getSeafDirent() {
            DataManager dataManager = new DataManager(dirInfo.getAccount());
            DirentCache cache = dataManager.getDirentCache(dirInfo.getDirId());
            return cache.get(index);
        }

        public String getFilePath(){
            return Utils.pathJoin(getDirInfo().getDirPath(), getSeafDirent().name);
        }

        public File getFile(){
            DataManager dataManager = new DataManager(getDirInfo().getAccount());
            File file = dataManager.getLocalRepoFile(getDirInfo().getRepoName(), getDirInfo().getRepoId(), getFilePath());
            return file;
        }

        public boolean exist(){
            return getFile() != null;
        }

        public Bitmap getBitMap(){
            File file = getFile();
            if(file == null){
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
}