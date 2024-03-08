package com.seafile.seadroid2.worker.observer;

import android.content.ContentResolver;
import android.content.Context;
import android.database.ContentObserver;
import android.net.Uri;
import android.os.Handler;
import android.provider.MediaStore;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.worker.BackgroundJobManagerImpl;

public class MediaContentObserver extends ContentObserver {
    private Context mContext;
    private final Uri mImagesUri = MediaStore.Images.Media.EXTERNAL_CONTENT_URI;
    private final Uri mVideosUri = MediaStore.Video.Media.EXTERNAL_CONTENT_URI;

    /**
     * Creates a content observer.
     *
     * @param handler The handler to run {@link #onChange} on, or null if none.
     */
    public MediaContentObserver(Context context, Handler handler) {
        super(handler);
        mContext = context;
    }

    @Override
    public void onChange(boolean selfChange) {
        super.onChange(selfChange);

        ToastUtils.showLong("检测到新文件，开始 Media 任务");
        BackgroundJobManagerImpl.getInstance().scheduleOneTimeMediaSyncJob();
    }

    public void register() {
        ContentResolver contentResolver = mContext.getContentResolver();
        contentResolver.registerContentObserver(mImagesUri, true, this);
        contentResolver.registerContentObserver(mVideosUri, true, this);
    }

    public void unregister() {
        mContext.getContentResolver().unregisterContentObserver(this);
    }
}
