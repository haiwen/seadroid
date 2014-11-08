package com.seafile.seadroid2.fileschooser;

import java.io.File;
import java.util.List;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.util.Utils;

import android.content.Context;
import android.os.FileObserver;
import android.support.v4.content.AsyncTaskLoader;

public class FileLoader extends AsyncTaskLoader<List<SelectableFile>> {

    private static final int FILE_OBSERVER_MASK = FileObserver.CREATE
            | FileObserver.DELETE | FileObserver.DELETE_SELF
            | FileObserver.MOVED_FROM | FileObserver.MOVED_TO
            | FileObserver.MODIFY | FileObserver.MOVE_SELF;

    private FileObserver mFileObserver;

    private List<SelectableFile> mData;
    private String mPath;
    private List<File> mSelectedFiles;

    public FileLoader(Context context, String path, List<File> selectedFiles) {
        super(context);
        this.mPath = path;
        mSelectedFiles = Lists.newArrayList(selectedFiles);
    }

    @Override
    public List<SelectableFile> loadInBackground() {
        return Utils.getFileList(mPath, mSelectedFiles);
    }

    @Override
    public void deliverResult(List<SelectableFile> data) {
        if (isReset()) {
            onReleaseResources(data);
            return;
        }

        List<SelectableFile> oldData = mData;
        mData = data;

        if (isStarted())
            super.deliverResult(data);

        if (oldData != null && oldData != data)
            onReleaseResources(oldData);
    }

    @Override
    protected void onStartLoading() {
        if (mData != null)
            deliverResult(mData);

        if (mFileObserver == null) {
            mFileObserver = new FileObserver(mPath, FILE_OBSERVER_MASK) {
                @Override
                public void onEvent(int event, String path) {
                    onContentChanged();
                }
            };
        }
        mFileObserver.startWatching();

        if (takeContentChanged() || mData == null)
            forceLoad();
    }

    @Override
    protected void onStopLoading() {
        cancelLoad();
    }

    @Override
    protected void onReset() {
        onStopLoading();

        if (mData != null) {
            onReleaseResources(mData);
            mData = null;
        }
    }

    @Override
    public void onCanceled(List<SelectableFile> data) {
        super.onCanceled(data);

        onReleaseResources(data);
    }

    protected void onReleaseResources(List<SelectableFile> data) {

        if (mFileObserver != null) {
            mFileObserver.stopWatching();
            mFileObserver = null;
        }
    }
}