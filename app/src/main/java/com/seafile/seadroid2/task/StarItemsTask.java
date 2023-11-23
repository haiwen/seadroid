package com.seafile.seadroid2.task;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.listener.OnCallback;
import com.seafile.seadroid2.ui.activity.BrowserActivity;
import com.seafile.seadroid2.util.SupportAsyncTask;

/**
 * star repo、file、dir
 */
public class StarItemsTask extends SupportAsyncTask<BrowserActivity, Void, Void, Void> {
    private String repoId;
    private String path;
    private boolean is_starred;
    private SeafException err;

    private OnCallback onCallback;

    public StarItemsTask(BrowserActivity browserActivity, String repoId, String path, boolean starred) {
        super(browserActivity);
        this.repoId = repoId;
        this.path = path;
        is_starred = starred;
    }

    public void setOnCallback(OnCallback onCallback) {
        this.onCallback = onCallback;
    }

    @Override
    protected Void doInBackground(Void... params) {
        try {
            if (getContextParam() != null) {
                if (is_starred) {
                    getContextParam().getDataManager().unstarItems(repoId, path);
                } else {
                    getContextParam().getDataManager().starItems(repoId, path);
                }
            }
        } catch (SeafException e) {
            err = e;
        }

        return null;
    }

    @Override
    protected void onPostExecute(Void v) {
        if (err != null) {
            if (getContextParam() != null) {
                if (is_starred) {
                    getContextParam().showShortToast(getContextParam(), R.string.unstar_file_failed);
                } else {
                    getContextParam().showShortToast(getContextParam(), R.string.star_file_failed);
                }
            }
            if (onCallback != null) {
                onCallback.onFailed();
            }
            return;
        }
        
        if (getContextParam() != null) {
            if (is_starred) {
                getContextParam().showShortToast(getContextParam(), R.string.unstar);
            } else {
                getContextParam().showShortToast(getContextParam(), R.string.star_file_succeed);
            }
        }
        if (onCallback != null) {
            onCallback.onSuccess();
        }
    }
}
