package com.seafile.seadroid2.framework.util;

import android.os.AsyncTask;
import androidx.annotation.Nullable;

import java.lang.ref.WeakReference;

public abstract class SupportAsyncTask<ContextParams, Params, Progress, Result> extends AsyncTask<Params, Progress, Result>{

    private WeakReference<ContextParams> mContextParams = null;

    @Nullable
    public ContextParams getContextParam() {
        if (mContextParams == null) {
            return null;
        }
        return mContextParams.get();
    }

    public SupportAsyncTask(ContextParams contextParams) {
        if (contextParams != null) {
            this.mContextParams = new WeakReference<ContextParams>(contextParams);
        }
    }
}
