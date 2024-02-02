package com.seafile.seadroid2.ui.base.adapter;

import com.chad.library.adapter4.BaseMultiItemAdapter;
import com.seafile.seadroid2.util.SLogs;

public abstract class BaseMultiAdapter<M> extends BaseMultiItemAdapter<M> {
    public void d(String d) {
        SLogs.d(this.getClass().getSimpleName() + " => " + d);
    }
}
