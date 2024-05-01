package com.seafile.seadroid2.ui.base.adapter;

import com.chad.library.adapter4.BaseQuickAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.framework.util.SLogs;

public abstract class BaseAdapter<M, VH extends BaseViewHolder> extends BaseQuickAdapter<M, VH> {
    public void d(String d) {
        SLogs.d(this.getClass().getSimpleName() + " => " + d);
    }


}
