package com.seafile.seadroid2.ui.base.adapter;

import com.blankj.utilcode.util.SizeUtils;
import com.chad.library.adapter.base.BaseQuickAdapter;

public abstract class ParentAdapter<M, VH extends BaseViewHolder> extends BaseQuickAdapter<M, VH> {
    public int DP_2 = SizeUtils.dp2px(2);
    public int DP_4 = SizeUtils.dp2px(4);
    public int DP_8 = SizeUtils.dp2px(8);
    public int DP_16 = SizeUtils.dp2px(16);
}
