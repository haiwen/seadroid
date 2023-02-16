package com.seafile.seadroid2.folderbackup.selectfolder;

import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;


public class TabbarFileViewHolder extends RecyclerView.ViewHolder {
    protected TextView tvName;
    protected LinearLayout llRoot;

    public TabbarFileViewHolder(View itemView) {
        super(itemView);
        llRoot = (LinearLayout) itemView.findViewById(R.id.ll_root);
        tvName = (TextView) itemView.findViewById(R.id.btn_item_tabbar);
    }
}
