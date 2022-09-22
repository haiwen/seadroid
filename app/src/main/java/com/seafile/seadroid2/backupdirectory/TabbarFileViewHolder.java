package com.seafile.seadroid2.backupdirectory;

import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.TextView;

import com.seafile.seadroid2.R;


public class TabbarFileViewHolder extends RecyclerView.ViewHolder {
    protected TextView tvName;

    public TabbarFileViewHolder(View itemView) {
        super(itemView);
        tvName = (TextView) itemView.findViewById(R.id.btn_item_tabbar);
    }
}
