package com.seafile.seadroid2.ui.selector.folder_selector;

import androidx.recyclerview.widget.RecyclerView;

import android.media.Image;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;


public class NavPathViewHolder extends RecyclerView.ViewHolder {

    protected TextView name;
    protected LinearLayout llRoot;
    protected ImageView icon;

    public NavPathViewHolder(View itemView) {
        super(itemView);
        llRoot = (LinearLayout) itemView.findViewById(R.id.ll_root);
        name = (TextView) itemView.findViewById(R.id.item_nav_path_text);
        icon = (ImageView) itemView.findViewById(R.id.item_nav_path_icon);
    }
}
