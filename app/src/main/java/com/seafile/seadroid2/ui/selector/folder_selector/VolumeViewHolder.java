package com.seafile.seadroid2.ui.selector.folder_selector;

import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.recyclerview.widget.RecyclerView;
import com.seafile.seadroid2.R;

public class VolumeViewHolder extends RecyclerView.ViewHolder {

    public TextView tvPath;
    public LinearLayout llRoot;

    public VolumeViewHolder(View itemView) {
        super(itemView);
        tvPath = itemView.findViewById(R.id.tv_path);
        llRoot = itemView.findViewById(R.id.ll_root);
    }
}
