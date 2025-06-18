package com.seafile.seadroid2.ui.selector.folder_selector;

import android.view.View;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.recyclerview.widget.RecyclerView;
import com.seafile.seadroid2.R;

public class VolumeViewHolder extends RecyclerView.ViewHolder {

    public TextView tvDescription;
    public LinearLayout llRoot;

    public VolumeViewHolder(View itemView) {
        super(itemView);
        tvDescription = itemView.findViewById(R.id.tv_description);
        llRoot = itemView.findViewById(R.id.ll_root);
    }
}
