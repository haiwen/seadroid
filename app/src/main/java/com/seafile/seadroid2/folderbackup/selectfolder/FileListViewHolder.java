package com.seafile.seadroid2.folderbackup.selectfolder;

import android.support.v7.widget.RecyclerView;
import android.view.View;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.RelativeLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;


public class FileListViewHolder extends RecyclerView.ViewHolder {
    protected ImageView imgvFiletype;
    protected TextView tvFileName, tvFileDetail;
    protected CheckBox checkBoxFile;
    protected RelativeLayout layoutRoot;

    public FileListViewHolder(View itemView) {
        super(itemView);
        layoutRoot = (RelativeLayout) itemView.findViewById(R.id.ll_root);
        imgvFiletype = (ImageView) itemView.findViewById(R.id.iv_file_type_fileitem);
        tvFileName = (TextView) itemView.findViewById(R.id.tv_file_name_fileitem);
        tvFileDetail = (TextView) itemView.findViewById(R.id.tv_file_detail_fileitem);
        checkBoxFile = (CheckBox) itemView.findViewById(R.id.checkbox_file_fileitem);
    }
}
