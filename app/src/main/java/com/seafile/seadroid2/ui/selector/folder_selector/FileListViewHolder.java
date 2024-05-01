package com.seafile.seadroid2.ui.selector.folder_selector;

import android.view.View;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;

import com.google.android.material.checkbox.MaterialCheckBox;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;


public class FileListViewHolder extends BaseViewHolder {
    protected ImageView imgvFiletype;
    protected TextView tvFileName, tvFileDetail;
    protected MaterialCheckBox checkBoxFile;

    public FileListViewHolder(View itemView) {
        super(itemView);
        imgvFiletype = (ImageView) itemView.findViewById(R.id.iv_file_type_fileitem);
        tvFileName = (TextView) itemView.findViewById(R.id.tv_file_name_fileitem);
        tvFileDetail = (TextView) itemView.findViewById(R.id.tv_file_detail_fileitem);
        checkBoxFile = (MaterialCheckBox) itemView.findViewById(R.id.checkbox_file_fileitem);
    }
}
