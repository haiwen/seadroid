package com.seafile.seadroid2.folderbackup.selectfolder;

import android.view.View;

public interface OnFileItemClickListener {
    void onItemClick(int position);
    void onCheckBoxClick(View view, int position);
}
