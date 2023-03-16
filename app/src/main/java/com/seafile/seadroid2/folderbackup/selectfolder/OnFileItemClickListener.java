package com.seafile.seadroid2.folderbackup.selectfolder;

import android.view.View;

public interface OnFileItemClickListener {
    void itemClick(int position);
    void checkBoxClick(View view, int position);
}
