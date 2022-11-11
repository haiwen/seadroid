package com.seafile.seadroid2.backupdirectory;

import android.view.View;

public interface OnFileItemClickListener {
    void click(int position);

    void checkBoxClick(View view, int position);
}
