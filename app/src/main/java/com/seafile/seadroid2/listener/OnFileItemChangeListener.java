package com.seafile.seadroid2.listener;

import com.seafile.seadroid2.ui.selector.folder_selector.FileBean;

public interface OnFileItemChangeListener {
    void onChanged(FileBean fileBean, int position, boolean isChecked);
}
