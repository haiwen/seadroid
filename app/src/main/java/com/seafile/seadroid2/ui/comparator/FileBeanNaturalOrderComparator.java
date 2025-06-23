package com.seafile.seadroid2.ui.comparator;

import com.seafile.seadroid2.ui.selector.folder_selector.FileBean;

public class FileBeanNaturalOrderComparator extends NaturalOrderComparator<FileBean> {
    @Override
    public String extractName(FileBean t) {
        return t.getFileName();
    }
}
