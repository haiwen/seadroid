package com.seafile.seadroid2.ui.comparator;

import java.io.File;

public class FileNaturalOrderComparator extends NaturalOrderComparator<File> {
    @Override
    public String extractName(File t) {
        return t.getName();
    }
}
