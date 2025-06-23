package com.seafile.seadroid2.ui.comparator;

import com.seafile.seadroid2.framework.db.entities.DirentModel;

public class DirentNaturalOrderComparator extends NaturalOrderComparator<DirentModel> {
    @Override
    public String extractName(DirentModel t) {
        return t.name;
    }
}
