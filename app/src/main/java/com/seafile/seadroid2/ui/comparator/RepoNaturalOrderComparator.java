package com.seafile.seadroid2.ui.comparator;

import com.seafile.seadroid2.framework.db.entities.RepoModel;

public class RepoNaturalOrderComparator extends NaturalOrderComparator<RepoModel> {
    @Override
    public String extractName(RepoModel t) {
        return t.repo_name;
    }
}
