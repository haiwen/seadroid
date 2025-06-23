package com.seafile.seadroid2.ui.comparator;

import com.seafile.seadroid2.framework.db.entities.RepoModel;

public class RepoNaturalOrderComparator extends NaturalOrderComparator<RepoModel> {
    @Override
    public String extractName(RepoModel t) {
        return t.repo_name;
    }

//    private String extractName(Object obj) {
//        if (obj instanceof RepoModel) return ((RepoModel) obj).repo_name;
//        if (obj instanceof DirentModel) return ((DirentModel) obj).name;
//        if (obj instanceof FileBean) return ((FileBean) obj).getFileName();
//        if (obj instanceof String) return (String) obj;
//        throw new IllegalArgumentException("NaturalOrderComparator extractName() -> Unsupported type: " + obj.getClass().getName());
//    }
}
