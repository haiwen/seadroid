package com.seafile.seadroid2.ui.comparator;

public class StringNaturalOrderComparator extends NaturalOrderComparator<String> {
    @Override
    public String extractName(String t) {
        return t;
    }

//    private String extractName(Object obj) {
//        if (obj instanceof RepoModel) return ((RepoModel) obj).repo_name;
//        if (obj instanceof DirentModel) return ((DirentModel) obj).name;
//        if (obj instanceof FileBean) return ((FileBean) obj).getFileName();
//        if (obj instanceof String) return (String) obj;
//        throw new IllegalArgumentException("NaturalOrderComparator extractName() -> Unsupported type: " + obj.getClass().getName());
//    }
}
