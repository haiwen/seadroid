package com.seafile.seadroid2.data;

public class SeafCachedPhoto implements SeafItem {
    public int id;
    public String repoName;
    public String repoID;
    public String path;
    public String accountSignature;

    public SeafCachedPhoto() {
        id = -1;
    }

    @Override
    public String getTitle() {
        return path.substring(path.lastIndexOf('/') + 1);
    }

    @Override
    public String getSubtitle() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public int getIcon() {
        // TODO Auto-generated method stub
        return 0;
    }

    public String getAccountSignature() {
        return accountSignature;
    }
}
