package com.seafile.seadroid2.backupdirectory;

import org.litepal.crud.LitePalSupport;

import java.util.ArrayList;
import java.util.List;

public class FolderBean extends LitePalSupport {

    private String email;

    private String repoID;

    private String repoName;

    private String name;

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    private List<String> selectFolder = new ArrayList<String>();

    public List<String> getSelectFolder() {
        return selectFolder;
    }

    public void setSelectFolder(List<String> selectFolder) {
        this.selectFolder = selectFolder;
    }


    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }

    public String getRepoID() {
        return repoID;
    }

    public void setRepoID(String repoID) {
        this.repoID = repoID;
    }

    public String getRepoName() {
        return repoName;
    }

    public void setRepoName(String repoName) {
        this.repoName = repoName;
    }

}
