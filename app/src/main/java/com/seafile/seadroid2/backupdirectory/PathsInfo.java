package com.seafile.seadroid2.backupdirectory;

import java.io.Serializable;

public class PathsInfo implements Serializable {
    private String email;

    private String paths;


    public PathsInfo(String paths,String email) {
        this.paths=paths;
        this.email=email;
    }


    public void setEmail(String email) {
        this.email = email;
    }

    public String getEmail() {
        return email;
    }

    public void setPaths(String paths) {
        this.paths = paths;
    }

    public String getPaths() {
        return paths;
    }

}
