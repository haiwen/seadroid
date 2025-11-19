package com.seafile.seadroid2.framework.model.search;

import com.seafile.seadroid2.framework.model.BaseModel;

public class SearchFileModel extends BaseModel {
    public String path;
    public String type;
    public long size;    // size of file, 0 if type is dir
    public String mtime;
}
