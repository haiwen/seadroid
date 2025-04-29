package com.seafile.seadroid2.framework.model.sdoc;

import com.seafile.seadroid2.framework.model.BaseModel;

import java.util.List;

public class OutlineItemModel extends BaseModel {
    public String id;
    public String type;
    public String text;
    public boolean indent;
    public List<OutlineItemModel> children;
    public SDocDataModel data;
}
