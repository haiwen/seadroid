package com.seafile.seadroid2.framework.data.model.sdoc;

import com.seafile.seadroid2.framework.data.model.BaseModel;

import java.util.List;

public class OutlineItemModel extends BaseModel {
    public String id;
    public String type;
    public String text;
    public boolean indent;
    public List<OutlineItemModel> children;
    public SDocDataModel data;
}
