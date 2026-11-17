package com.seafile.seadroid2.framework.model.sdoc;

import java.util.List;
import com.google.gson.annotations.SerializedName;

public class UndoRedoModel {
    @SerializedName("undos")
    public List<Object> undos;
    @SerializedName("redos")
    public List<Object> redos;
}
