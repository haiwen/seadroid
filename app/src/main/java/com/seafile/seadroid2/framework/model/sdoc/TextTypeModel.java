package com.seafile.seadroid2.framework.model.sdoc;

public class TextTypeModel {
    /**
     * undo
     * redo
     * paragraph,title,subtitle,h1,h2,h3,h4,h5,h6
     * unordered-list
     * ordered-list
     * checkbox
     */
    public String type;

    public TextTypeModel(String type) {
        this.type = type;
    }
}
