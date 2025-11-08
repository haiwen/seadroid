package com.seafile.seadroid2.framework.model.sdoc;

public class TextTypeModel {
    /***
     * text-style:paragraph,title,subtitle,h1,h2,h3,h4,h5,h6<br/>
     * checkbox:true/false<br/>
     */
    public String state;

    /**
     * undo
     * redo
     * text-style
     * unordered-list
     * ordered-list
     * checkbox
     */
    public String type;

    public int line;
}
