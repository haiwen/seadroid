package com.seafile.seadroid.data;


public class SeafGroup implements SeafItem {
    
    private String name;
    
    public SeafGroup(String name) {
        this.name = name;
    }

    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public String getSubtitle() {
        return null;
    }

    @Override
    public int getIcon() {
        return 0;
    }
    
}
