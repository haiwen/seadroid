package com.seafile.seadroid2.ui.repo.repo_list;

public class ScrollState {
    public int index;
    public int top;

    public ScrollState(int index, int top) {
        this.index = index;
        this.top = top;
    }

    @Override
    public String toString() {
        return "ScrollState{" +
                "index=" + index +
                ", top=" + top +
                '}';
    }
}
