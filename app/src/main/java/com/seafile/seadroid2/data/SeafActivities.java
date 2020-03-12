package com.seafile.seadroid2.data;

import java.util.List;

/**
 * Seafile Activities data model
 */
public class SeafActivities {

    public List<SeafEvent> events;

    public boolean more;

    public int offset;

    public List<SeafEvent> getEvents() {
        return events;
    }

    public void setEvents(List<SeafEvent> events) {
        this.events = events;
    }

    public boolean isMore() {
        return more;
    }

    public void setMore(boolean more) {
        this.more = more;
    }

    public int getOffset() {
        return offset;
    }

    public void setOffset(int offset) {
        this.offset = offset;
    }

    public SeafActivities(List<SeafEvent> events, int offset, boolean more) {
        this.events = events;
        this.offset = offset;
        this.more = more;
    }
}
