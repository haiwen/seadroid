package com.seafile.seadroid2.framework.data;

import com.google.common.collect.Lists;

import java.util.List;

/**
 * Event details tree of each commit
 */
public class EventDetailsTree {
    private List<EventDetailsFileItem> items;
    private SeafEvent event;

    public EventDetailsTree(SeafEvent event) {
        this.items = Lists.newArrayList();
        this.event = event;
    }

    public List<EventDetailsFileItem> setCommitDetails(CommitDetails details) {

        items.clear();

        processEventCategory(details.addedFiles, "Added files", EventDetailsFileItem.EType.FILE_ADDED);
        processEventCategory(details.deletedFiles, "Deleted files", EventDetailsFileItem.EType.FILE_DELETED);
        processEventCategory(details.modifiedFiles, "Modified files", EventDetailsFileItem.EType.FILE_MODIFIED);

        processEventCategory(details.addedDirs, "Added folders", EventDetailsFileItem.EType.DIR_ADDED);
        processEventCategory(details.deletedDirs, "Deleted folders", EventDetailsFileItem.EType.DIR_DELETED);

        // renamed files is a list of (before rename, after rename) pair
        List<String> renamedFiles = Lists.newArrayList();
        for (int i = 1, n = details.renamedFiles.size(); i < n; i += 2) {
            final String rename = details.renamedFiles.get(i);
            renamedFiles.add(rename);
        }
        processEventCategory(renamedFiles, "Renamed files", EventDetailsFileItem.EType.FILE_RENAMED);

        return items;
    }

    private void processEventCategory(List<String> files, String desc, EventDetailsFileItem.EType etype) {
        if (files == null || files.isEmpty()) {
            return;
        }

        for (int i = 0, n = files.size(); i < n; i++) {
            EventDetailsFileItem item = new EventDetailsFileItem(event, files.get(i), etype);
            items.add(item);
        }
    }
}
