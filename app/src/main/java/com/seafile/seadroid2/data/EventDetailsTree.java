package com.seafile.seadroid2.data;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.data.EventDetailsFileItem.EType;

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

        processEventCategory(details.addedFiles, "Added files", EType.FILE_ADDED);
        processEventCategory(details.deletedFiles, "Deleted files", EType.FILE_DELETED);
        processEventCategory(details.modifiedFiles, "Modified files", EType.FILE_MODIFIED);

        processEventCategory(details.addedDirs, "Added folders", EType.DIR_ADDED);
        processEventCategory(details.deletedDirs, "Deleted folders", EType.DIR_DELETED);

        // renamed files is a list of (before rename, after rename) pair
        List<String> renamedFiles = Lists.newArrayList();
        for (int i = 1, n = details.renamedFiles.size(); i < n; i += 2) {
            final String rename = details.renamedFiles.get(i);
            renamedFiles.add(rename);
        }
        processEventCategory(renamedFiles, "Renamed files", EType.FILE_RENAMED);

        return items;
    }

    private void processEventCategory(List<String> files, String desc, EType etype) {
        if (files == null || files.isEmpty()) {
            return;
        }

        for (int i = 0, n = files.size(); i < n; i++) {
            EventDetailsFileItem item = new EventDetailsFileItem(event, files.get(i), etype);
            items.add(item);
        }
    }
}
