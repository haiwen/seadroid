package com.seafile.seadroid2.data;

import com.google.common.collect.Lists;

import java.util.List;

/**
 * Event details tree of each commit
 */
public class EventDetailsTree {
    private List<EventDetailsFileItem> items;

    public EventDetailsTree() {
        items = Lists.newArrayList();
    }

    public List<EventDetailsFileItem> setCommitDetails(CommitDetails details) {

        processEventCategory(details.added_files, "Added files", EType.FILE_ADDED);
        processEventCategory(details.deleted_files, "Deleted files", EType.FILE_DELETED);
        processEventCategory(details.modified_files, "Modified files", EType.FILE_MODIFIED);

        processEventCategory(details.added_dirs, "Added folders", EType.DIR_ADDED);
        processEventCategory(details.deleted_dirs, "Deleted folders", EType.DIR_DELETED);

        // renamed files is a list of (before rename, after rename) pair
        /*List<String> renamed_files;
        for (int i = 0, n = details.renamed_files.size(); i < n; i++) {
            renamed_files.push_back(details.renamed_files[i].second);
        }
        processEventCategory(renamed_files, "Renamed files", EType.FILE_RENAMED);*/

        return items;
    }

    private void processEventCategory(List<String> files, String desc, EType etype) {
        if (files == null || files.isEmpty()) {
            return;
        }

        for (int i = 0, n = files.size(); i < n; i++) {
            EventDetailsFileItem item = new EventDetailsFileItem(files.get(i), desc, etype);
            items.add(item);
        }
    }

    public enum EType {
        FILE_ADDED,
        FILE_DELETED,
        FILE_MODIFIED,
        FILE_RENAMED,
        DIR_ADDED,
        DIR_DELETED
    }

    public class EventDetailsFileItem {
        public String file, desc;
        public EType eType;

        public EventDetailsFileItem(String file, String desc, EType etype) {
            this.file = file;
            this.desc = desc;
            this.eType = etype;
        }

        public boolean isFileOpenable(EType etype_) {
            return etype_ == EType.FILE_ADDED ||
                    etype_ == EType.FILE_MODIFIED ||
                    etype_ == EType.FILE_RENAMED ||
                    etype_ == EType.DIR_ADDED;
        }
    }
}
