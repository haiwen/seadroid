package com.seafile.seadroid2.framework.file_monitor;

import org.apache.commons.io.IOCase;
import org.apache.commons.io.monitor.FileAlterationObserver;
import org.apache.commons.io.monitor.FileEntry;

import java.io.File;
import java.io.FileFilter;

public class SingleFolderPathFileObserver extends FileAlterationObserver {

    public SingleFolderPathFileObserver(File directory) {
        super(directory);
    }

    public SingleFolderPathFileObserver(File directory, FileFilter fileFilter) {
        super(directory, fileFilter);
    }

    public SingleFolderPathFileObserver(File directory, FileFilter fileFilter, IOCase ioCase) {
        super(directory, fileFilter, ioCase);
    }

    protected SingleFolderPathFileObserver(FileEntry rootEntry, FileFilter fileFilter, IOCase ioCase) {
        super(rootEntry, fileFilter, ioCase);
    }

    public SingleFolderPathFileObserver(String directoryName) {
        super(directoryName);
    }

    public SingleFolderPathFileObserver(String directoryName, FileFilter fileFilter) {
        super(directoryName, fileFilter);
    }

    public SingleFolderPathFileObserver(String directoryName, FileFilter fileFilter, IOCase ioCase) {
        super(directoryName, fileFilter, ioCase);
    }
}
