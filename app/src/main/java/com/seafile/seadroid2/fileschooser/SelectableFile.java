package com.seafile.seadroid2.fileschooser;

import java.io.File;
import java.io.FileFilter;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.SeafItem;
import com.seafile.seadroid2.util.Utils;

public class SelectableFile implements SeafItem {

    private boolean selected;
    private File file;

    public SelectableFile(String path) {
        selected = false;
        file = new File(path);
    }

    public SelectableFile(File file, boolean isSelected) {
        this.file = file;
        selected = isSelected;
    }

    public void setSelected(boolean isSelected) {
        selected = isSelected;
    }

    public boolean isSelected() {
        return selected;
    }

    public boolean isDirectory() {
        return file.isDirectory();
    }

    public boolean isFile() {
        return file.isFile();
    }

    public String getName() {
        return file.getName();
    }

    @Override
    public boolean equals(Object o) {

        if (this == o) {
            return true;
        }

        if (!(o instanceof SelectableFile)) {
            return false;
        }

        SelectableFile lhs = (SelectableFile) o;

        return file.equals(lhs.getFile()) && selected == lhs.isSelected();
    }

    public SelectableFile[] listFiles(FileFilter fileFilter) {
        File[] files = file.listFiles(fileFilter);
        SelectableFile[] selectedFiles = new SelectableFile[files.length];
        for (int i = 0; i < files.length; ++i) {
            selectedFiles[i] = new SelectableFile(files[i], false);
        }
        return selectedFiles;
    }

    public long length() {
        return file.length();
    }

    public String getAbsolutePath() {
        return file.getAbsolutePath();
    }

    public File getFile() {
        return file;
    }

    public void toggleSelected() {
        selected = !selected;
    }

    @Override
    public String getTitle() {
        return getName();
    }

    @Override
    public String getSubtitle() {
        String timestamp = Utils.translateCommitTime(file.lastModified());
        if (isDirectory())
            return timestamp;
        return Utils.readableFileSize(file.length()) + ", " + timestamp;
    }

    @Override
    public int getIcon() {
        if (isDirectory())
            return R.drawable.folder;
        return Utils.getFileIcon(getTitle());
    }

}
