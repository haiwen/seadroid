package com.seafile.seadroid2.data;

import java.io.Serializable;

/**
 * Photo entity for displaying photos in gallery
 */
public class SeafPhoto implements Serializable {
    public static final long serialVersionUID = 0L;

    /** display name */
    private String name;
    /** link for loading thumbnail */
    private String link;
    /** related {@link SeafDirent} */
    private SeafDirent dirent;

    public SeafPhoto(String link, SeafDirent dirent) {
        this.link = link;
        this.dirent = dirent;
        this.name = dirent.name;
    }

    public String getName() {
        return name;
    }

    public String getLink() {
        return link;
    }

    public SeafDirent getDirent() {
        return dirent;
    }

}