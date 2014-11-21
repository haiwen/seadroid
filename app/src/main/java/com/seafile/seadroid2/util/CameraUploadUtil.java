package com.seafile.seadroid2.util;

import java.io.File;
import java.io.FileFilter;
import java.util.List;

import android.util.Log;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.gallery.ImageManager;

public class CameraUploadUtil {
    
    private static final String HIDDEN_PREFIX = ".";

    private static final FileFilter PHOTO_FILTER = new FileFilter() {
        @Override
        public boolean accept(File file) {
            final String fileName = file.getName();
            // Return files only (not directories) and skip hidden files
            for (String ext : IMAGE_EXTENTION) {
                if (file.getName().endsWith("." + ext)
                        && !fileName.startsWith(HIDDEN_PREFIX))
                    return true;
            }
            return false;
        }
    };

    private static final String IMAGE_EXTENTION[] = { 
        "png",
        "PNG", 
        "jpg", 
        "JPG",
        "jpeg", 
        "JPEG", 
        "bmp", 
        "BMP", 
        "gif", 
        "GIF" 
        };

    /**
     * {get photo path from sd-card @link
     * http://stackoverflow.com/questions/3873496
     * /how-to-get-image-path-from-images-stored-on-sd-card}
     */
    private static final FileFilter PHOTO_DIR_FILTER = new FileFilter() {
        @Override
        public boolean accept(File folder) {
            try {
                // Checking only directories
                if (folder.isDirectory()
                        && !folder.getName().startsWith(HIDDEN_PREFIX)) {
                    File[] listOfFiles = folder.listFiles();

                    if (listOfFiles == null)
                        return false;

                    // For each file in the directory
                    for (File file : listOfFiles) {
                        // Check if the extension is one of the supported file types
                        for (String ext : IMAGE_EXTENTION) {
                            if (file.getName().endsWith("." + ext))
                                return true;
                        }
                    }
                }
                return false;
            } catch (SecurityException e) {
                Log.v("debug", "Access Denied");
                return false;
            }
        }
    };

    private static List<File> getPhotosAbsolutePathList(String path) {

        List<File> list = Lists.newArrayList();

        // Current directory File instance
        final File pathDir = new File(path);
        if (!pathDir.isDirectory()) {
            return null;
        }
        // List folders in this directory with the directory filter
        final File[] dirs = pathDir.listFiles(PHOTO_DIR_FILTER);
        if (dirs != null) {
            // Add each folder to the File list for the list adapter
            for (File dir : dirs) {
                // List photos inside each directory with the photo filter
                final File[] photoFiles = dir.listFiles(PHOTO_FILTER);
                if (photoFiles != null) {
                    // Add each file to the File list for the list adapter
                    for (File file : photoFiles) {
                        list.add(file);
                    }
                }
            }
        }
        // List photos in this directory with the photo filter
        final File[] photos = pathDir.listFiles(PHOTO_FILTER);
        if (photos != null) {
            // Add each file to the File list for the list adapter
            for (File file : photos) {
                list.add(file);
            }
        }
        return list;
    }

    public static List<File> getAllPhotosAbsolutePathList() {
        List<File> list = Lists.newArrayList();
        List<File> photoAbsolutePathList = Lists.newArrayList();

        List<String> paths = ImageManager.getAllPath();
        for (String path : paths) {
            photoAbsolutePathList = getPhotosAbsolutePathList(path);
            if (photoAbsolutePathList != null) {
                for (File sf : photoAbsolutePathList) {
                    if (!list.contains(sf)) {
                        list.add(sf);
                    }
                }
            }
        }
        return list;
    }
}
