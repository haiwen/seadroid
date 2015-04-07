package com.seafile.seadroid2.util;

import java.io.File;
import java.io.FileFilter;
import java.util.List;

import android.text.TextUtils;
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

    private static final FileFilter MEDIA_FILTER = new FileFilter() {
        @Override
        public boolean accept(File file) {
            final String fileName = file.getName();
            // Return files only (not directories) and skip hidden files
            for (String ext : IMAGE_EXTENTION) {
                if (file.getName().toLowerCase().endsWith("." + ext)
                        && !fileName.startsWith(HIDDEN_PREFIX))
                    return true;
            }

            for (String ext : VIDEO_EXTENTION) {
                if (file.getName().toLowerCase().endsWith("." + ext)
                        && !fileName.startsWith(HIDDEN_PREFIX))
                    return true;
            }
            return false;
        }
    };

    private static final String IMAGE_EXTENTION[] = {
            "bmp",
            "cgm",
            "g3",
            "gif",
            "ief",
            "jpe",
            "jpeg",
            "jpg",
            "png",
            "btif",
            "svg",
            "svgz",
            "tif",
            "tiff",
            "psd",
            "dwg",
            "dxf",
            "fbs",
            "fpx",
            "fst",
            "mmr",
            "rlc",
            "mdi",
            "npx",
            "wbmp",
            "xif",
            "ras",
            "ico",
            "pcx",
            "pct",
            "pic",
            "xbm",
            "xwd",
            "bpg"
    };

    private static final String VIDEO_EXTENTION[] = {
            "3gp",
            "3gpp",
            "3g2",
            "gpp2",
            "h261",
            "h263",
            "h264",
            "jpgv",
            "jpgm",
            "jpm",
            "mj2",
            "mp4",
            "mp4v",
            "mpg4",
            "m1v",
            "m2v",
            "mpa",
            "mpe",
            "mpg",
            "mpeg",
            "ogv",
            "mov",
            "qt",
            "fvt",
            "m4u",
            "pyv",
            "viv",
            "f4v",
            "fli",
            "flv",
            "m4v",
            "asf",
            "asx",
            "avi",
            "wmv",
            "wmx",
            "mkv"
    };

    private static final String AUDIO_EXTENTION[] = {
            "aac",
            "adp",
            "aif",
            "aifc",
            "aiff",
            "amr",
            "ape",
            "au",
            "dts",
            "eol",
            "flac",
            "kar",
            "lvp",
            "m2a",
            "m3a",
            "m3u",
            "m4a",
            "mid",
            "mid",
            "mka",
            "mp2",
            "mp3",
            "mpga",
            "oga",
            "ogg",
            "pya",
            "ram",
            "rmi",
            "snd",
            "spx",
            "wav",
            "wax",
            "wma",
    };

    /**
     * {get photo path from sd-card @link
     * http://stackoverflow.com/questions/3873496
     * /how-to-get-image-path-from-images-stored-on-sd-card}
     */
    private static final FileFilter MEDIA_DIR_FILTER = new FileFilter() {
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

                    // For each file in the directory
                    for (File file : listOfFiles) {
                        // Check if the extension is one of the supported file types
                        for (String ext : VIDEO_EXTENTION) {
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

    private static List<File> getPathsByFilter(String path, FileFilter filter) {

        List<File> list = Lists.newArrayList();

        // Current directory File instance
        final File pathDir = new File(path);
        if (!pathDir.isDirectory()) {
            return list;
        }
        // List folders in this directory with the directory filter
        final File[] dirs = pathDir.listFiles(MEDIA_DIR_FILTER);
        if (dirs != null) {
            // Add each folder to the File list for the list adapter
            for (File dir : dirs) {
                // List photos inside each directory with the photo filter
                final File[] photoFiles = dir.listFiles(filter);
                if (photoFiles != null) {
                    // Add each file to the File list for the list adapter
                    for (File file : photoFiles) {
                        list.add(file);
                    }
                }
            }
        }
        // List photos in this directory with the photo filter
        final File[] photos = pathDir.listFiles(filter);
        if (photos != null) {
            // Add each file to the File list for the list adapter
            for (File file : photos) {
                list.add(file);
            }
        }
        return list;
    }

    private static List<File> getAbsolutePathList(String path, boolean isVideosAllowed) {
        if (isVideosAllowed)
            return getPathsByFilter(path, MEDIA_FILTER);
        else
            return getPathsByFilter(path, PHOTO_FILTER);
    }

    public static boolean isImageType(String path) {
        if (path == null || TextUtils.isEmpty(path))
            return false;

        for (String ext : IMAGE_EXTENTION) {
            if (path.endsWith("." + ext))
                return true;
        }

        return false;
    }

    public static boolean isVideoType(String path) {
        if (path == null || TextUtils.isEmpty(path))
            return false;

        for (String ext : VIDEO_EXTENTION) {
            if (path.endsWith("." + ext))
                return true;
        }

        return false;
    }

    /**
     * Only files within auto scanned or custom selected directories could be uploaded
     * @param photo
     * @param isVideosAllowed
     * @param isCustom
     * @return
     */
    public static boolean isPathValid(File photo, boolean isVideosAllowed, boolean isCustom) {
        List<File> paths;
        if (isCustom) { // custom upload directories
            paths = getCustomPathList(isVideosAllowed);
        } else { // auto scan device
            paths = getAutoScannedPathList(isVideosAllowed);
        }

        if (paths.isEmpty())
            return false;

        return paths.contains(photo);
    }

    /**
     * get path list for uploading files picked by user
     * @param isAllowed if allow videos upload
     * @return
     */
    public static List<File> getCustomPathList(boolean isAllowed) {
        List<File> list = Lists.newArrayList();

        // user selected directories
        List<String> paths = ImageManager.getCustomDirList();
        for (String path : paths) {
            for (File sf : getAbsolutePathList(path, isAllowed)) {
                if (!list.contains(sf)) {
                    list.add(sf);
                }
            }
        }
        return list;
    }

    public static List<File> getAutoScannedPathList(boolean isAllowed) {
        List<File> list = Lists.newArrayList();

        // auto scanned directories
        List<String> paths = ImageManager.getAutoScannedPathList();
        for (String path : paths) {
            for (File sf : getAbsolutePathList(path, isAllowed)) {
                if (!list.contains(sf)) {
                    list.add(sf);
                }
            }
        }
        return list;
    }
}
