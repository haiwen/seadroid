package com.seafile.seadroid;

import java.io.File;

import android.os.Environment;

public class DataManager {
    
    public static String getExternalRootDirectory() {
        if (Environment.getExternalStorageState().equals(Environment.MEDIA_MOUNTED)) {
            File extDir = new File(Environment.getExternalStorageDirectory().getAbsolutePath() + "/Seafile/");
            if (extDir.mkdirs() || extDir.exists()) {
                return extDir.getAbsolutePath();
            } else {
                throw new RuntimeException("Couldn't create external directory");
            }
        } else {
            throw new RuntimeException("External Storage is currently not available");
        }
    }
    
    public static String getExternalTempDirectory() {
        String root = getExternalRootDirectory();
        File tmpDir = new File(root + "/" + "temp");
        if (tmpDir.exists())
            return tmpDir.getAbsolutePath();
        else {
            if (tmpDir.mkdirs() == false)
                throw new RuntimeException("Couldn't create external temp directory");
            else
                return tmpDir.getAbsolutePath();
        }
    }
    
    static public String constructFileName(String path, String oid) {
        String filename = path.substring(path.lastIndexOf("/") + 1);
        String purename = filename.substring(0, filename.lastIndexOf('.'));
        String suffix = filename.substring(filename.lastIndexOf('.') + 1);
        return purename + "-" + oid.substring(0, 8) + "." + suffix;  
    }
    
    static public File getFile(String path, String oid) {
        String p = getExternalRootDirectory() + "/" + constructFileName(path, oid); 
        return new File(p);
    }
    
    static public File getTempFile(String path, String oid) {
        String p = getExternalTempDirectory() + "/" + constructFileName(path, oid); 
        return new File(p);
    }
    
}
