package com.seafile.seadroid2.framework.util;

import android.content.Context;
import android.os.Build;
import android.os.Environment;
import android.os.storage.StorageManager;
import android.os.storage.StorageVolume;
import android.text.TextUtils;

import androidx.core.os.EnvironmentCompat;

import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.model.StorageInfo;
import com.seafile.seadroid2.ui.selector.folder_selector.StringTools;

import java.io.File;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class FileTools {
    public static File getFileByPath(String path) {
        return TextUtils.isEmpty(path) ? null : new File(path);
    }

    public static boolean rename(String filePath, String newName) {
        return rename(getFileByPath(filePath), newName);
    }

    public static boolean rename(File file, String newName) {
        if (file == null) {
            return false;
        } else if (!file.exists()) {
            return false;
        } else if (TextUtils.isEmpty(newName)) {
            return false;
        } else if (newName.equals(file.getName())) {
            return true;
        } else {
            File newFile = new File(file.getParent() + File.separator + newName);
            return !newFile.exists() && file.renameTo(newFile);
        }
    }

    public static int[] getChildrenNumber(File file) {
        boolean isSkipHiddenFile = FolderBackupSharePreferenceHelper.isFolderBackupSkipHiddenFiles();

        File[] files = file.listFiles();
        int[] numbers = new int[]{0, 0};
        if (files == null) {
            return numbers;
        }

        for (File value : files) {
            if (isSkipHiddenFile && value.isHidden()) {
                continue;
            }

            if (value.isFile()) {
                numbers[0]++;
            } else {
                numbers[1]++;
            }
        }
        return numbers;
    }

    public static boolean isDir(String dirPath) {
        return isDir(getFileByPath(dirPath));
    }

    public static boolean isDir(File file) {
        return file != null && file.exists() && file.isDirectory();
    }

    public static boolean isFile(String filePath) {
        return isFile(getFileByPath(filePath));
    }

    public static boolean isFile(File file) {
        return file != null && file.exists() && file.isFile();
    }

    public static String getDirName(String filePath) {
        if (TextUtils.isEmpty(filePath)) {
            return "";
        } else {
            int lastSep1 = filePath.lastIndexOf(File.separator);
            if (lastSep1 == -1) {
                return "";
            } else {
                int lastSep2 = filePath.substring(0, lastSep1).lastIndexOf(File.separator);
                return filePath.substring(lastSep2 + 1, lastSep1);
            }
        }
    }

    public static String getFileName(String filePath) {
        if (TextUtils.isEmpty(filePath)) {
            return "";
        } else {
            int lastSep = filePath.lastIndexOf(File.separator);
            return lastSep == -1 ? filePath : filePath.substring(lastSep + 1);
        }
    }

    public static String getFileNameNoExtension(String filePath) {
        if (TextUtils.isEmpty(filePath)) {
            return "";
        } else {
            int lastPoi = filePath.lastIndexOf(46);
            int lastSep = filePath.lastIndexOf(File.separator);
            if (lastSep == -1) {
                return lastPoi == -1 ? filePath : filePath.substring(0, lastPoi);
            } else {
                return lastPoi != -1 && lastSep <= lastPoi ? filePath.substring(lastSep + 1, lastPoi) : filePath.substring(lastSep + 1);
            }
        }
    }

    public static String getFileExtension(String filePath) {
        if (TextUtils.isEmpty(filePath) || !filePath.contains(".")) {
            return "";
        } else {
            int lastPoi = filePath.lastIndexOf(46);
            int lastSep = filePath.lastIndexOf(File.separator);
            return lastPoi != -1 && lastSep < lastPoi ? filePath.substring(lastPoi + 1) : "";
        }
    }


    public static long getFileLastModified(File file) {
        return file == null ? -1L : file.lastModified();
    }

    public static Long getSimpleSize(File file) {
        return StringTools.getOnlyNumber(getSize(file));
    }

    public static String getSize(File file) {
        if (file == null) {
            return "";
        } else {
            return file.isDirectory() ? getDirSize(file) : getFileSize(file);
        }
    }

    private static String getDirSize(File dir) {
        long len = getDirLength(dir);
        return len == -1L ? "" : byte2FitMemorySize(len, 3);
    }

    private static String getFileSize(File file) {
        long len = getFileLength(file);
        return len == -1L ? "" : byte2FitMemorySize(len, 3);
    }

    private static long getFileLength(File file) {
        return !isFile(file) ? -1L : file.length();
    }

    private static long getDirLength(File dir) {
        if (!isDir(dir)) {
            return 0L;
        } else {
            long len = 0L;
            File[] files = dir.listFiles();
            if (files != null && files.length > 0) {
                File[] var4 = files;
                int var5 = files.length;

                for (int var6 = 0; var6 < var5; ++var6) {
                    File file = var4[var6];
                    if (file.isDirectory()) {
                        len += getDirLength(file);
                    } else {
                        len += file.length();
                    }
                }
            }
            return len;
        }
    }

    public static String byte2FitMemorySize(long byteSize, int precision) {
        if (precision < 0) {
            throw new IllegalArgumentException("precision shouldn't be less than zero!");
        } else if (byteSize < 0L) {
            throw new IllegalArgumentException("byteSize shouldn't be less than zero!");
        } else if (byteSize < 1024L) {
            return String.format("%." + precision + "fB", (double) byteSize);
        } else if (byteSize < 1048576L) {
            return String.format("%." + precision + "fKB", (double) byteSize / 1024.0D);
        } else {
            return byteSize < 1073741824L ? String.format("%." + precision + "fMB", (double) byteSize / 1048576.0D) : String.format("%." + precision + "fGB", (double) byteSize / 1.073741824E9D);
        }
    }

    public static List<StorageInfo> getAllStorageInfos(Context context) {
        List<StorageInfo> result = new ArrayList<>();
        StorageManager sm = (StorageManager) context.getSystemService(Context.STORAGE_SERVICE);
        HashSet<String> pathSet = new HashSet<>();

        List<StorageVolume> volumes = sm.getStorageVolumes();
        for (StorageVolume volume : volumes) {
            try {
                File dir;
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                    dir = volume.getDirectory();
                } else {
                    // Before API 30 way to get a volume path
                    Method getPathMethod = StorageVolume.class.getDeclaredMethod("getPathFile");
                    dir = (File) getPathMethod.invoke(volume);
                }

                String state = volume.getState();
                boolean isAvailable = dir != null && dir.exists() &&
                        (Environment.MEDIA_MOUNTED.equals(state) || Environment.MEDIA_MOUNTED_READ_ONLY.equals(state));

                String path = dir == null ? null : dir.getAbsolutePath();
                boolean isRemovable = volume.isRemovable();
                boolean isPrimary = volume.isPrimary();
                String label = volume.getDescription(context);

                if (pathSet.add(path)) {
                    StorageInfo storageInfo = new StorageInfo();
                    storageInfo.setPath(path);
                    storageInfo.setLabel(label);
                    storageInfo.setRemovable(isRemovable);
                    storageInfo.setPrimary(isPrimary);
                    storageInfo.setAvailable(isAvailable);
                    result.add(storageInfo);
                }
            } catch (Exception e) {
                // Will fallback to volumePath
            }
        }

        return result;
    }


}
