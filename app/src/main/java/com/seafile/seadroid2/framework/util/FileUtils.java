package com.seafile.seadroid2.framework.util;

import android.content.ContentResolver;
import android.content.Context;
import android.content.res.AssetFileDescriptor;
import android.database.Cursor;
import android.media.MediaMetadataRetriever;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.os.storage.StorageManager;
import android.os.storage.StorageVolume;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import android.provider.OpenableColumns;
import android.text.TextUtils;

import com.seafile.seadroid2.framework.datastore.sp_livedata.FolderBackupSharePreferenceHelper;
import com.seafile.seadroid2.framework.model.StorageInfo;
import com.seafile.seadroid2.ui.selector.folder_selector.StringTools;

import org.apache.commons.io.FilenameUtils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.nio.file.attribute.PosixFilePermission;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class FileUtils {

    private static final HashMap<String, String> needUpdateFileMime = new HashMap<>();

    static {
        //{fileType，    MIME}
        needUpdateFileMime.put(".doc", "application/msword");
        needUpdateFileMime.put(".dot", "application/msword");
        needUpdateFileMime.put(".docx", "application/vnd.openxmlformats-officedocument.wordprocessingml.document");
        needUpdateFileMime.put(".dotx", "application/vnd.openxmlformats-officedocument.wordprocessingml.template");
        needUpdateFileMime.put(".docm", "application/vnd.ms-word.document.macroEnabled.12");
        needUpdateFileMime.put(".dotm", "application/vnd.ms-word.template.macroEnabled.12");
        needUpdateFileMime.put(".xls", "application/vnd.ms-excel");
        needUpdateFileMime.put(".xlt", "application/vnd.ms-excel");
        needUpdateFileMime.put(".xla", "application/vnd.ms-excel");
        needUpdateFileMime.put(".xlsx", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet");
        needUpdateFileMime.put(".xltx", "application/vnd.openxmlformats-officedocument.spreadsheetml.template");
        needUpdateFileMime.put(".xlsm", "application/vnd.ms-excel.sheet.macroEnabled.12");
        needUpdateFileMime.put(".xltm", "application/vnd.ms-excel.template.macroEnabled.12");
        needUpdateFileMime.put(".xlam", "application/vnd.ms-excel.addin.macroEnabled.12");
        needUpdateFileMime.put(".xlsb", "application/vnd.ms-excel.sheet.binary.macroEnabled.12");
        needUpdateFileMime.put(".ppt", "application/vnd.ms-powerpoint");
        needUpdateFileMime.put(".pot", "application/vnd.ms-powerpoint");
        needUpdateFileMime.put(".pps", "application/vnd.ms-powerpoint");
        needUpdateFileMime.put(".ppa", "application/vnd.ms-powerpoint");
        needUpdateFileMime.put(".pptx", "application/vnd.openxmlformats-officedocument.presentationml.presentation");
        needUpdateFileMime.put(".potx", "application/vnd.openxmlformats-officedocument.presentationml.template");
        needUpdateFileMime.put(".ppsx", "application/vnd.openxmlformats-officedocument.presentationml.slideshow");
        needUpdateFileMime.put(".ppam", "application/vnd.ms-powerpoint.addin.macroEnabled.12");
        needUpdateFileMime.put(".pptm", "application/vnd.ms-powerpoint.presentation.macroEnabled.12");
        needUpdateFileMime.put(".ppsm", "application/vnd.ms-powerpoint.slideshow.macroEnabled.12");
        needUpdateFileMime.put(".wps", "application/vnd.ms-works");
    }


    public static boolean isOfficeOrTextFile(String mime) {
        return needUpdateFileMime.containsValue(mime);
    }

    public static String getMimeType(File file) {
        String mime = "text/plain";
        String filePath = file.getAbsolutePath();
        MediaMetadataRetriever mmr = new MediaMetadataRetriever();
        try {
            mmr.setDataSource(filePath);
            mime = mmr.extractMetadata(MediaMetadataRetriever.METADATA_KEY_MIMETYPE);
        } catch (RuntimeException e) {
            return mime;
        }
        return mime;
    }

    public static boolean isAvailable(File f) {
        if (f == null) {
            return false;
        }

        if (!f.exists()) {
            return false;
        }

        if (!f.canWrite()) {
            return false;
        }

        Path p = f.toPath();
        if (!Files.isWritable(p)) {
            return false;
        }

        try {
            Set<PosixFilePermission> permissions = Files.getPosixFilePermissions(f.toPath());
            return permissions.contains(PosixFilePermission.OWNER_WRITE);
        } catch (IOException e) {
            SLogs.e(e);
            return false;
        }
    }

    /**
     * build valid file path and name.
     *
     * <p>
     * " * < ? / : > convert to _
     * <p/>
     * <p>
     * for example:
     * <p>
     * /a'"'a/b'*'b/c'<'c/d'>'d/e'?'e/f':'f/g'/'g/
     * </p>
     * convert to ->
     * <p>
     * /a_a/b_b/c_c/d_d/e_e/f_f/g_g/
     * </p>
     * and so on.
     */
    public static String buildValidFilePathName(String fileName) {
        fileName = buildValidFatFilename(fileName);
        fileName = buildValidExtFilename(fileName);
        return fileName;
    }


    /**
     * Check if given filename is valid for an ext4 filesystem.
     */
    public static boolean isValidExtFilename(String name) {
        return (name != null) && name.equals(buildValidExtFilename(name));
    }

    /**
     * Mutate the given filename to make it valid for an ext4 filesystem,
     * replacing any invalid characters with "_".
     */
    public static String buildValidExtFilename(String name) {
        if (TextUtils.isEmpty(name) || ".".equals(name) || "..".equals(name)) {
            return "(invalid)";
        }
        final StringBuilder res = new StringBuilder(name.length());
        for (int i = 0; i < name.length(); i++) {
            final char c = name.charAt(i);
            if (isValidExtFilenameChar(c)) {
                res.append(c);
            } else {
                res.append('_');
            }
        }
        trimFilename(res, 255);
        return res.toString();
    }

    private static boolean isValidExtFilenameChar(char c) {
        switch (c) {
            case '\0':
                //case '/':
                return false;
            default:
                return true;
        }
    }

    /**
     * Check if given filename is valid for a FAT filesystem.
     */
    public static boolean isValidFatFilename(String name) {
        return (name != null) && name.equals(buildValidFatFilename(name));
    }

    /**
     * Mutate the given filename to make it valid for a FAT filesystem,
     * replacing any invalid characters with "_".
     */
    public static String buildValidFatFilename(String name) {
        if (TextUtils.isEmpty(name) || ".".equals(name) || "..".equals(name)) {
            return "(invalid)";
        }
        final StringBuilder res = new StringBuilder(name.length());
        for (int i = 0; i < name.length(); i++) {
            final char c = name.charAt(i);
            if (isValidFatFilenameChar(c)) {
                res.append(c);
            } else {
                res.append('_');
            }
        }
        // Even though vfat allows 255 UCS-2 chars, we might eventually write to
        // ext4 through a FUSE layer, so use that limit.
        trimFilename(res, 255);
        return res.toString();
    }

    private static boolean isValidFatFilenameChar(char c) {
        if ((0x00 <= c && c <= 0x1f)) {
            return false;
        }
        switch (c) {
            case '"':
            case '*':
                //case '/':
            case ':':
            case '<':
            case '>':
            case '?':
            case '\\':
            case '|':
            case 0x7F:
                return false;
            default:
                return true;
        }
    }

    public static String trimFilename(String str, int maxBytes) {
        final StringBuilder res = new StringBuilder(str);
        trimFilename(res, maxBytes);
        return res.toString();
    }

    private static void trimFilename(StringBuilder res, int maxBytes) {
        byte[] raw = res.toString().getBytes(StandardCharsets.UTF_8);
        if (raw.length > maxBytes) {
            maxBytes -= 3;
            while (raw.length > maxBytes) {
                res.deleteCharAt(res.length() / 2);
                raw = res.toString().getBytes(StandardCharsets.UTF_8);
            }
            res.insert(res.length() / 2, "...");
        }
    }

    public static long getEstimationFileSize(Context context, Uri uri) {
        long size = -1;
        Cursor cursor = context.getContentResolver().query(uri, null, null, null, null);
        if (cursor != null) {
            try {
                if (cursor.moveToFirst()) {
                    int sizeIndex = cursor.getColumnIndex(OpenableColumns.SIZE);
                    if (!cursor.isNull(sizeIndex)) {
                        size = cursor.getLong(sizeIndex);
                    }
                }
            } finally {
                cursor.close();
            }
        }
        return size;
    }


    public static boolean isUriHasPermission(Context context, Uri uri) {
        try {
            ContentResolver resolver = context.getContentResolver();
            AssetFileDescriptor afd = resolver.openAssetFileDescriptor(uri, "r");
            if (afd != null) {
                afd.close();
                return true;
            }
            return true;
        } catch (Exception e) {
            SLogs.e("check URI permission failed: " + e.getMessage());
        }
        return false;
    }

    public static long getCreatedTimeFromPath(Context context, File file) {
        try {
            Path path = file.toPath();
            BasicFileAttributes attrs = Files.readAttributes(path, BasicFileAttributes.class);
            FileTime creationTime = attrs.creationTime();
            long timestamp = creationTime.toMillis();
            SLogs.d("FileUtils", "read file create timestamp success.", new Date(timestamp).toString());
            return timestamp;
        } catch (IOException e) {
            SLogs.e("FileUtils", "read file create timestamp failed.", e.getMessage());
            return -1;
        }

    }

    public static long getCreatedTimeFromUri(Context context, Uri uri) {
        ContentResolver resolver = context.getContentResolver();
        Cursor cursor = null;

        try {
            // 优先使用 MediaStore.DATE_ADDED + LAST_MODIFIED
            String[] projection = {
                    MediaStore.MediaColumns.DATE_ADDED,
                    MediaStore.MediaColumns.DATE_MODIFIED,
                    DocumentsContract.Document.COLUMN_LAST_MODIFIED
            };

            cursor = resolver.query(uri, projection, null, null, null);
            if (cursor != null && cursor.moveToFirst()) {
                //sort order: DATE_ADDED > COLUMN_LAST_MODIFIED > DATE_MODIFIED
                int dateAddedIndex = cursor.getColumnIndex(MediaStore.MediaColumns.DATE_ADDED);
                int lastModifiedIndex = cursor.getColumnIndex(MediaStore.MediaColumns.DATE_MODIFIED);
                int docLastModifiedIndex = cursor.getColumnIndex(DocumentsContract.Document.COLUMN_LAST_MODIFIED);

                long created = -1;
                if (dateAddedIndex != -1 && !cursor.isNull(dateAddedIndex)) {
                    created = cursor.getLong(dateAddedIndex) * 1000L;
                    SLogs.d("FileUtils", "read uri dateAdded success", created + "");
                }

                if (created <= 0 && docLastModifiedIndex != -1 && !cursor.isNull(docLastModifiedIndex)) {
                    created = cursor.getLong(docLastModifiedIndex);
                    SLogs.d("FileUtils", "read uri docLastModified success", created + "");
                }

                if (created <= 0 && lastModifiedIndex != -1 && !cursor.isNull(lastModifiedIndex)) {
                    created = cursor.getLong(lastModifiedIndex) * 1000L;
                    SLogs.d("FileUtils", "read uri lastModified success", created + "");
                }


                return created > 0 ? created : -1;
            }
        } catch (Exception e) {
            SLogs.e("FileUtils", "read uri create timestamp failed", e.getMessage());
        } finally {
            if (cursor != null) cursor.close();
        }

        return -1;
    }

    /**
     * 获取 Uri 文件的实际大小（字节数）。
     * 优先使用 OpenableColumns.SIZE，如果不可用则通过流读取计算。
     *
     * @param context 上下文
     * @param uri     文件 Uri
     * @return 文件大小，如果获取失败返回 -1
     */
    public static long getFileSize(Context context, Uri uri) {
        if (context == null || uri == null) {
            return -1;
        }

        long size = getSizeFromOpenableColumns(context, uri);
        if (size > 0) {
            return size;
        }
        // fallback: 通过流读取计算
        return getSizeFromStream(context, uri);
    }

    /**
     * 优先通过 OpenableColumns.SIZE 获取文件大小
     */
    private static long getSizeFromOpenableColumns(Context context, Uri uri) {
        Cursor cursor = null;
        try {
            cursor = context.getContentResolver().query(uri, null, null, null, null);
            if (cursor != null) {
                int sizeIndex = cursor.getColumnIndex(OpenableColumns.SIZE);
                if (sizeIndex != -1 && cursor.moveToFirst()) {
                    long size = cursor.getLong(sizeIndex);
                    if (size > 0) {
                        return size;
                    }
                }
            }
        } catch (Exception ignore) {
            // 忽略异常，继续使用流式计算
        } finally {
            if (cursor != null) {
                cursor.close();
            }
        }
        return -1;
    }

    /**
     * 通过流读取计算文件大小
     */
    private static long getSizeFromStream(Context context, Uri uri) {
        long totalBytes = 0;
        InputStream inputStream = null;
        try {
            inputStream = context.getContentResolver().openInputStream(uri);
            if (inputStream == null) return -1;

            byte[] buffer = new byte[8192]; // 8KB buffer
            int bytesRead;
            while ((bytesRead = inputStream.read(buffer)) != -1) {
                totalBytes += bytesRead;
            }
        } catch (IOException ignore) {
            // 忽略异常，返回 -1
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException ignore) {
                }
            }
        }
        return totalBytes;
    }

    public static File getFileByPath(String path) {
        return com.blankj.utilcode.util.FileUtils.getFileByPath(path);
    }

    public static boolean rename(String filePath, String newName) {
        return rename(getFileByPath(filePath), newName);
    }

    public static boolean rename(File file, String newName) {
        return com.blankj.utilcode.util.FileUtils.rename(file, newName);

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

    public static String getName(String f) {
        f = FilenameUtils.getName(f);
        if (TextUtils.isEmpty(f)) {
            return "";
        }
        return f;
    }

    public static String getBaseName(String f) {
        f = FilenameUtils.getBaseName(f);
        if (TextUtils.isEmpty(f)) {
            return "";
        }
        return f;
    }

    public static String getExtension(String f) {
        f = FilenameUtils.getExtension(f);
        if (TextUtils.isEmpty(f)) {
            return "";
        }
        return f;
    }


    public static long getFileLastModified(File file) {
        return com.blankj.utilcode.util.FileUtils.getFileLastModified(file);
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
