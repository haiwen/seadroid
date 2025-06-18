package com.seafile.seadroid2.framework.util;

import android.content.ContentResolver;
import android.content.Context;
import android.content.res.AssetFileDescriptor;
import android.database.Cursor;
import android.net.Uri;
import android.provider.DocumentsContract;
import android.provider.MediaStore;
import android.provider.OpenableColumns;
import android.text.TextUtils;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.attribute.BasicFileAttributes;
import java.nio.file.attribute.FileTime;
import java.util.Date;

public class FileUtils {

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
}
