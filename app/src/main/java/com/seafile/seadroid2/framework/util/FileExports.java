package com.seafile.seadroid2.framework.util;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.database.Cursor;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.os.ParcelFileDescriptor;
import android.provider.MediaStore;

import androidx.annotation.RequiresApi;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.framework.util.SLogs;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class FileExports {
    @RequiresApi(api = Build.VERSION_CODES.Q)
    public static void exportFileAndroid10AndAbove(String fileName, String mimeType, ContentResolver contentResolver, File file) throws IOException {
        // First, try to find if the file already exists in MediaStore
        Uri existingUri = findExistingFileInDownloads(contentResolver, fileName);

        Uri uri;
        ContentValues cv = new ContentValues();
        cv.put(MediaStore.MediaColumns.DISPLAY_NAME, fileName);
        cv.put(MediaStore.MediaColumns.MIME_TYPE, mimeType);
        cv.put(MediaStore.MediaColumns.RELATIVE_PATH, Environment.DIRECTORY_DOWNLOADS);

        if (existingUri != null) {
            // File exists, update it directly
            uri = existingUri;
        } else {
            // File doesn't exist, insert new
            // On Android 10+, MediaStore will handle duplicates by returning null
            // We catch this case and fall back to the manual copy method
            try {
                uri = contentResolver.insert(MediaStore.Downloads.EXTERNAL_CONTENT_URI, cv);
            } catch (Exception e) {
                SLogs.e("Failed to insert file to MediaStore", e);
                uri = null;
            }
        }

        if (uri != null) {
            ParcelFileDescriptor descriptor = null;
            FileOutputStream fos = null;
            FileInputStream fis = null;

            try {
                descriptor = contentResolver.openFileDescriptor(uri, "w");
                if (descriptor != null) {
                    fos = new FileOutputStream(descriptor.getFileDescriptor());
                    fis = new FileInputStream(file);

                    byte[] buffer = new byte[1024];
                    int len;
                    while ((len = fis.read(buffer)) != -1) {
                        fos.write(buffer, 0, len);
                    }
                }

            } finally {
                if (fos != null) {
                    fos.close();
                }
                if (fis != null) {
                    fis.close();
                }
                if (descriptor != null) {
                    descriptor.close();
                }
            }

        } else {
            // Fallback: use traditional file copy method
            File downloadFolder = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS);
            File dest = new File(downloadFolder.getPath() + "/" + fileName);
            if (!dest.exists()) {
                FileUtils.copy(file, dest);
            }
        }
    }

    /**
     * Find if a file with the given name already exists in Downloads
     *
     * @param contentResolver ContentResolver
     * @param fileName File name to search for
     * @return Uri of existing file or null if not found
     */
    @RequiresApi(api = Build.VERSION_CODES.Q)
    private static Uri findExistingFileInDownloads(ContentResolver contentResolver, String fileName) {
        try {
            String selection = MediaStore.MediaColumns.DISPLAY_NAME + " = ?";
            String[] selectionArgs = new String[]{fileName};

            try (Cursor cursor = contentResolver.query(
                    MediaStore.Downloads.EXTERNAL_CONTENT_URI,
                    new String[]{MediaStore.MediaColumns._ID},
                    selection,
                    selectionArgs,
                    null)) {
                if (cursor != null && cursor.moveToFirst()) {
                    int idColumn = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns._ID);
                    long id = cursor.getLong(idColumn);
                    return Uri.withAppendedPath(MediaStore.Downloads.EXTERNAL_CONTENT_URI, String.valueOf(id));
                }
            }
        } catch (Exception e) {
            SLogs.e("Error finding existing file in Downloads", e);
        }
        return null;
    }
}
