package com.seafile.seadroid2.framework.util;

import android.content.ContentResolver;
import android.database.Cursor;
import android.net.Uri;
import android.provider.MediaStore;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class ContentResolvers {

    public static String getFileNameFromUri(ContentResolver contentResolver, Uri uri) {
        String fileName = null;
        String[] projection = {MediaStore.MediaColumns.DISPLAY_NAME};
        try (Cursor cursor = contentResolver.query(uri, projection, null, null, null)) {
            if (cursor != null && cursor.moveToFirst()) {
                int index = cursor.getColumnIndexOrThrow(MediaStore.MediaColumns.DISPLAY_NAME);
                fileName = cursor.getString(index);
            }
        }

        return fileName;
    }

    // 获取文件内容的方法
    public static byte[] getFileContentFromUri(ContentResolver contentResolver, Uri uri) {
        byte[] fileContent = null;
        InputStream inputStream = null;
        try {
            inputStream = contentResolver.openInputStream(uri);
            if (inputStream != null) {
                ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
                byte[] buffer = new byte[4096];
                int bytesRead;
                while ((bytesRead = inputStream.read(buffer)) != -1) {
                    outputStream.write(buffer, 0, bytesRead);
                }
                fileContent = outputStream.toByteArray();
            }
        } catch (IOException e) {
            e.printStackTrace();
        } finally {
            if (inputStream != null) {
                try {
                    inputStream.close();
                } catch (IOException e) {
                    e.printStackTrace();
                }
            }
        }
        return fileContent;
    }
}
