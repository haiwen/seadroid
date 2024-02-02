package com.seafile.seadroid2.util;

import android.content.ContentResolver;
import android.content.ContentValues;
import android.net.Uri;
import android.os.Build;
import android.os.Environment;
import android.os.ParcelFileDescriptor;
import android.provider.MediaStore;

import androidx.annotation.RequiresApi;

import com.blankj.utilcode.util.FileUtils;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

public class FileExports {
    @RequiresApi(api = Build.VERSION_CODES.Q)
    public static void exportFileAndroid10AndAbove(String fileName, String mimeType, ContentResolver contentResolver, File file) throws IOException {
        ContentValues cv = new ContentValues();
        cv.put(MediaStore.MediaColumns.DISPLAY_NAME, fileName);
        cv.put(MediaStore.MediaColumns.MIME_TYPE, mimeType);
        cv.put(MediaStore.MediaColumns.RELATIVE_PATH, Environment.DIRECTORY_DOWNLOADS);

        Uri uri = contentResolver.insert(MediaStore.Downloads.EXTERNAL_CONTENT_URI, cv);
        if (uri != null) {
            ParcelFileDescriptor descriptor = null;
            FileOutputStream fos = null;
            FileInputStream fis = null;
            //TODO
            try {
                descriptor = contentResolver.openFileDescriptor(uri, "w");
                if (descriptor != null) {
                    fos = new FileOutputStream(descriptor.getFileDescriptor());
                    fis = new FileInputStream(file);
                    copyStream(fis, fos);
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
            File downloadFolder = Environment.getExternalStoragePublicDirectory(Environment.DIRECTORY_DOWNLOADS);
            //
            File dest = new File(downloadFolder.getPath() + "/" + fileName);
            if (!dest.exists()) {
                FileUtils.copy(file, dest);
            }
        }

    }

    private static void copyStream(InputStream inputStream, FileOutputStream outputStream) throws IOException {
        byte[] buffer = new byte[1024];
        int len;
        while ((len = inputStream.read(buffer)) != -1) {
            outputStream.write(buffer, 0, len);
        }
    }

}
