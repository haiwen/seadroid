package com.seafile.seadroid2.framework.service;

import android.content.ContentResolver;
import android.content.Context;
import android.database.Cursor;
import android.net.Uri;
import android.provider.OpenableColumns;

import com.seafile.seadroid2.framework.util.SafeLogs;

import java.io.InputStream;

public class FileUploadUtils {
    private static final long FALLBACK_SIZE_THRESHOLD = 100 * 1024 * 1024L; // 100MB
    private static final String TAG = "FileUploadUtils";

    /**
     * 1、get File Size from ContentResolver.OpenableColumns.SIZE
     * 2、if uri is trusted, return size
     * 3、check file size by stream if size is small, and return size
     * 5、return size
     */
    public static long resolveSize(Context context, Uri uri) {
        ContentResolver resolver = context.getContentResolver();
        long size = -1L;

        // 获取 OpenableColumns 信息
        try (Cursor cursor = resolver.query(uri,
                new String[]{OpenableColumns.SIZE},
                null,
                null,
                null)
        ) {
            if (cursor != null && cursor.moveToFirst()) {
                int sizeIdx = cursor.getColumnIndex(OpenableColumns.SIZE);
                if (sizeIdx != -1) {
                    size = cursor.getLong(sizeIdx);
                }
            }
        } catch (Exception ignored) {
        }

        if (isTrustedUri(uri)) {
            SafeLogs.d(TAG, "uri is trusted. return size = " + size);
            return size;
        }


        if (size <= FALLBACK_SIZE_THRESHOLD) {
            long actualSize = getSizeByStream(resolver, uri);
            SafeLogs.d(TAG, "uri is not trusted. but size is small. return checked size = " + actualSize);
            if (actualSize > 0) {
                size = actualSize;
            }

            return size;
        }

        SafeLogs.d(TAG, "uri is not trusted. but size is large. return size = " + size);

        return size;
    }

    /**
     * 是否为受信任的 URI（如 file://、MediaStore、FileProvider）。
     */
    private static boolean isTrustedUri(Uri uri) {
        String scheme = uri.getScheme();
        if ("file".equalsIgnoreCase(scheme)) return true;
        if ("content".equalsIgnoreCase(scheme)) {
            String auth = uri.getAuthority();
            return auth != null && (
                    auth.startsWith("media") ||             // MediaStore
                            auth.endsWith(".fileprovider") ||       // 自家 FileProvider
                            auth.contains("documents")              // 通用 SAF 类型
            );
        }
        return false;
    }

    /**
     * fallback：通过 InputStream 计算大小。
     */
    private static long getSizeByStream(ContentResolver resolver, Uri uri) {
        try (InputStream in = resolver.openInputStream(uri)) {
            if (in == null) return -1;
            byte[] buffer = new byte[8192];
            long total = 0;
            int read;
            while ((read = in.read(buffer)) != -1) {
                total += read;
                if (total > FALLBACK_SIZE_THRESHOLD) break; // 防止过大文件阻塞
            }
            return total;
        } catch (Exception e) {
            return -1;
        }
    }
}
