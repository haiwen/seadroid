package com.seafile.seadroid2.framework.motion_photo;

import android.util.Log;

import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.annotation.Unstable;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.RandomAccessFile;

@Todo
@Unstable
public class MotionPhotoExtractor {

    private static final String TAG = "MotionPhotoExtractor";
    private static final String MOTION_PHOTO_FLAG = "MotionPhoto_Data";

    public static boolean extractVideoFromMotionPhoto(File jpegFile, File outMp4File) {
        if (!jpegFile.exists()) return false;

        try (RandomAccessFile raf = new RandomAccessFile(jpegFile, "r")) {

            long fileLength = raf.length();
            byte[] buffer = new byte[(int) fileLength];
            raf.readFully(buffer);

            // Search for MotionPhoto_Data offset
            int index = indexOf(buffer, MOTION_PHOTO_FLAG.getBytes());
            if (index == -1) {
                Log.w(TAG, "No MotionPhoto_Data marker found in file: " + jpegFile.getName());
                return false;
            }

            // The video offset is encoded after the flag as a 4-byte little-endian int (Google format)
            int offsetIndex = index + MOTION_PHOTO_FLAG.length() + 4; // skip 4-byte length
            if (offsetIndex + 4 > buffer.length) return false;

            int videoOffset = toIntLE(buffer, offsetIndex);
            Log.i(TAG, "Found Motion Photo video offset at: " + videoOffset);

            int videoLength = (int) (fileLength - videoOffset);
            raf.seek(videoOffset);

            byte[] videoBytes = new byte[videoLength];
            raf.readFully(videoBytes);

            // Save as .mp4
            try (FileOutputStream fos = new FileOutputStream(outMp4File)) {
                fos.write(videoBytes);
                Log.i(TAG, "Video saved to: " + outMp4File.getAbsolutePath());
                return true;
            }

        } catch (IOException e) {
            Log.e(TAG, "Failed to extract video: " + e.getMessage(), e);
            return false;
        }
    }

    private static int indexOf(byte[] data, byte[] pattern) {
        for (int i = 0; i < data.length - pattern.length; i++) {
            boolean matched = true;
            for (int j = 0; j < pattern.length; j++) {
                if (data[i + j] != pattern[j]) {
                    matched = false;
                    break;
                }
            }
            if (matched) return i;
        }
        return -1;
    }

    private static int toIntLE(byte[] bytes, int offset) {
        return (bytes[offset] & 0xFF)
                | ((bytes[offset + 1] & 0xFF) << 8)
                | ((bytes[offset + 2] & 0xFF) << 16)
                | ((bytes[offset + 3] & 0xFF) << 24);
    }
}