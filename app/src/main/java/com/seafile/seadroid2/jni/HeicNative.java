package com.seafile.seadroid2.jni;

import android.util.Log;

import com.seafile.seadroid2.annotation.Todo;

@Todo
public class HeicNative {

    private static final String TAG = "HeicNative";

    /**
     * Motion Photo type constants.
     */
    public static final int MOTION_PHOTO_TYPE_JPEG = 0;  // JPEG motion photo
    public static final int MOTION_PHOTO_TYPE_HEIC = 1;  // HEIC motion photo
    public static final int MOTION_PHOTO_TYPE_NONE = 2;  // Not a motion photo

    /**
     * 原生库加载失败（如 32 位设备缺对应 ABI 的库）时为 true，
     * 调用方应跳过所有原生调用，避免 UnsatisfiedLinkError。
     */
    private static volatile boolean nativeUnavailable = false;

    static {
        try {
            System.loadLibrary("heicgen");
        } catch (Throwable t) {
            nativeUnavailable = true;
            Log.w(TAG, "failed to load heicgen, motion photo feature disabled", t);
        }
    }

    public static boolean isNativeUnavailable() {
        return nativeUnavailable;
    }

    // ==================== Native methods ====================

    public static native String ConvertJpeg2Heic(String jpegFilePath, String outputPath);


    public static native String ConvertHeic2Jpeg(String heicFilePath, String vendor, String outputPath);

    public static native String ExtractHeicXMP(String inputFilePath);

    /**
     * Stream the MP4 video embedded in the JPEG Motion Photo to a temporary file (avoiding the Java heap to prevent large byte[] OOM).
     *
     * @param inputFilePath  原图路径
     * @param outputFilePath 输出视频文件路径
     * @return 是否提取成功
     */
    public static native boolean ExtractJpegVideoToFile(String inputFilePath, String outputFilePath);

    /**
     * 将 HEIC Motion Photo 中内嵌的 MP4 视频流式写入临时文件（不经过 Java 堆，避免大 byte[] OOM）。
     *
     * @param inputFilePath  原图路径
     * @param outputFilePath 输出视频文件路径
     * @return 是否提取成功
     */
    public static native boolean ExtractHeicVideoToFile(String inputFilePath, String outputFilePath);

    public static boolean extractJpegVideoToFile(String inputFilePath, String outputFilePath) {
        return !nativeUnavailable && ExtractJpegVideoToFile(inputFilePath, outputFilePath);
    }

    public static boolean extractHeicVideoToFile(String inputFilePath, String outputFilePath) {
        return !nativeUnavailable && ExtractHeicVideoToFile(inputFilePath, outputFilePath);
    }

    /**
     * Checks whether the image is a Motion Photo and returns its type.
     * <p>
     * Detection steps:
     * 1. Determine whether the file is JPEG or HEIC from the file header
     * 2. Check the GCamera:MotionPhoto flag in XMP metadata
     * 3. Search for embedded MP4 video data (ftyp/mpvd)
     *
     * @param inputFilePath image file path
     * @return Motion Photo type:
     * - {@link #MOTION_PHOTO_TYPE_JPEG} (0): JPEG motion photo
     * - {@link #MOTION_PHOTO_TYPE_HEIC} (1): HEIC motion photo
     * - {@link #MOTION_PHOTO_TYPE_NONE} (2): not a motion photo
     */
    public static native int CheckMotionPhotoType(String inputFilePath);

}
