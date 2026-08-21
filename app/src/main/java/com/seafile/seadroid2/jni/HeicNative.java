package com.seafile.seadroid2.jni;

import android.util.Log;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.framework.util.Toasts;

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

    /**
     * Returns the libheif version.
     */
    public static native String GetLibVersion();

    /**
     * Generates a still HEIC image.
     *
     * @param primaryImage JPEG data for the primary image
     * @param outputPath output file path
     * @return whether the operation succeeded
     */
    public static native boolean GenStillHeicSeq(byte[] primaryImage, String outputPath);

    public static native String ConvertJpeg2Heic(String jpegFilePath, String outputPath);

    public static String convertJpegMotionPhotoTo(String heicFilePath, String outputPath) {
        if (nativeUnavailable) {
            return null;
        }

        if (FileUtils.isFileExists(heicFilePath)) {
            return ConvertJpeg2Heic(heicFilePath, outputPath);
        } else {
            Toasts.show(R.string.not_available);
        }
        return null;
    }

    public static native String ConvertHeic2Jpeg(String heicFilePath, String vendor, String outputPath);


    /**
     * Generates a Google Motion Photo HEIC file.
     *
     * @param primaryImage JPEG data for the primary image
     * @param mp4Video MP4 video data
     * @param outputPath output file path
     * @return result string (success:... or error:...)
     */
    public static native String GenHeicMotionPhoto(byte[] primaryImage, byte[] hdrDatas, byte[] exifDatas, byte[] xmpBytes, byte[] mp4Video, long presentationTimestampUs, String outputPath);

    /**
     * Extracts MP4 video data from a HEIC Motion Photo file.
     * This applies to HEIC motion photos stored in mpvd box format.
     *
     * @param inputFilePath HEIC Motion Photo file path
     * @return MP4 video bytes, or null on failure
     */
    public static native byte[] ExtractHeicVideo(String inputFilePath);

    public static native String ExtractHeicXMP(String inputFilePath);

    /**
     * Extracts MP4 video data from a JPEG Motion Photo file.
     * This applies to JPEG motion photos captured by Google Camera.
     * <p>
     * JPEG Motion Photo structure: JPEG image plus appended MP4 video.
     *
     * @param inputFilePath JPEG Motion Photo file path
     * @return MP4 video bytes, or null on failure
     */
    public static native byte[] ExtractJpegVideo(String inputFilePath);

    /**
     * 将 JPEG Motion Photo 中内嵌的 MP4 视频流式写入临时文件（不经过 Java 堆，避免大 byte[] OOM）。
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

    /**
     * Checks whether the image is a Motion Photo.
     *
     * @param inputFilePath image file path
     * @return whether the file is a motion photo
     */
    public static boolean isMotionPhoto(String inputFilePath) {
        if (nativeUnavailable) {
            return false;
        }
        int type = CheckMotionPhotoType(inputFilePath);
        return type == MOTION_PHOTO_TYPE_JPEG || type == MOTION_PHOTO_TYPE_HEIC;
    }

    /**
     * Automatically extracts video data from a Motion Photo by file path.
     *
     * @param inputFilePath image file path
     * @return MP4 video bytes, or null on failure or if not a motion photo
     */
    public static byte[] extractMotionPhotoVideo(String inputFilePath) {
        if (nativeUnavailable) {
            return null;
        }

        int type = CheckMotionPhotoType(inputFilePath);
        switch (type) {
            case MOTION_PHOTO_TYPE_JPEG:
                return ExtractJpegVideo(inputFilePath);
            case MOTION_PHOTO_TYPE_HEIC:
                return ExtractHeicVideo(inputFilePath);
            default:
                return null;
        }
    }
}
