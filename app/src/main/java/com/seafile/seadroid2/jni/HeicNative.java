package com.seafile.seadroid2.jni;

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

    static {
        System.loadLibrary("heicgen");
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
