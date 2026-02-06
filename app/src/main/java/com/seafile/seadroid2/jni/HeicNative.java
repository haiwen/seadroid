package com.seafile.seadroid2.jni;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.framework.util.Toasts;

@Todo
public class HeicNative {

    private static final String TAG = "HeicNative";

    /**
     * Motion Photo 类型枚举值
     */
    public static final int MOTION_PHOTO_TYPE_JPEG = 0;  // JPEG 格式的动态照片
    public static final int MOTION_PHOTO_TYPE_HEIC = 1;  // HEIC 格式的动态照片
    public static final int MOTION_PHOTO_TYPE_NONE = 2;  // 非动态照片

    static {
        System.loadLibrary("heicgen");
    }

    // ==================== Native methods ====================

    /**
     * 获取 libheif 版本号
     */
    public static native String GetLibVersion();

    /**
     * 生成静态 HEIC 图片
     *
     * @param primaryImage 主图 JPEG 数据
     * @param outputPath   输出文件路径
     * @return 是否成功
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
     * 生成 Google Motion Photo 格式的 HEIC 动态照片
     *
     * @param primaryImage 主图 JPEG 数据
     * @param mp4Video     视频 MP4 数据
     * @param outputPath   输出文件路径
     * @return 结果字符串 (success:... 或 error:...)
     */
    public static native String GenHeicMotionPhoto(byte[] primaryImage, byte[] hdrDatas, byte[] exifDatas,byte[] xmpBytes, byte[] mp4Video,long presentationTimestampUs, String outputPath);

    /**
     * 从 HEIC Motion Photo 文件中提取 MP4 视频数据
     * (适用于 mpvd box 格式的 HEIC 动态照片)
     *
     * @param inputFilePath HEIC Motion Photo 文件路径
     * @return MP4 视频数据的字节数组，失败返回 null
     */
    public static native byte[] ExtractHeicVideo(String inputFilePath);

    public static native String ExtractHeicXMP(String inputFilePath);

    /**
     * 从 JPEG Motion Photo 文件中提取 MP4 视频数据
     * (适用于 Google 相机拍摄的 JPEG 格式动态照片)
     * <p>
     * JPEG Motion Photo 结构：JPEG 图片 + MP4 视频直接追加
     *
     * @param inputFilePath JPEG Motion Photo 文件路径
     * @return MP4 视频数据的字节数组，失败返回 null
     */
    public static native byte[] ExtractJpegVideo(String inputFilePath);

    /**
     * 检查图片是否为 Motion Photo，并返回类型
     * <p>
     * 检测逻辑：
     * 1. 通过文件头判断是 JPEG 还是 HEIC 格式
     * 2. 检查 XMP 元数据中的 GCamera:MotionPhoto 标识
     * 3. 搜索嵌入的 MP4 视频数据 (ftyp/mpvd)
     *
     * @param inputFilePath 图片文件路径
     * @return Motion Photo 类型:
     * - {@link #MOTION_PHOTO_TYPE_JPEG} (0): JPEG 格式的动态照片
     * - {@link #MOTION_PHOTO_TYPE_HEIC} (1): HEIC 格式的动态照片
     * - {@link #MOTION_PHOTO_TYPE_NONE} (2): 非动态照片
     */
    public static native int CheckMotionPhotoType(String inputFilePath);

    /**
     * 检查图片是否为 Motion Photo
     *
     * @param inputFilePath 图片文件路径
     * @return 是否为动态照片
     */
    public static boolean isMotionPhoto(String inputFilePath) {
        int type = CheckMotionPhotoType(inputFilePath);
        return type == MOTION_PHOTO_TYPE_JPEG || type == MOTION_PHOTO_TYPE_HEIC;
    }

    /**
     * 根据文件路径自动提取 Motion Photo 中的视频数据
     *
     * @param inputFilePath 图片文件路径
     * @return MP4 视频数据的字节数组，失败或非动态照片返回 null
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
