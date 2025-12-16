package com.seafile.seadroid2.framework.motion_photo;

public class HeicNative {


    private static final String TAG = "HeicNative";


    static {
        System.loadLibrary("heicgen");
    }

    public HeicNative() {

    }

    private static HeicNative heicNative;

    public static HeicNative getInstance() {
        if (heicNative == null) {
            heicNative = new HeicNative();
        }
        return heicNative;
    }


    // Native methods
    public native String nativeGetLibVersion();

    public native boolean genStillHeicSeq(byte[] primaryImage, String outputPath);

    /**
     * 生成 Google Motion Photo 格式的 HEIC 动态照片
     *
     * @param primaryImage 主图 JPEG 数据
     * @param mp4Video     视频 MP4 数据
     * @param outputPath   输出文件路径
     * @return 结果字符串 (success:... 或 error:...)
     */
    public native String nativeGenGoogleHeicMotionPhoto(byte[] primaryImage, byte[] mp4Video, String outputPath);

    /**
     * 从 HEIC Motion Photo 文件中提取 MP4 视频数据
     * (适用于 mpvd box 格式的 HEIC 动态照片)
     *
     * @param inputFilePath HEIC Motion Photo 文件路径
     * @return MP4 视频数据的字节数组，失败返回 null
     */
    public native byte[] nativeExtractGoogleHeicMotionPhotoVideo(String inputFilePath);

    /**
     * 从 JPEG Motion Photo 文件中提取 MP4 视频数据
     * (适用于 Google 相机拍摄的 JPEG 格式动态照片)
     * <p>
     * JPEG Motion Photo 结构：JPEG 图片 + MP4 视频直接追加
     *
     * @param inputFilePath JPEG Motion Photo 文件路径
     * @return MP4 视频数据的字节数组，失败返回 null
     */
    public native byte[] nativeExtractGoogleJpegMotionPhotoVideo(String inputFilePath);


}
