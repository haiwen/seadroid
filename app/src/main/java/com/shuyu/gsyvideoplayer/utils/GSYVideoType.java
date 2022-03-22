package com.shuyu.gsyvideoplayer.utils;

/**
 * Some default configurations of video
 */

public class GSYVideoType {

    //Default display scale
    public final static int SCREEN_TYPE_DEFAULT = 0;

    //16:9
    public final static int SCREEN_TYPE_16_9 = 1;

    //4:3
    public final static int SCREEN_TYPE_4_3 = 2;

    public final static int SCREEN_TYPE_FULL = 4;

    public final static int SCREEN_MATCH_FULL = -4;

    public final static int GLSURFACE = 2;

    public final static int SUFRACE = 1;

    public final static int TEXTURE = 0;


    private static int TYPE = SCREEN_TYPE_DEFAULT;

    private static boolean MEDIA_CODEC_FLAG = false;

    private static int sRenderType = TEXTURE;

    private static boolean sTextureMediaPlay = false;


    /**
     * Enable hard decoding, set before playing
     */
    public static void enableMediaCodec() {
        MEDIA_CODEC_FLAG = true;
    }

    /**
     * Disable hard decoding, set before playing
     */
    public static void disableMediaCodec() {
        MEDIA_CODEC_FLAG = false;
    }

    public static void enableMediaCodecTexture() {
        sTextureMediaPlay = true;
    }

    public static void disableMediaCodecTexture() {
        sTextureMediaPlay = false;
    }

    public static boolean isMediaCodec() {
        return MEDIA_CODEC_FLAG;
    }

    public static boolean isMediaCodecTexture() {
        return sTextureMediaPlay;
    }

    public static int getShowType() {
        return TYPE;
    }

    public static void setShowType(int type) {
        TYPE = type;
    }


    public static int getRenderType() {
        return sRenderType;
    }

    public static void setRenderType(int renderType) {
        sRenderType = renderType;
    }

}
