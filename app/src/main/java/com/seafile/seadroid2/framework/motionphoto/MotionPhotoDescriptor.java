package com.seafile.seadroid2.framework.motionphoto;

import com.seafile.seadroid2.annotation.Todo;

import java.util.List;

@Todo
public final class MotionPhotoDescriptor {

    /**
     * 是否被判定为动态照片
     */
    public boolean isMotionPhoto = false;
    public long motionPhotoVersion;
    /**
     * 建议的播放时间点（微秒）
     */
    public long motionPhotoPresentationTimestampUs = -1;

    public String tempJpegPath;

    /**
     * 信息来源（调试 / 统计用）
     */
    public Source source;

    public List<MotionPhotoItem> items;

    @Override
    public String toString() {
        return "MotionPhotoDescriptor{" +
                "isMotionPhoto=" + isMotionPhoto +
                ", motionPhotoPresentationTimestampUs=" + motionPhotoPresentationTimestampUs +
                ", source=" + source +
                '}';
    }

    public enum Source {
        MOTION_PHOTO,
        CONTAINER,//NOT Motion Photo
        MICRO_VIDEO
    }

    public static class MotionPhotoItem {
        public String semantic;
        public Long length;
        public Long padding;
        public String mime;
        public Long offset;
    }
}