package com.seafile.seadroid2.framework.motionphoto;

import com.seafile.seadroid2.annotation.Todo;

import java.util.List;

@Todo
public final class MotionPhotoDescriptor {

    /**
     * Motion Photo Type
     *
     * @see MotionPhotoTypeEnum
     *
     */
    public MotionPhotoTypeEnum mpType = MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_NONE;
    public long motionPhotoVersion;

    /**
     * suggested playback time microseconds
     */
    public long motionPhotoPresentationTimestampUs = -1;

    public String tempJpegPath;

    public Source source = Source.CONTAINER;

    public List<MotionPhotoItem> items;

    @Override
    public String toString() {
        return "MotionPhotoDescriptor{" + mpType.name() + '}';
    }

    public boolean isMotionPhoto() {
        return mpType != MotionPhotoTypeEnum.MOTION_PHOTO_TYPE_NONE;
    }

    public enum Source {
        MOTION_PHOTO,
        CONTAINER, //NOT Motion Photo
        MICRO_VIDEO
    }

    public enum MotionPhotoTypeEnum {
        MOTION_PHOTO_TYPE_JPEG,
        MOTION_PHOTO_TYPE_HEIC,
        MOTION_PHOTO_TYPE_NONE
    }

    public static class MotionPhotoItem {
        public String semantic;
        public Long length;
        public Long padding;
        public String mime;
        public Long offset;
    }
}