package com.seafile.seadroid2.framework.model.livephoto;

import android.net.Uri;
import com.google.gson.annotations.SerializedName;

public class LivePhotoModel {
    @SerializedName("imageUri")
    private final Uri imageUri;
    @SerializedName("videoUri")
    private final Uri videoUri;
    @SerializedName("displayName")
    private final String displayName;
    @SerializedName("takenTime")
    private final long takenTime;

    public LivePhotoModel(Uri imageUri, Uri videoUri, String displayName, long takenTime) {
        this.imageUri = imageUri;
        this.videoUri = videoUri;
        this.displayName = displayName;
        this.takenTime = takenTime;
    }

    @Override
    public String toString() {
        return "LivePhotoModel{" +
                "imageUri=" + imageUri +
                ", videoUri=" + videoUri +
                ", displayName='" + displayName + '\'' +
                ", takenTime=" + takenTime +
                '}';
    }

    public Uri getImageUri() {
        return imageUri;
    }

    public Uri getVideoUri() {
        return videoUri;
    }

    public String getDisplayName() {
        return displayName;
    }

    public long getTakenTime() {
        return takenTime;
    }
}
