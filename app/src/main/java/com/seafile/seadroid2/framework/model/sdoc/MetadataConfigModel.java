package com.seafile.seadroid2.framework.model.sdoc;

import android.os.Parcel;
import android.os.Parcelable;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.GsonUtils;
import com.seafile.seadroid2.framework.model.profile.DetailsSettingsColumnModel;
import com.seafile.seadroid2.framework.model.profile.DetailsSettingsKeyModel;

import org.apache.commons.lang3.StringUtils;

import java.util.List;

public class MetadataConfigModel implements Parcelable {
    public boolean enabled = false;
    public boolean tags_enabled = false;
    //public String tags_lang;
    public String details_settings;
    //public boolean ocr_enabled;

    public String getDetailsSettings() {
        return details_settings;
    }

    @Nullable
    public List<DetailsSettingsKeyModel> getDetailsSettingsList() {
        if (StringUtils.isEmpty(details_settings)) {
            return null;
        }

        DetailsSettingsColumnModel model = GsonUtils.fromJson(details_settings, DetailsSettingsColumnModel.class);
        if (model == null) {
            return null;
        }

        return model.columns;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeByte(this.enabled ? (byte) 1 : (byte) 0);
        dest.writeByte(this.tags_enabled ? (byte) 1 : (byte) 0);
    }

    public MetadataConfigModel() {
    }

    protected MetadataConfigModel(Parcel in) {
        this.enabled = in.readByte() != 0;
        this.tags_enabled = in.readByte() != 0;
    }

    public static final Parcelable.Creator<MetadataConfigModel> CREATOR = new Parcelable.Creator<MetadataConfigModel>() {
        @Override
        public MetadataConfigModel createFromParcel(Parcel source) {
            return new MetadataConfigModel(source);
        }

        @Override
        public MetadataConfigModel[] newArray(int size) {
            return new MetadataConfigModel[size];
        }
    };
}
