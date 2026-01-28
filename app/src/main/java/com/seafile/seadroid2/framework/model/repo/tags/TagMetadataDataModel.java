package com.seafile.seadroid2.framework.model.repo.tags;

import android.os.Parcel;
import android.os.Parcelable;

public class TagMetadataDataModel implements Parcelable {
    public String display_column_key;
    public String link_id;
    public String other_table_id;
    public String table_id;
    public Boolean is_linked_back;

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.display_column_key);
        dest.writeString(this.link_id);
        dest.writeString(this.other_table_id);
        dest.writeString(this.table_id);
        dest.writeValue(this.is_linked_back);
    }

    public TagMetadataDataModel() {
    }

    protected TagMetadataDataModel(Parcel in) {
        this.display_column_key = in.readString();
        this.link_id = in.readString();
        this.other_table_id = in.readString();
        this.table_id = in.readString();
        this.is_linked_back = (Boolean) in.readValue(Boolean.class.getClassLoader());
    }

    public static final Parcelable.Creator<TagMetadataDataModel> CREATOR = new Parcelable.Creator<TagMetadataDataModel>() {
        @Override
        public TagMetadataDataModel createFromParcel(Parcel source) {
            return new TagMetadataDataModel(source);
        }

        @Override
        public TagMetadataDataModel[] newArray(int size) {
            return new TagMetadataDataModel[size];
        }
    };
}
