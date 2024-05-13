package com.seafile.seadroid2.framework.data.model.search;

import android.os.Parcel;
import android.os.Parcelable;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.Utils;

public class SearchModel extends BaseModel implements Parcelable {

    public boolean is_dir;
    public String fullpath;
    public String repo_id;
    public String repo_name;
    public String repo_owner_email;
    public String repo_owner_name;
    public String repo_owner_contact_email;
    public String thumbnail_url;
    public String repo_type;

    public String name;
    public String content_highlight;

    public long last_modified;
    public long size;    // size of file, 0 if type is dir


    public String getTitle() {
        return name;
    }

    //    public String getSubtitle() {
//        String timestamp = Utils.translateCommitTime(last_modified * 1000);
//        if (is_dir)
//            return timestamp;
//        return Utils.readableFileSize(size) + ", " + timestamp;
//    }

    public String getSubtitle() {
        return repo_name;
    }


    public int getIcon() {
        if (is_dir)
            return R.drawable.baseline_folder_24;
        return Icons.getFileIcon(getTitle());
    }

    public static DirentModel converterThis2DirentModel(SearchModel model) {
        DirentModel d = new DirentModel();
        d.full_path = model.fullpath;
        d.type = model.is_dir ? "dir" : "file";

        d.name = model.name;
        d.repo_id = model.repo_id;
        d.repo_name = model.repo_name;
        d.size = model.size;
        return d;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeByte(this.is_dir ? (byte) 1 : (byte) 0);
        dest.writeString(this.fullpath);
        dest.writeString(this.repo_id);
        dest.writeString(this.repo_name);
        dest.writeString(this.repo_owner_email);
        dest.writeString(this.repo_owner_name);
        dest.writeString(this.repo_owner_contact_email);
        dest.writeString(this.thumbnail_url);
        dest.writeString(this.repo_type);
        dest.writeString(this.name);
        dest.writeString(this.content_highlight);
        dest.writeLong(this.last_modified);
        dest.writeLong(this.size);
    }

    public void readFromParcel(Parcel source) {
        this.is_dir = source.readByte() != 0;
        this.fullpath = source.readString();
        this.repo_id = source.readString();
        this.repo_name = source.readString();
        this.repo_owner_email = source.readString();
        this.repo_owner_name = source.readString();
        this.repo_owner_contact_email = source.readString();
        this.thumbnail_url = source.readString();
        this.repo_type = source.readString();
        this.name = source.readString();
        this.content_highlight = source.readString();
        this.last_modified = source.readLong();
        this.size = source.readLong();
    }

    public SearchModel() {
    }

    protected SearchModel(Parcel in) {
        this.is_dir = in.readByte() != 0;
        this.fullpath = in.readString();
        this.repo_id = in.readString();
        this.repo_name = in.readString();
        this.repo_owner_email = in.readString();
        this.repo_owner_name = in.readString();
        this.repo_owner_contact_email = in.readString();
        this.thumbnail_url = in.readString();
        this.repo_type = in.readString();
        this.name = in.readString();
        this.content_highlight = in.readString();
        this.last_modified = in.readLong();
        this.size = in.readLong();
    }

    public static final Parcelable.Creator<SearchModel> CREATOR = new Parcelable.Creator<SearchModel>() {
        @Override
        public SearchModel createFromParcel(Parcel source) {
            return new SearchModel(source);
        }

        @Override
        public SearchModel[] newArray(int size) {
            return new SearchModel[size];
        }
    };
}
