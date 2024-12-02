package com.seafile.seadroid2.framework.data.db.entities;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;

@Entity(tableName = "starred_dirents")
public class StarredModel extends BaseModel implements Parcelable {
    @PrimaryKey(autoGenerate = true)
    public long uid;

    public String related_account;  //related account
    public String repo_id;
    public String repo_name;
    public boolean repo_encrypted;
    public boolean is_dir;
    public boolean deleted;

    public String mtime;
    public String path;
    public String obj_name;
    public String user_email;
    public String user_name;
    public String user_contact_email;

    //image thumbnail url
    public String encoded_thumbnail_src;

    @Ignore
    public long mtime_long;

    public String getSubtitle() {
        if (deleted) {
            return "";
        }

        if (TextUtils.isEmpty(mtime)) {
            return repo_name;
        }

        if (mtime_long == 0) {
            mtime_long = Times.convertMtime2Long(mtime);
        }

        if (isRepo()) {
            return Utils.translateCommitTime(mtime_long);
        }
        return repo_name + " · " + Utils.translateCommitTime(mtime_long);
    }

    public boolean isRepo() {
        return TextUtils.equals("/", path) && is_dir;
    }

    public static DirentModel convert2DirentModel(StarredModel model) {
        DirentModel d = new DirentModel();
        d.full_path = model.path;
        d.type = model.is_dir ? "dir" : "file";
        d.mtime = 0;
        d.name = model.obj_name;
        d.repo_id = model.repo_id;
        d.repo_name = model.repo_name;
        d.parent_dir = Utils.getParentPath(model.path);
        return d;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.repo_id);
        dest.writeString(this.repo_name);
        dest.writeByte(this.repo_encrypted ? (byte) 1 : (byte) 0);
        dest.writeByte(this.is_dir ? (byte) 1 : (byte) 0);
        dest.writeByte(this.deleted ? (byte) 1 : (byte) 0);
        dest.writeString(this.mtime);
        dest.writeString(this.path);
        dest.writeString(this.obj_name);
        dest.writeString(this.user_email);
        dest.writeString(this.user_name);
        dest.writeString(this.user_contact_email);
        dest.writeString(this.encoded_thumbnail_src);
//        dest.writeLong(this.size);
        dest.writeLong(this.mtime_long);
        dest.writeLong(this.uid);
    }

    public void readFromParcel(Parcel source) {
        this.repo_id = source.readString();
        this.repo_name = source.readString();
        this.repo_encrypted = source.readByte() != 0;
        this.is_dir = source.readByte() != 0;
        this.deleted = source.readByte() != 0;
        this.mtime = source.readString();
        this.path = source.readString();
        this.obj_name = source.readString();
        this.user_email = source.readString();
        this.user_name = source.readString();
        this.user_contact_email = source.readString();
        this.encoded_thumbnail_src = source.readString();
//        this.size = source.readLong();
        this.mtime_long = source.readLong();
        this.uid = source.readLong();
    }

    public StarredModel() {
    }

    protected StarredModel(Parcel in) {
        this.repo_id = in.readString();
        this.repo_name = in.readString();
        this.repo_encrypted = in.readByte() != 0;
        this.is_dir = in.readByte() != 0;
        this.deleted = in.readByte() != 0;
        this.mtime = in.readString();
        this.path = in.readString();
        this.obj_name = in.readString();
        this.user_email = in.readString();
        this.user_name = in.readString();
        this.user_contact_email = in.readString();
        this.encoded_thumbnail_src = in.readString();
//        this.size = in.readLong();
        this.mtime_long = in.readLong();
        this.uid = in.readLong();
    }

    public static final Parcelable.Creator<StarredModel> CREATOR = new Parcelable.Creator<StarredModel>() {
        @Override
        public StarredModel createFromParcel(Parcel source) {
            return new StarredModel(source);
        }

        @Override
        public StarredModel[] newArray(int size) {
            return new StarredModel[size];
        }
    };
}
