package com.seafile.seadroid2.framework.data.db.entities;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

import com.blankj.utilcode.util.EncryptUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.data.model.enums.TransferStatus;
import com.seafile.seadroid2.framework.data.model.search.SearchModel;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;

@Entity(tableName = "dirents")
public class DirentModel extends BaseModel implements Parcelable {

    /**
     * md5(repo_id + parent_dir + name)
     */
    @PrimaryKey
    @NonNull
    public String uid = "";

    /**
     * parent_dir + name
     */
    public String full_path = "";

    ///////////////////common///////////////


    public String name;

    /**
     * start and end with a slash, unless parent_dir is /.
     */
    public String parent_dir;

    //file id or folder id. if empty, value is 0000000000000000000000000000000000000000
    public String id;
    public String type;
    public long mtime;   // last modified timestamp (seconds)
    public String permission;
    public boolean starred;
    public String dir_id;
    public String related_account;//signature
    public String repo_id;
    public String repo_name;

    ///////////////////file///////////////

    public long size;    // size of file, 0 if type is dir
    public boolean is_locked;
    public boolean is_freezed;
    public boolean locked_by_me;
    public long lock_time;
    public String lock_owner;
    public String lock_owner_name;
    public String lock_owner_contact_email;
    public String modifier_email;
    public String modifier_name;
    public String modifier_contact_email;
    public String encoded_thumbnail_src;

    //locally last modified time (mills)
    public long last_modified_at = 0;

//    //transfer
//    public String transfer_id;
//    public String transfer_target_path;

    public TransferStatus transfer_status;

    @Ignore
    public String local_file_path;

    @Ignore
    private String timestamp;

    public String getUID() {
        return EncryptUtils.encryptMD5ToString(repo_id + parent_dir + name).toLowerCase();
    }

    public boolean isDir() {
        return TextUtils.equals(type, "dir");
    }

    public String getSubtitle() {
        if (TextUtils.isEmpty(timestamp)) {
            timestamp = Utils.translateCommitTime(mtime * 1000);
        }
        if (isDir())
            return timestamp;
        return Utils.readableFileSize(size) + ", " + timestamp;
    }

    public int getIcon() {
        if (isDir()) {
            if (!hasWritePermission()) {
                return R.drawable.ic_folder_read_only;
            } else {
                return R.drawable.ic_folder;
            }
        }
        return Icons.getFileIcon(name);
    }

    public boolean hasWritePermission() {
        return !TextUtils.isEmpty(permission) && permission.contains("w");
    }

    public static DirentModel convertStarredModelToThis(StarredModel starredModel) {
        if (starredModel == null) {
            return null;
        }
        DirentModel direntModel = new DirentModel();
        direntModel.full_path = starredModel.path;
        direntModel.repo_id = starredModel.repo_id;
        direntModel.repo_name = starredModel.repo_name;
        direntModel.type = starredModel.is_dir ? "dir" : "file";
        direntModel.mtime = Times.convertMtime2Long(starredModel.mtime);
        direntModel.parent_dir = Utils.getParentPath(starredModel.path);
        direntModel.name = starredModel.obj_name;
        direntModel.encoded_thumbnail_src = starredModel.encoded_thumbnail_src;
//        direntModel.size = starredModel.size;
//        direntModel.repo_id = starredModel.repo_encrypted;
//        direntModel.deleted = starredModel.deleted;
//        direntModel.repo_id = starredModel.user_email;
//        direntModel.repo_id = starredModel.user_name;
//        direntModel.repo_id = starredModel.user_contact_email;

        direntModel.uid = direntModel.getUID();

        return direntModel;
    }

    public static DirentModel convertActivityModelToThis(ActivityModel model) {
        if (model == null) {
            return null;
        }

        DirentModel direntModel = new DirentModel();
        direntModel.full_path = model.path;
        direntModel.repo_id = model.repo_id;
        direntModel.repo_name = model.repo_name;
        direntModel.type = model.obj_type;
        direntModel.mtime = Times.convertMtime2Long(model.time);
        direntModel.parent_dir = Utils.getParentPath(model.path);
        direntModel.name = model.name;
//        direntModel.encoded_thumbnail_src = model.encoded_thumbnail_src;
//        direntModel.size = model.size;
//        direntModel.repo_id = model.repo_encrypted;
//        direntModel.deleted = model.deleted;
//        direntModel.repo_id = model.user_email;
//        direntModel.repo_id = model.user_name;
//        direntModel.repo_id = model.user_contact_email;

        direntModel.uid = direntModel.getUID();

        return direntModel;
    }

    public static DirentModel convertSearchModelToThis(SearchModel searchModel) {
        if (searchModel == null) {
            return null;
        }

        DirentModel direntModel = new DirentModel();
        direntModel.full_path = searchModel.fullpath;
        direntModel.repo_id = searchModel.repo_id;
        direntModel.repo_name = searchModel.repo_name;
        direntModel.type = searchModel.is_dir ? "dir" : "file";
        direntModel.mtime = searchModel.last_modified;
        direntModel.parent_dir = Utils.getParentPath(searchModel.fullpath);
        direntModel.name = searchModel.name;
        direntModel.encoded_thumbnail_src = searchModel.thumbnail_url;
        direntModel.size = searchModel.size;

        direntModel.uid = direntModel.getUID();
        return direntModel;
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeString(this.uid);
        dest.writeString(this.full_path);
        dest.writeString(this.name);
        dest.writeString(this.parent_dir);
        dest.writeString(this.id);
        dest.writeString(this.type);
        dest.writeLong(this.mtime);
        dest.writeString(this.permission);
        dest.writeString(this.dir_id);
        dest.writeByte(this.starred ? (byte) 1 : (byte) 0);
        dest.writeLong(this.size);
        dest.writeString(this.related_account);
        dest.writeString(this.repo_id);
        dest.writeString(this.repo_name);
        dest.writeByte(this.is_locked ? (byte) 1 : (byte) 0);
        dest.writeByte(this.is_freezed ? (byte) 1 : (byte) 0);
        dest.writeByte(this.locked_by_me ? (byte) 1 : (byte) 0);
        dest.writeLong(this.lock_time);
        dest.writeString(this.lock_owner);
        dest.writeString(this.lock_owner_name);
        dest.writeString(this.lock_owner_contact_email);
        dest.writeString(this.modifier_email);
        dest.writeString(this.modifier_name);
        dest.writeString(this.modifier_contact_email);
        dest.writeString(this.encoded_thumbnail_src);
        dest.writeLong(this.last_modified_at);
        dest.writeInt(this.transfer_status == null ? -1 : this.transfer_status.ordinal());
        dest.writeString(this.timestamp);
    }

    public void readFromParcel(Parcel source) {
        this.uid = source.readString();
        this.full_path = source.readString();
        this.name = source.readString();
        this.parent_dir = source.readString();
        this.id = source.readString();
        this.type = source.readString();
        this.mtime = source.readLong();
        this.permission = source.readString();
        this.dir_id = source.readString();
        this.starred = source.readByte() != 0;
        this.size = source.readLong();
        this.related_account = source.readString();
        this.repo_id = source.readString();
        this.repo_name = source.readString();
        this.is_locked = source.readByte() != 0;
        this.is_freezed = source.readByte() != 0;
        this.locked_by_me = source.readByte() != 0;
        this.lock_time = source.readLong();
        this.lock_owner = source.readString();
        this.lock_owner_name = source.readString();
        this.lock_owner_contact_email = source.readString();
        this.modifier_email = source.readString();
        this.modifier_name = source.readString();
        this.modifier_contact_email = source.readString();
        this.encoded_thumbnail_src = source.readString();
        this.last_modified_at = source.readLong();
        int tmpTransfer_status = source.readInt();
        this.transfer_status = tmpTransfer_status == -1 ? null : TransferStatus.values()[tmpTransfer_status];
        this.timestamp = source.readString();
    }

    public DirentModel() {
    }

    protected DirentModel(Parcel in) {
        this.uid = in.readString();
        this.full_path = in.readString();
        this.name = in.readString();
        this.parent_dir = in.readString();
        this.id = in.readString();
        this.type = in.readString();
        this.mtime = in.readLong();
        this.permission = in.readString();
        this.dir_id = in.readString();
        this.starred = in.readByte() != 0;
        this.size = in.readLong();
        this.related_account = in.readString();
        this.repo_id = in.readString();
        this.repo_name = in.readString();
        this.is_locked = in.readByte() != 0;
        this.is_freezed = in.readByte() != 0;
        this.locked_by_me = in.readByte() != 0;
        this.lock_time = in.readLong();
        this.lock_owner = in.readString();
        this.lock_owner_name = in.readString();
        this.lock_owner_contact_email = in.readString();
        this.modifier_email = in.readString();
        this.modifier_name = in.readString();
        this.modifier_contact_email = in.readString();
        this.encoded_thumbnail_src = in.readString();
        this.last_modified_at = in.readLong();
        int tmpTransfer_status = in.readInt();
        this.transfer_status = tmpTransfer_status == -1 ? null : TransferStatus.values()[tmpTransfer_status];
        this.timestamp = in.readString();
    }

    public static final Creator<DirentModel> CREATOR = new Creator<DirentModel>() {
        @Override
        public DirentModel createFromParcel(Parcel source) {
            return new DirentModel(source);
        }

        @Override
        public DirentModel[] newArray(int size) {
            return new DirentModel[size];
        }
    };
}
