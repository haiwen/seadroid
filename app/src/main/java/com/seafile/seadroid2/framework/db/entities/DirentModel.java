package com.seafile.seadroid2.framework.db.entities;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.PrimaryKey;

import com.blankj.utilcode.util.EncryptUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.model.search.SearchModel;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

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

    @Deprecated
    public TransferStatus transfer_status;

    @Ignore
    public String local_file_id;

    @Ignore
    private String timestamp;

    @Ignore
    private String file_ext;

    public String getFileExt() {
        if (TextUtils.isEmpty(file_ext)) {
            if (type.equals("dir")) {
                return "";
            }

            int lastDot = name.lastIndexOf('.');
            if (lastDot == -1 || lastDot == name.length() - 1) return "";
            file_ext = name.substring(lastDot + 1).toLowerCase();
        }
        return file_ext;
    }

    public String getUID() {
        return EncryptUtils.encryptMD5ToString(repo_id + parent_dir + name).toLowerCase();
    }

    public boolean isDir() {
        return TextUtils.equals(type, "dir");
    }

    public String getName() {
        return name;
    }

    public String getFullName() {
        return Utils.pathJoin(parent_dir, name);
    }

    public String getSubtitle() {
        if (TextUtils.isEmpty(timestamp)) {
            timestamp = Utils.translateCommitTime(mtime * 1000);
        }
        if (isDir())
            return timestamp;
        return Utils.readableFileSize(size) + " · " + timestamp;
    }

    public int getIcon() {
        if (isDir()) {
            if (isCustomPermission()) {
                return R.drawable.baseline_folder_24;
            } else if (!hasWritePermission()) {
                return R.drawable.baseline_folder_read_only_24;
            } else {
                return R.drawable.baseline_folder_24;
            }
        }
        return Icons.getFileIcon(name);
    }

    public boolean hasWritePermission() {
        if (TextUtils.isEmpty(permission)) {
            return false;
        }
        if (permission.equals("cloud-edit") || permission.equals("preview")) {
            return false;
        }
        return permission.contains("w");
    }

    public boolean hasDownloadPermission() {
        if (TextUtils.isEmpty(permission)) {
            return false;
        }
        return !permission.equals("cloud-edit") && !permission.equals("preview");
    }

    public boolean isCustomPermission() {
        return !TextUtils.isEmpty(permission) && permission.startsWith("custom-");
    }

    public int getCustomPermissionNum() {
        String[] ss = StringUtils.split(permission, "-");
        return Integer.parseInt(ss[1]);
    }

    public static DirentModel convertStarredModelToThis(StarredModel starredModel) {
        if (starredModel == null) return null;
        DirentModel direntModel = new DirentModel();
        direntModel.full_path = starredModel.path;
        direntModel.related_account = starredModel.related_account;
        direntModel.repo_id = starredModel.repo_id;
        direntModel.repo_name = starredModel.repo_name;
        direntModel.type = starredModel.is_dir ? "dir" : "file";
        direntModel.mtime = Times.convertMtime2Long(starredModel.mtime);
        direntModel.parent_dir = Utils.getParentPath(starredModel.path);
        direntModel.name = starredModel.obj_name;
        direntModel.encoded_thumbnail_src = starredModel.encoded_thumbnail_src;
        direntModel.uid = direntModel.getUID();
        return direntModel;
    }

    public static DirentModel convertDetailModelToThis(DirentFileModel model, String full_path, String repo_id, String repo_name) {
        if (model == null) return null;
        DirentModel direntModel = new DirentModel();
        direntModel.full_path = full_path;
        direntModel.repo_id = repo_id;
        direntModel.repo_name = repo_name;
        direntModel.type = model.type;
        direntModel.mtime = model.getMtimeInMills();
        direntModel.parent_dir = Utils.getParentPath(full_path);
        direntModel.name = model.name;
        direntModel.last_modified_at = Times.convertMtime2Long(model.last_modified);
        direntModel.modifier_email = model.last_modifier_email;
        direntModel.modifier_name = model.last_modifier_name;
        direntModel.modifier_contact_email = model.last_modifier_contact_email;
        direntModel.size = model.size;
        direntModel.permission = model.permission;
        direntModel.id = model.id;
        direntModel.starred = model.starred;
        direntModel.uid = direntModel.getUID();
        return direntModel;
    }

    public static DirentModel convertActivityModelToThis(ActivityModel model) {
        if (model == null) return null;
        DirentModel direntModel = new DirentModel();
        direntModel.related_account = model.related_account;
        direntModel.full_path = model.path;
        direntModel.repo_id = model.repo_id;
        direntModel.repo_name = model.repo_name;
        direntModel.type = model.obj_type;
        direntModel.mtime = Times.convertMtime2Long(model.time);
        direntModel.parent_dir = Utils.getParentPath(model.path);
        direntModel.name = model.name;
        direntModel.uid = direntModel.getUID();
        return direntModel;
    }

    public static DirentModel convertSearchModelToThis(SearchModel searchModel) {
        if (searchModel == null) return null;
        DirentModel direntModel = new DirentModel();
        direntModel.related_account = searchModel.related_account;
        direntModel.full_path = searchModel.fullpath;
        direntModel.repo_id = searchModel.repo_id;
        direntModel.repo_name = searchModel.repo_name;
        direntModel.type = searchModel.is_dir ? "dir" : "file";
        direntModel.parent_dir = Utils.getParentPath(searchModel.fullpath);
        direntModel.name = searchModel.name;
        direntModel.uid = direntModel.getUID();
        return direntModel;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        DirentModel that = (DirentModel) o;
        return mtime == that.mtime
                && starred == that.starred
                && size == that.size
                && last_modified_at == that.last_modified_at
                && Objects.equals(uid, that.uid)
                && Objects.equals(full_path, that.full_path)
                && Objects.equals(name, that.name)
                && Objects.equals(parent_dir, that.parent_dir)
                && Objects.equals(id, that.id)
                && Objects.equals(type, that.type)
                && Objects.equals(permission, that.permission)
                && Objects.equals(dir_id, that.dir_id)
                && Objects.equals(related_account, that.related_account)
                && Objects.equals(repo_id, that.repo_id)
                && Objects.equals(repo_name, that.repo_name)
                && Objects.equals(timestamp, that.timestamp);
    }

    @Override
    public int hashCode() {
        return Objects.hash(uid, full_path, name, parent_dir, id, type, mtime, permission,
                starred, dir_id, related_account, repo_id, repo_name, size, timestamp);
    }

    @Override
    public String toString() {
        return "DirentModel{" +
                "uid='" + uid + '\'' +
                ", full_path='" + full_path + '\'' +
                ", name='" + name + '\'' +
                ", id='" + id + '\'' +
                ", type='" + type + '\'' +
                ", permission='" + permission + '\'' +
                ", starred=" + starred +
                ", repo_id='" + repo_id + '\'' +
                '}';
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
        // FIXED: Removed source.readInt() which was causing byte mismatch
        this.timestamp = source.readString();
    }

    public DirentModel() {
    }

    public DirentModel(ItemPositionEnum positionEnum) {
        item_position = positionEnum;
    }

    protected DirentModel(Parcel in) {
        this.readFromParcel(in);
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
