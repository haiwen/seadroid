package com.seafile.seadroid2.framework.model.search;

import android.os.Parcel;
import android.os.Parcelable;
import android.text.TextUtils;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.util.Icons;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

public class SearchModel extends BaseModel implements Parcelable {

    public boolean is_dir;
    public String fullpath;
    public String related_account;
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


    private SearchModel() {
        super();
        checkable = false;
    }

    public boolean isDir() {
        return is_dir;
    }

    public String getTitle() {
        String formatName = StringUtils.substringAfterLast(fullpath, '/');
        if (TextUtils.isEmpty(formatName)) {
            return name;
        }
        return formatName;
    }

    //    public String getSubtitle() {
//        String timestamp = Utils.translateCommitTime(last_modified * 1000);
//        if (is_dir)
//            return timestamp;
//        return Utils.readableFileSize(size) + ", " + timestamp;
//    }

    public String getSubtitle() {
        String p = Utils.getPathFromFullPath(fullpath);
        return repo_name + p;
    }


    public int getIcon() {
        if (is_dir) {
            if (TextUtils.equals(name, repo_name)) {
                return R.drawable.baseline_repo_24;
            } else {
                return R.drawable.baseline_folder_24;
            }
        }
        return Icons.getFileIcon(getTitle());
    }

    public static RepoModel convert2RepoModel(SearchModel model) {
        RepoModel d = new RepoModel();
        d.type = model.repo_type;
        d.repo_id = model.repo_id;
        d.repo_name = model.repo_name;
        d.related_account = "";//?
//        d.last_modified = model.last_modified;
        d.size = model.size;

        d.owner_email = model.repo_owner_email;
        d.owner_name = model.repo_owner_name;
        d.owner_contact_email = model.repo_owner_contact_email;

        return d;
    }

    public static DirentModel convert2DirentModel(SearchModel model) {
        DirentModel d = new DirentModel();
        d.full_path = model.fullpath;
        d.type = model.is_dir ? "dir" : "file";

        d.name = Utils.getFileNameFromPath(model.fullpath);
        d.repo_id = model.repo_id;
        d.repo_name = model.repo_name;
        d.last_modified_at = model.last_modified;
        d.size = model.size;
        d.parent_dir = Utils.getParentPath(model.fullpath);
        return d;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        SearchModel that = (SearchModel) o;
        return is_dir == that.is_dir && last_modified == that.last_modified && size == that.size && Objects.equals(fullpath, that.fullpath) && Objects.equals(repo_id, that.repo_id) && Objects.equals(repo_name, that.repo_name) && Objects.equals(repo_owner_email, that.repo_owner_email) && Objects.equals(repo_owner_name, that.repo_owner_name) && Objects.equals(repo_owner_contact_email, that.repo_owner_contact_email) && Objects.equals(thumbnail_url, that.thumbnail_url) && Objects.equals(repo_type, that.repo_type) && Objects.equals(name, that.name) && Objects.equals(content_highlight, that.content_highlight);
    }

    @Override
    public int hashCode() {
        return Objects.hash(is_dir, fullpath, repo_id, repo_name, repo_owner_email, repo_owner_name, repo_owner_contact_email, thumbnail_url, repo_type, name, content_highlight, last_modified, size);
    }

    @Override
    public int describeContents() {
        return 0;
    }

    @Override
    public void writeToParcel(Parcel dest, int flags) {
        dest.writeByte(this.is_dir ? (byte) 1 : (byte) 0);
        dest.writeString(this.fullpath);
        dest.writeString(this.related_account);
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
        this.related_account = source.readString();
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

    protected SearchModel(Parcel in) {
        this.is_dir = in.readByte() != 0;
        this.fullpath = in.readString();
        this.related_account = in.readString();
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
