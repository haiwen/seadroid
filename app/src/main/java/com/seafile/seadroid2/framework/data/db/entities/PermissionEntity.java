package com.seafile.seadroid2.framework.data.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.Ignore;

import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.permission.PermissionParentModel;
import com.seafile.seadroid2.framework.data.model.permission.PermissionWrapperModel;

import java.util.HashMap;

@Entity(tableName = "permissions", primaryKeys = {"repo_id", "id"})
public class PermissionEntity extends BaseModel {

    @NonNull
    public String repo_id = "";

    public int id;
    public String description;
    public String name;

    //permissions
    public boolean create;
    public boolean upload;
    public boolean download;
    public boolean preview;
    public boolean copy;
    public boolean delete;
    public boolean modify;
    public boolean download_external_link;

    public PermissionEntity() {
    }

    /**
     * @return false if the permission is empty
     */
    public boolean isValid() {
        return !TextUtils.isEmpty(repo_id);
    }

    public PermissionEntity(@NonNull String repoId, @NonNull PermissionParentModel p) {
        this.id = p.id;
        this.name = p.name;
        this.description = p.description;

        this.create = p.permission.create;
        this.upload = p.permission.upload;
        this.download = p.permission.download;
        this.copy = p.permission.copy;
        this.delete = p.permission.delete;
        this.modify = p.permission.modify;
        this.download_external_link = p.permission.download_external_link;
        this.preview = p.permission.preview;
        this.repo_id = repoId;
    }

    public PermissionEntity(@NonNull String repoId, @NonNull String permission) {
        if (TextUtils.isEmpty(permission)) {
            throw new IllegalArgumentException("permission is null");
        }

        this.name = permission;
        this.id = -1;
        this.repo_id = repoId;

        if ("cloud-edit".equals(permission)) {
            //用户可以通过浏览器在线查看和编辑，文件不能被下载。
            this.create = true;
            this.upload = false;
            this.download = false;
            this.preview = true;
            this.copy = true;
            this.delete = true;
            this.modify = true;
            this.download_external_link = false;

        } else if ("preview".equals(permission)) {
            //用户只能通过浏览器在线查看，文件不能被下载。
            this.create = false;
            this.upload = false;
            this.download = false;
            this.preview = true;
            this.copy = false;
            this.delete = false;
            this.modify = false;
            this.download_external_link = false;
        } else if ("r".equals(permission)) {
            //用户可以查看、下载和同步文件
            this.create = false;
            this.upload = false;
            this.download = true;
            this.preview = true;
            this.copy = true;
            this.delete = false;
            this.modify = false;
            this.download_external_link = false;
        } else if ("rw".equals(permission)) {
            this.create = true;
            this.upload = true;
            this.download = true;
            this.preview = true;
            this.copy = true;
            this.delete = true;
            this.modify = true;
            this.download_external_link = true;
        }
    }
}
