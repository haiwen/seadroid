package com.seafile.seadroid2.framework.data.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.Ignore;

import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.permission.PermissionWrapperModel;

import java.util.HashMap;
import java.util.Set;

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


    /**
     * key -> repo_id or dirent_id(uid)
     */
    @Ignore
    public HashMap<String, BaseModel> cachedMap;

    public boolean hasId(String id) {
        if (cachedMap == null) {
            return false;
        }

        return cachedMap.containsKey(id);
    }

    public void cacheBaseModel(BaseModel model) {
        if (cachedMap == null) {
            cachedMap = new HashMap<>();
            return;
        }

        if (model instanceof RepoModel m) {
            cachedMap.put(m.repo_id, m);
        } else if (model instanceof DirentModel m) {
            cachedMap.put(m.uid, m);
        }
    }

    public void removeById(String id) {
        if (cachedMap == null) {
            return;
        }

        cachedMap.remove(id);
    }

    public void removeByModel(BaseModel model) {
        if (cachedMap == null) {
            return;
        }

        if (model instanceof RepoModel m) {
            cachedMap.remove(m.repo_id);
        } else if (model instanceof DirentModel m) {
            cachedMap.remove(m.uid);
        }
    }

    public boolean isEmptyIds() {
        return cachedMap == null || cachedMap.isEmpty();
    }

    public PermissionEntity() {
    }

    public PermissionEntity(@NonNull String repoId, @NonNull PermissionWrapperModel model) {
        this.id = model.id;
        this.name = model.name;
        this.description = model.description;

        this.create = model.permission.create;
        this.upload = model.permission.upload;
        this.download = model.permission.download;
        this.copy = model.permission.copy;
        this.delete = model.permission.delete;
        this.modify = model.permission.modify;
        this.download_external_link = model.permission.download_external_link;
        this.preview = model.permission.preview;
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
