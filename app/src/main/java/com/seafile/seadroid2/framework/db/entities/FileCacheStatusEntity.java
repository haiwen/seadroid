package com.seafile.seadroid2.framework.db.entities;


import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.room.Entity;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.util.Utils;

import java.io.File;

/**
 * v = 2
 */
@Entity(tableName = "file_cache_status", indices = {
        @Index(value = {"uid", "full_path", "related_account"}, unique = true, name = "index_download_path")
})
public class FileCacheStatusEntity extends BaseModel {


    /**
     * this field value is md5(related_account + repo_id + full_path)
     */
    @PrimaryKey
    @NonNull
    public String uid = "";

    /**
     * <p><b>DOWNLOAD</b></p>
     * <p> full_path is the relative path in the repository. <br>
     * eg. /a/b/c/d.txt (in remote repo)</p>
     */
    public String full_path;

    /**
     * <p><b>DOWNLOAD</b></p>
     * <p>
     * target_path is the absolute path to the file stored locally ("/storage/emulated/0/Android/media").
     * <br>
     * </p>
     * <pre>
     * {@code
     *     full_path = /a/d.txt (in remote repo)
     *     -> target_path = /sdcard/Android/media/{package_name}/Seafile/{account}/{repo_name}/a/d.txt
     * }
     * </pre>
     */
    public String target_path;
    /**
     * <pre>
     * {@code
     *     full_path = /a/b.txt
     *     target_path = /sdcard/Android/media/{package_name}/Seafile/{account}/{repo_name}/a/d.txt
     *     -> parent_path = full_path's parent: /a/
     * }
     * </pre>
     */
    private String parent_path;

    public void setParent_path(String parent_path) {
        if (TextUtils.isEmpty(parent_path)) {
            parent_path = "/";
        }

        if (!parent_path.startsWith("/")) {
            parent_path = "/" + parent_path;
        }

        if (!parent_path.endsWith("/")) {
            parent_path = parent_path + "/";
        }

        this.parent_path = parent_path;
    }

    public String getParent_path() {
        return parent_path;
    }

    public String repo_id;
    public String repo_name;
    public String related_account;

    /**
     * file_id is dirent.id
     */
    public String file_id;

    public String file_name;
    public String motion_photo;

    public String getFileName() {
        return file_name;
    }

    public String getFullPathFileName() {
        return Utils.pathJoin(parent_path, file_name);
    }

    @Nullable
    public String file_format;

    /**
     * <a href="https://www.iana.org/assignments/media-types/media-types.xhtml">media-types</a>
     */
    @Nullable
    public String mime_type;

    public long file_size;

    public String file_md5;

    public long created_at;
    public long modified_at;

    @Override
    public String toString() {
        return "FileCacheStatusEntity{" +
                ", repo_name='" + repo_name + '\'' +
                ", full_path='" + full_path + '\'' +
                '}';
    }

    /**
     * md5(related_account + transfer_action + full_path)
     */
    @NonNull
    public String genUID() {
        if (TextUtils.isEmpty(related_account)) {
            throw new IllegalArgumentException("related_account can not be null.");
        }

        if (TextUtils.isEmpty(repo_id)) {
            throw new IllegalArgumentException("repo_id can not be null.");
        }

        if (TextUtils.isEmpty(full_path)) {
            throw new IllegalArgumentException("full_path can not be null.");
        }

        return EncryptUtils.encryptMD5ToString(related_account + repo_id + full_path).toLowerCase();
    }

    private static FileCacheStatusEntity convert(boolean isDownload, TransferModel transferModel, String fileId) {
        if (transferModel == null) {
            return null;
        }
        FileCacheStatusEntity entity = new FileCacheStatusEntity();
        entity.v = 2;//new version
        entity.repo_id = transferModel.repo_id;
        entity.repo_name = transferModel.repo_name;
        entity.related_account = transferModel.related_account;
        entity.file_name = transferModel.file_name;
        entity.file_id = fileId;


        if (isDownload) {
            //
            entity.target_path = transferModel.target_path;
            entity.full_path = transferModel.full_path;
            entity.setParent_path(Utils.getParentPath(transferModel.full_path));
        } else {
            //
            entity.target_path = transferModel.full_path;
            entity.full_path = transferModel.target_path;
            entity.setParent_path(Utils.getParentPath(transferModel.target_path));
        }

        File file = new File(entity.target_path);
        entity.file_name = transferModel.file_name;
        entity.file_size = file.length();
        entity.file_format = FileUtils.getFileExtension(entity.full_path);
        entity.file_md5 = FileUtils.getFileMD5ToString(entity.target_path).toLowerCase();
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.created_at = System.currentTimeMillis();
        entity.modified_at = entity.created_at;

        entity.uid = entity.genUID();

        return entity;
    }

    public static FileCacheStatusEntity convertFromDownload(TransferModel transferModel, String fileId) {
        return convert(true, transferModel, fileId);
    }

    public static FileCacheStatusEntity convertFromUpload(TransferModel transferModel, String fileId) {
        return convert(false, transferModel, fileId);
    }

}
