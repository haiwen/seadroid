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
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.worker.queue.TransferModel;
import com.seafile.seadroid2.framework.util.Utils;

import java.io.File;

/**
 * v = 2
 */
@Entity(tableName = "file_backup_status", indices = {
        @Index(value = {"uid", "full_path", "related_account"}, unique = true, name = "index_backup_path")
})
public class FileBackupStatusEntity extends BaseModel {


    /**
     * this field value is md5(related_account + repo_id + data_source + full_path)
     */
    @PrimaryKey
    @NonNull
    public String uid = "";

    /**
     * <h2>Original file path</h2>
     * <p>The value of this field depends on the type of data_source</p>
     * <br>
     * <p><b>ALBUM_BACKUP/FOLDER_BACKUP</b></p>
     * <p>
     * full_path is the absolute path to the file stored locally ("/storage/emulated/0/").
     * <br>
     * eg. /storage/emulated/0/DCIM/xxx.jpg or /storage/emulated/0/Downloads/xxx.txt
     * </p>
     */
    public String full_path;


    /**
     * <h2>Destination path<h2/>
     * <p>The value of this field depends on the type of data_source</p>
     * <p><b>ALBUM_BACKUP</b></p>
     * <p>
     * target_path's format is "/My Photos/" + buckName.
     * <p/>
     * <pre>
     * {@code
     *     data_source = ALBUM_BACKUP
     *     full_path = /storage/emulated/0/DCIM/mm/xxx.jpg
     *     -> target_path = /My Photos/mm/xxx.jpg (in remote)
     * }
     * </pre>
     * <p><b>FOLDER_BACKUP</b></p>
     * <p>
     * target_path is the name stored in the repository, which is the parent directory of <b><i>full_path<i/></b>.
     * </p>
     * <pre>
     * {@code
     *     data_source = FOLDER_BACKUP
     *     full_path = /storage/emulated/0/Download/mm/xxx.jpg (locally)
     *     -> target_path = /mm/xxx.jpg (in remote)
     * }
     * </pre>
     */
    public String target_path;

    /**
     * <p>
     * parent_path is the parent directory of <b><i>target_path<i/></b>.<br>
     * </p>
     * <pre>
     * {@code
     *     full_path = /storage/emulated/0/DCIM/mm/xxx.jpg
     *     target_path = /My Photos/mm/xxx.jpg
     *     -> parent_path = /mm/
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


    /**
     * This task comes from which function is being called.
     *
     * @see TransferDataSource
     */
    public TransferDataSource data_source;

    public String repo_id;
    public String repo_name;
    public String related_account;

    /**
     * when action is upload: The value is the ID returned after the file upload is complete
     * when action is download: file_id is id.
     */
    public String file_id;

    public String file_name;

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
    public int backup_status = 1;

    @Override
    public String toString() {
        return "FileBackupStatusEntity{" +
                ", source='" + data_source + '\'' +
                ", repo_name='" + repo_name + '\'' +
                ", full_path='" + full_path + '\'' +
                '}';
    }

    /**
     * md5(related_account + transfer_action + full_path)
     */
    @NonNull
    public String getUID() {
        if (TextUtils.isEmpty(related_account)) {
            throw new IllegalArgumentException("related_account can not be null.");
        }

        if (TextUtils.isEmpty(repo_id)) {
            throw new IllegalArgumentException("repo_id can not be null.");
        }

        if (TextUtils.isEmpty(full_path)) {
            throw new IllegalArgumentException("full_path can not be null.");
        }

        return EncryptUtils.encryptMD5ToString(related_account + repo_id + data_source + full_path).toLowerCase();
    }

    public static FileBackupStatusEntity convertTransferModel2This(TransferModel transferModel, String fileId) {
        if (transferModel == null) {
            return null;
        }

        FileBackupStatusEntity entity = new FileBackupStatusEntity();
        entity.v = 2;//new version
        entity.repo_id = transferModel.repo_id;
        entity.repo_name = transferModel.repo_name;
        entity.related_account = transferModel.related_account;

        entity.file_id = fileId;
        entity.full_path = transferModel.full_path;
        entity.target_path = transferModel.target_path;

        entity.setParent_path(transferModel.getParentPath());

        File file = new File(entity.full_path);
        entity.file_name = transferModel.file_name;
        entity.file_size = file.length();
        entity.file_format = FileUtils.getFileExtension(entity.full_path);
//        entity.file_md5 = FileUtils.getFileMD5ToString(entity.full_path).toLowerCase();
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.created_at = System.currentTimeMillis();
        entity.modified_at = entity.created_at;
        entity.data_source = transferModel.data_source;

        entity.uid = entity.getUID();

        return entity;
    }

}
