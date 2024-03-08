package com.seafile.seadroid2.data.db.entities;


import android.webkit.MimeTypeMap;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.room.Entity;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.blankj.utilcode.util.EncryptUtils;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.data.model.enums.TransferAction;
import com.seafile.seadroid2.data.model.enums.TransferFeature;
import com.seafile.seadroid2.data.model.enums.TransferResult;
import com.seafile.seadroid2.data.model.enums.TransferStatus;
import com.seafile.seadroid2.util.FileTools;
import com.seafile.seadroid2.util.Utils;

@Entity(tableName = "file_transfer_list", indices = {
        @Index(value = {"uid", "full_path", "related_account"}, unique = true, name = "index_transfer_path")
})
public class FileTransferEntity extends BaseModel {


    /**
     * this field value is md5(account.email + transfer_action + full_path)
     */
    @PrimaryKey
    @NonNull
    public String uid = "";


    /**
     * <h2>Where was it stored in.<h2/>
     * <p>The value of this field depends on the type of transfer_action and data_source</p>
     * <p><b>UPLOAD - ALBUM_BACKUP<b/></p>
     * <p>
     * target_path's format is "/My Photos/" + buckName.
     * <p/>
     * <pre>
     * {@code
     *     transfer_action = UPLOAD
     *     data_source = ALBUM_BACKUP
     *     full_path = /storage/emulated/0/DCIM/mm/xxx.jpg
     *     -> target_path = /My Photos/mm/
     * }
     * </pre>
     * <p><b>UPLOAD - FOLDER_BACKUP<b/></p>
     * <p>
     * target_path is the name stored in the repository, which is the parent directory of <b><i>full_path<i/><b/>.
     * </p>
     * <pre>
     * {@code
     *     transfer_action = UPLOAD
     *     data_source = FOLDER_BACKUP
     *     full_path = /storage/emulated/0/Download/mm/xxx.jpg
     *     -> target_path = /mm/
     * }
     * </pre>
     *
     * <p><b>DOWNLOAD - ALBUM_BACKUP - FOLDER_BACKUP<b/></p>
     * <p>
     * target_path is the absolute path to the file stored locally ("/storage/emulated/0/Android/media").
     * <br>
     * </p>
     * <pre>
     * {@code
     *     transfer_action = DOWNLOAD
     *     data_source = DOWNLOAD
     *     full_path = /a/d.txt
     *     -> target_path = /storage/emulated/0/Android/media/com.xxx/Seafile/account@xxx.com (xxx.com)/My Library/a/d.txt
     * }
     * </pre>
     */
    public String target_path;

    /**
     * <h2>Original file path</h2>
     * <p>The value of this field depends on the type of transfer_action and data_source</p>
     * <br>
     * <p><b>UPLOAD - ALBUM_BACKUP - FOLDER_BACKUP<b/></p>
     * <p>
     * full_path is the absolute path to the file stored locally ("/storage/emulated/0/").
     * <br>
     * eg. /storage/emulated/0/DCIM/xxx.jpg or /storage/emulated/0/Downloads/xxx.txt
     * <br>
     * <br>
     * <p><b>DOWNLOAD - ALBUM_BACKUP - FOLDER_BACKUP<b/></p>
     * <p> full_path is the relative path to the file in the repository. <br>
     * full_path = /a/b/c/d.txt </p>
     */
    public String full_path;


    /**
     * <p>The value of this field depends on the type of transfer_action and data_source</p>
     * <p><b>UPLOAD - ALBUM_BACKUP - FOLDER_BACKUP<b/></p>
     * <p>
     * parent_path is the parent directory of <b><i>target_path<i/><b/>.<br>
     * </p>
     * <pre>
     * {@code
     *     transfer_action = UPLOAD
     *     full_path = /storage/emulated/0/DCIM/mm/xxx.jpg
     *     target_path = /My Photos/mm/
     *     -> parent_path = /mm/
     * }
     * </pre>
     * <p><b>DOWNLOAD - ALBUM_BACKUP - FOLDER_BACKUP<b/></p>
     * <p>
     * parent_path is the parent directory of <b><i>full_path<i/><b/>.
     * </p>
     * <pre>
     * {@code
     *     transfer_action = DOWNLOAD
     *     full_path = /a/d.txt
     *     target_path = /storage/emulated/0/Android/media/com.xxx/Seafile/account@xxx.com (xxx.com)/My Library/a/b.txt
     *     -> parent_path = /a/
     * }
     * </pre>
     */
    public String parent_path;

    /**
     * This task comes from which function is being called.
     * @see TransferFeature
     */
    public TransferFeature data_source;

    public String repo_id;
    public String repo_name;

    public String related_account;

    /**
     * when action is upload: The value is the ID returned after the file upload is complete
     * when action is download: file_id is id.
     */
    public String file_id;

    public String file_name;
    public String file_format;

    public String mime_type;
    public long file_size;
    public long transferred_size;

    /**
     * When LocalAction is UPLOAD, this field should calculate the MD5 of the file first,
     * and when it is DOWNLOAD, it should wait until the file is downloaded before calculating the MD5 of the file
     */
    public String file_md5;

    public boolean is_block;

    public boolean is_update;

    public boolean is_copy_to_local;

    /**
     * start timestamp (mills).
     * The time when the database was inserted
     */
    public long created_at;
    /**
     * modified timestamp (mills)
     * <br>
     * <br>
     * <p><b>UPLOAD - ALBUM_BACKUP - FOLDER_BACKUP<b/></p>
     * The time when the file was <i><b>modified</b><i/> locally
     * <br>
     * <br>
     * <p><b>DOWNLOAD - ALBUM_BACKUP - FOLDER_BACKUP<b/></p>
     * The time when the file was <i><b>created</b><i/> locally
     */
    public long modified_at;

    /**
     * When the action ended (mills)
     * The time of the upload or download
     */
    public long action_end_at;

    /**
     * upload/download
     */
    public TransferAction transfer_action;

    /**
     * Status of upload
     */
    public TransferStatus transfer_status;

    /**
     * Result from last transfer operation.
     */
    public TransferResult transfer_result;

    @Override
    public String toString() {
        return "FileTransferEntity{" +
                ", v=" + v +
                ", source='" + data_source + '\'' +
                ", repo_name='" + repo_name + '\'' +
                ", related_account='" + related_account + '\'' +
                ", full_path='" + full_path + '\'' +
                ", transfer_action=" + transfer_action +
                ", transfer_status=" + transfer_status +
                ", transfer_result=" + transfer_result +
                '}';
    }

    public String getUID() {
        return EncryptUtils.encryptMD5ToString(related_account + transfer_action + full_path).toLowerCase();
    }

    @Nullable
    public static FileTransferEntity convertDirentModel2This(boolean is_block, DirentModel direntModel) {
        FileTransferEntity entity = new FileTransferEntity();
        entity.full_path = direntModel.full_path;
//        entity.target_path = direntModel.full_path;
        entity.data_source = TransferFeature.DOWNLOAD;
        entity.repo_id = direntModel.repo_id;
        entity.repo_name = direntModel.repo_name;
        entity.related_account = direntModel.related_account;

        entity.file_id = direntModel.id;
        entity.parent_path = Utils.getParentPath(direntModel.full_path);
        entity.file_name = direntModel.name;
        entity.file_format = FileTools.getFileExtension(entity.full_path);
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.file_size = 0;
        entity.file_md5 = null;

        entity.is_block = is_block;
        entity.is_update = false;
        entity.is_copy_to_local = true;

        long now = System.currentTimeMillis();
        entity.created_at = now;
        entity.modified_at = direntModel.mtime * 1000;
        entity.action_end_at = 0L;

        entity.transfer_action = TransferAction.DOWNLOAD;
        entity.transfer_status = TransferStatus.TRANSFER_WAITING;
        entity.transfer_result = TransferResult.NO_RESULT;

        entity.uid = entity.getUID();

        return entity;
    }


    public static FileTransferEntity convertDirentRecursiveModel2This(RepoModel repoModel, DirentRecursiveFileModel model) {
        FileTransferEntity entity = new FileTransferEntity();

        if (model.parent_dir.endsWith("/")) {
            entity.full_path = String.format("%s%s", model.parent_dir, model.name);
        } else {
            entity.full_path = String.format("%s/%s", model.parent_dir, model.name);
        }

        entity.target_path = entity.full_path;
        entity.data_source = TransferFeature.DOWNLOAD;
        entity.repo_id = repoModel.repo_id;
        entity.repo_name = repoModel.repo_name;
        entity.related_account = repoModel.related_account;

        entity.file_id = model.id;
        entity.parent_path = Utils.getParentPath(entity.full_path);
        entity.file_name = model.name;
        entity.file_format = FileTools.getFileExtension(entity.full_path);
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.file_size = 0;
        entity.file_md5 = null;

        entity.is_block = repoModel.encrypted;
        entity.is_update = false;
        entity.is_copy_to_local = true;

        long now = System.currentTimeMillis();
        entity.created_at = now;
        entity.modified_at = now;
        entity.action_end_at = 0L;

        entity.transfer_action = TransferAction.DOWNLOAD;
        entity.transfer_status = TransferStatus.TRANSFER_WAITING;
        entity.transfer_result = TransferResult.NO_RESULT;

        entity.uid = entity.getUID();

        return entity;
    }
}
