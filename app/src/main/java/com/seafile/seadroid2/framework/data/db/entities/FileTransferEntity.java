package com.seafile.seadroid2.framework.data.db.entities;


import android.text.TextUtils;
import android.webkit.MimeTypeMap;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.room.Entity;
import androidx.room.Index;
import androidx.room.PrimaryKey;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentFileModel;
import com.seafile.seadroid2.framework.data.model.dirents.DirentRecursiveFileModel;
import com.seafile.seadroid2.enums.TransferAction;
import com.seafile.seadroid2.enums.TransferDataSource;
import com.seafile.seadroid2.enums.TransferResult;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.util.FileTools;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;

import org.apache.commons.lang3.StringUtils;

import java.io.File;

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
     * <h2>Destination path<h2/>
     * <p>The value of this field depends on the type of transfer_action and data_source</p>
     * <p><b>UPLOAD: ALBUM_BACKUP</b></p>
     * <p>
     * target_path's format is "/My Photos/" + buckName.
     * <p/>
     * <pre>
     * {@code
     *     transfer_action = UPLOAD
     *     data_source = ALBUM_BACKUP
     *     full_path = /storage/emulated/0/DCIM/mm/xxx.jpg
     *     -> target_path = /My Photos/mm/xxx.jpg
     * }
     * </pre>
     * <p><b>UPLOAD: FOLDER_BACKUP/FILE_BACKUP</b></p>
     * <p>
     * target_path is the name stored in the repository, which is the parent directory of <b><i>full_path<i/></b>.
     * </p>
     * <pre>
     * {@code
     *     transfer_action = UPLOAD
     *     data_source = FOLDER_BACKUP
     *     full_path = /storage/emulated/0/Download/mm/xxx.jpg (locally)
     *     -> target_path = /mm/xxx.jpg (in remote repo)
     * }
     * </pre>
     * <p><b>or</b></p>
     * <p>
     * target_path is special dir in repo.
     * </p>
     * <pre>
     * {@code
     *     transfer_action = UPLOAD
     *     data_source = FILE_BACKUP
     *     full_path = /storage/emulated/0/Download/mm/xxx.jpg (locally)
     *     -> target_path = /test/test1/xxx.jpg (in remote repo)
     * }
     * </pre>
     *
     * <p><b>DOWNLOAD</b></p>
     * <p>
     * target_path is the absolute path to the file stored locally ("/storage/emulated/0/Android/media").
     * <br>
     * </p>
     * <pre>
     * {@code
     *     transfer_action = DOWNLOAD
     *     data_source = DOWNLOAD
     *     full_path = /a/d.txt (in remote repo)
     *     -> target_path = /storage/emulated/0/Android/media/com.xxx/Seafile/account@xxx.com (xxx.com)/My Library/a/d.txt
     * }
     * </pre>
     */
    public String target_path;

    /**
     * <h2>Original file path</h2>
     * <p>The value of this field depends on the type of transfer_action and data_source</p>
     * <br>
     * <p><b>UPLOAD (ALBUM_BACKUP/FOLDER_BACKUP) </b></p>
     * <p>
     * full_path is the absolute path to the file stored locally ("/storage/emulated/0/").
     * <br>
     * eg. /storage/emulated/0/DCIM/xxx.jpg or /storage/emulated/0/Downloads/xxx.txt (locally)
     * </p>
     * <p><b>UPLOAD (FILE_BACKUP Manually) </b></p>
     * <p>
     * full_path is the absolute path to the file stored locally
     * <br>
     * eg. /storage/emulated/0/Android/media/(package_name)/Seafile/(repo_name)/
     * </p>
     * <p><b>DOWNLOAD</b></p>
     * <p> full_path is the relative path to the file in the repository. <br>
     * eg. /a/b/c/d.txt (in remote repo)</p>
     */
    public String full_path;


    /**
     * <p>The value of this field depends on the type of transfer_action and data_source</p>
     *
     * <p><b>UPLOAD</b></p>
     * <p>
     * parent_path is the parent directory of <b><i>target_path<i/></b>.<br>
     * </p>
     * <pre>
     * {@code
     *     transfer_action = UPLOAD
     *     full_path = /storage/emulated/0/DCIM/mm/xxx.jpg
     *     target_path = /My Photos/mm/xxx.jpg
     *     -> parent_path = /mm/
     * }
     * </pre>
     *
     * <p><b>DOWNLOAD</b></p>
     * <p>
     * parent_path is the parent directory of <b><i>full_path<i/></b>.
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
     * Automatic transfer?
     * <p>
     * When this field is true, the transfer task will be automatically executed when the transfer task is added to the database.
     * <br>
     * <br>
     * When this field is false, the transfer task will be added to the database,
     * and the transfer task will be executed when the user starts manually
     * </p>
     * <br>
     * <br>
     */
    public boolean is_auto_transfer = true;

    /**
     * This task comes from which function is being called.
     *
     * @see TransferDataSource
     */
    public TransferDataSource data_source;

    public String repo_id;
    public String repo_name;

    //who
    public String related_account;

    /**
     * when action is upload: The value is the ID returned after the file upload is complete
     * when action is download: file_id is id.
     */
    public String file_id;

    public String file_name;

    @Nullable
    public String file_format;

    /**
     * <a href="https://www.iana.org/assignments/media-types/media-types.xhtml">media-types</a>
     */
    @Nullable
    public String mime_type;

    public long file_size;
    public long transferred_size;

    /**
     * When LocalAction is UPLOAD, this field should calculate the MD5 of the file first,
     * and when it is DOWNLOAD, it should wait until the file is downloaded before calculating the MD5 of the file
     */
    public String file_md5;
//
//    /**
//     * This field is used to determine whether the file can be encrypted or decrypted locally during transfer
//     * @see RepoModel#canLocalDecrypt()
//     */
//    public boolean is_block;

//    public boolean is_update;

    /**
     * An upgraded feat of the is_update field
     */
    public ExistingFileStrategy file_strategy = ExistingFileStrategy.AUTO;

    /**
     * whether is in /sdcard/Android/media/com.seafile.seadroid2.debug/Seafile/...
     */
    public boolean is_copy_to_local;

    /**
     * start timestamp (mills).
     * The time when the database was inserted
     */
    public long created_at;

    /**
     * <p>UNIT: mills<p/>
     * <p>if transfer_action = 'UPLOAD' and data_source = 'ALBUM_BACKUP', value is MediaStore.Images.ImageColumns.DATE_ADDED</p>
     * <br>
     * <p>if transfer_action = 'UPLOAD' and data_source = 'FOLDER_BACKUP', value is new File("").lastModified()</p>
     * <br>
     * <p>if transfer_action = 'DOWNLOAD' and data_source = 'DOWNLOAD', value is downloaded time.</p>
     * <br>
     */
    public long file_original_modified_at;

    /**
     * modified timestamp (mills)
     * <br>
     * <br>
     * <p><b>UPLOAD - ALBUM_BACKUP - FOLDER_BACKUP</b></p>
     * The time when the file was <i><b>modified</b><i/> locally
     * <br>
     * <br>
     * <p><b>DOWNLOAD - ALBUM_BACKUP - FOLDER_BACKUP</b></p>
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
    public TransferStatus transfer_status = TransferStatus.WAITING;

    /**
     * Result from last transfer operation.
     */
    public TransferResult transfer_result = TransferResult.NO_RESULT;

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

    @NonNull
    public String getUID() {
        if (TextUtils.isEmpty(related_account)) {
            throw new IllegalArgumentException("related_account can not be null.");
        }
        if (null == transfer_action) {
            throw new IllegalArgumentException("transfer_action can not be null.");
        }
        if (TextUtils.isEmpty(full_path)) {
            throw new IllegalArgumentException("full_path can not be null.");
        }

        return EncryptUtils.encryptMD5ToString(related_account + transfer_action + full_path).toLowerCase();
    }

    public static FileTransferEntity convertDirentModel2This(boolean is_block, DirentModel direntModel) {
        return convertDirentModel2This(is_block, true, direntModel);
    }

    public static FileTransferEntity convertDirentModel2This(boolean is_block, boolean is_auto_transfer, DirentModel direntModel) {
        FileTransferEntity entity = new FileTransferEntity();
        entity.full_path = direntModel.full_path;
//        entity.target_path = direntModel.full_path;
        entity.data_source = TransferDataSource.DOWNLOAD;
        entity.repo_id = direntModel.repo_id;
        entity.repo_name = direntModel.repo_name;
        entity.related_account = direntModel.related_account;


        entity.file_id = direntModel.id;
        entity.setParent_path(Utils.getParentPath(direntModel.full_path));
        entity.file_name = direntModel.name;
        entity.file_format = FileTools.getFileExtension(entity.full_path);
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.file_size = direntModel.size;
        entity.file_md5 = null;

        entity.is_auto_transfer = is_auto_transfer;

//        entity.is_block = is_block;
        entity.file_strategy = ExistingFileStrategy.AUTO;

        entity.is_copy_to_local = true;

        long now = System.currentTimeMillis();
        entity.created_at = now;
        entity.modified_at = direntModel.mtime * 1000;
        entity.action_end_at = 0L;

        entity.transfer_action = TransferAction.DOWNLOAD;
        entity.transfer_status = TransferStatus.WAITING;
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

//        entity.target_path = entity.full_path;
        entity.data_source = TransferDataSource.DOWNLOAD;
        entity.repo_id = repoModel.repo_id;
        entity.repo_name = repoModel.repo_name;
        entity.related_account = repoModel.related_account;

        entity.file_id = model.id;
        entity.setParent_path(Utils.getParentPath(entity.full_path));
        entity.file_name = model.name;
        entity.file_format = FileTools.getFileExtension(entity.full_path);
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.file_size = 0;
        entity.file_md5 = null;

        entity.is_auto_transfer = true;

//        entity.is_block = repoModel.encrypted;
        entity.is_copy_to_local = true;
        entity.file_strategy = ExistingFileStrategy.AUTO;

        long now = System.currentTimeMillis();
        entity.created_at = now;
        entity.modified_at = now;
        entity.action_end_at = 0L;

        entity.transfer_action = TransferAction.DOWNLOAD;
        entity.transfer_status = TransferStatus.WAITING;
        entity.transfer_result = TransferResult.NO_RESULT;

        entity.uid = entity.getUID();

        return entity;
    }

    public static FileTransferEntity convert2ThisForUploadFileSyncWorker(Account account, File file, String backupPath) {
        if (!file.isFile()) {
            return null;
        }

        FileTransferEntity entity = new FileTransferEntity();
        entity.full_path = file.getAbsolutePath();

        // backupPath
        String backupParent = Utils.getParentPath(backupPath);

        String t = entity.full_path;
        entity.target_path = StringUtils.removeStart(t, backupParent);
        entity.setParent_path(Utils.getParentPath(entity.target_path));

        entity.file_name = file.getName();
        entity.file_size = file.length();
        entity.file_format = FileUtils.getFileExtension(entity.full_path);
        entity.file_md5 = FileUtils.getFileMD5ToString(entity.full_path).toLowerCase();
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);

//        entity.is_block = repoModel.encrypted;
//        entity.repo_id = repoModel.repo_id;
//        entity.repo_name = repoModel.repo_name;
        entity.related_account = account.getSignature();
        entity.data_source = TransferDataSource.FOLDER_BACKUP;
        entity.created_at = System.currentTimeMillis();
        entity.modified_at = entity.created_at;
        entity.file_original_modified_at = file.lastModified();
        entity.action_end_at = 0;

        entity.is_auto_transfer = true;

        entity.file_strategy = ExistingFileStrategy.AUTO;
        entity.is_copy_to_local = false;
        entity.transfer_action = TransferAction.UPLOAD;
        entity.transfer_result = TransferResult.NO_RESULT;
        entity.transfer_status = TransferStatus.WAITING;

        entity.uid = entity.getUID();

        return entity;
    }


    public static FileTransferEntity convert2ThisForUploadMediaSyncWorker(Account account, File file, String parenPath, long dateAdd, boolean isRemoteExists) {
        long now = System.currentTimeMillis();

        FileTransferEntity entity = new FileTransferEntity();
        entity.full_path = file.getAbsolutePath();
        entity.target_path = Utils.pathJoin(parenPath, file.getName());
        entity.setParent_path(parenPath);
        entity.file_name = file.getName();
        entity.file_size = file.length();
        entity.file_format = FileTools.getFileExtension(entity.full_path);
        entity.file_md5 = FileUtils.getFileMD5ToString(entity.full_path).toLowerCase();
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
//        entity.is_block = false; //album backup is not store in encrypted repo.
//        entity.repo_id = repo_id;
//        entity.repo_name = repo_name;
        entity.related_account = account.getSignature();
        entity.created_at = now;
        entity.modified_at = now;
        entity.file_original_modified_at = dateAdd;
        entity.action_end_at = 0;

        entity.is_auto_transfer = true;

        entity.file_strategy = ExistingFileStrategy.AUTO;
        entity.is_copy_to_local = false;
        entity.data_source = TransferDataSource.ALBUM_BACKUP;

        if (isRemoteExists) {
            entity.transfer_action = TransferAction.UPLOAD;
            entity.transfer_result = TransferResult.TRANSMITTED;
            entity.transfer_status = TransferStatus.SUCCEEDED;
        } else {
            entity.transfer_action = TransferAction.UPLOAD;
            entity.transfer_result = TransferResult.NO_RESULT;
            entity.transfer_status = TransferStatus.WAITING;
        }


        entity.uid = entity.getUID();

        return entity;
    }

    public static FileTransferEntity convert2ThisForFileBackup(Account account, String repo_id, String repo_name, File file, String parenPath, long dateAdd, boolean is_update, boolean isRemoteExists) {
        long now = System.currentTimeMillis();

        FileTransferEntity entity = new FileTransferEntity();
        entity.full_path = file.getAbsolutePath();
        entity.target_path = Utils.pathJoin(parenPath, file.getName());
        entity.setParent_path(parenPath);
        entity.file_name = file.getName();
        entity.file_size = file.length();
        entity.file_format = FileTools.getFileExtension(entity.full_path);
        entity.file_md5 = FileUtils.getFileMD5ToString(entity.full_path).toLowerCase();
        entity.mime_type = MimeTypeMap.getSingleton().getMimeTypeFromExtension(entity.file_format);
        entity.repo_id = repo_id;
        entity.repo_name = repo_name;
        entity.related_account = account.getSignature();
        entity.created_at = now;
        entity.modified_at = now;
        entity.file_original_modified_at = dateAdd;
        entity.action_end_at = 0;

        entity.is_auto_transfer = true;

        entity.file_strategy = is_update ? ExistingFileStrategy.REPLACE : ExistingFileStrategy.KEEP;
        entity.is_copy_to_local = false;
        entity.data_source = TransferDataSource.FILE_BACKUP;

        if (isRemoteExists) {
            entity.transfer_action = TransferAction.UPLOAD;
            entity.transfer_result = TransferResult.TRANSMITTED;
            entity.transfer_status = TransferStatus.SUCCEEDED;
        } else {
            entity.transfer_action = TransferAction.UPLOAD;
            entity.transfer_result = TransferResult.NO_RESULT;
            entity.transfer_status = TransferStatus.WAITING;
        }


        entity.uid = entity.getUID();

        return entity;
    }
}
