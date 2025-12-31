package com.seafile.seadroid2.framework.worker.queue;

import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.enums.FeatureDataSource;
import com.seafile.seadroid2.enums.SaveTo;
import com.seafile.seadroid2.enums.TransferStatus;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.framework.worker.ExistingFileStrategy;
import com.seafile.seadroid2.framework.worker.upload.MediaBackupScanWorker;

import org.apache.commons.lang3.StringUtils;

import java.io.File;

public class TransferModel implements Comparable<TransferModel> {
    private String id;

    public String genStableId() {
        if (data_source == null) {
            throw new IllegalArgumentException("data_source is null");
        }

        if (TextUtils.isEmpty(repo_id)) {
            throw new IllegalArgumentException("repo_id is null");
        }

        if (TextUtils.isEmpty(full_path)) {
            throw new IllegalArgumentException("full_path is null");
        }

        return EncryptUtils.encryptMD5ToString(data_source.toString() + repo_id + full_path);
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    //just for uploading
    public String repo_id;
    public String repo_name;
    public String related_account;
    public String target_path;
    private String parent_path;
    public String full_path;
    public String file_name;
    public long created_at;
    public int retry_times = 0;

    /**
     *  Only motion photos in JPEG format will be converted to HEIC motion photos.
     *  HEIC Motion Photo Temporary Cache Path.
     * */
    public String motion_photo_path;


    public long file_size; //文件大小
    public long transferred_size; //已传输大小
    public TransferStatus transfer_status; //传输状态，用于传输列表页展示当前状态
    public FeatureDataSource data_source; //数据来源，用于区分不同的传输任务
    public ExistingFileStrategy transfer_strategy = ExistingFileStrategy.APPEND;
    public String err_msg; //传输结果/异常内容
    /**
     * save to where, 0:no save, 1: db,
     */
    public SaveTo save_to = SaveTo.NO_SAVE;


    public String getFileName() {
        return file_name;
    }

    public String getFileNameWithoutExtendFormat() {
        if (TextUtils.isEmpty(file_name)) {
            return file_name;
        }

        if (!file_name.contains(".")) {
            return file_name;
        }

        return file_name.substring(0, file_name.lastIndexOf("."));
    }

    public boolean hasExtraMotionPhoto() {
        if (TextUtils.isEmpty(motion_photo_path)) {
            return false;
        }

        boolean isExists = FileUtils.isFileExists(this.motion_photo_path);
        if (!isExists) {
            return false;
        }

        long length = FileUtils.getFileLength(this.motion_photo_path);
        if (length <= 0) {
            return false;
        }

        return true;
    }

    @NonNull
    @Override
    public String toString() {
        return "TransferModel{" +
                "id='" + id + '\'' +
                ", repo_id='" + repo_id + '\'' +
                ", file_name='" + file_name + '\'' +
                ", data_source=" + data_source +
                ", save_to=" + save_to +
                ", full_path='" + full_path + '\'' +
                '}';
    }

    public void setParentPath(String parent_path) {
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

    public String getParentPath() {
        return parent_path;
    }

    public static TransferModel convert(File file, String bucketName, long bucketId) {

        // /My Photos/{bucketName}/
        String p = Utils.pathJoin("/", MediaBackupScanWorker.BASE_DIR, "/", bucketName, "/");

        TransferModel entity = new TransferModel();
        entity.created_at = System.nanoTime();
        entity.full_path = file.getAbsolutePath();

        // remote path: /My Photos/{bucketName}/{fileName}
        entity.target_path = Utils.pathJoin(p, file.getName());
        entity.setParentPath(p);
        entity.file_name = file.getName();
        entity.file_size = file.length();
        entity.transferred_size = 0;
        entity.transfer_status = TransferStatus.WAITING;

        return entity;
    }

    public static TransferModel convert(File file, String localBackupPath) {

        String backupParent = Utils.getParentPath(localBackupPath);

        TransferModel entity = new TransferModel();
        entity.created_at = System.nanoTime();
        entity.full_path = file.getAbsolutePath();

        String t = entity.full_path;
        entity.target_path = StringUtils.removeStart(t, backupParent);
        entity.setParentPath(Utils.getParentPath(entity.target_path));

        entity.file_name = file.getName();
        entity.file_size = file.length();
        entity.transferred_size = 0;
        entity.transfer_status = TransferStatus.WAITING;
        return entity;
    }

    @Override
    public int compareTo(TransferModel o) {
//        // 优先按传输状态排序（WAITING > TRANSFERRING > FAILED > FINISHED）
//        if (this.transfer_status != o.transfer_status) {
//            return this.transfer_status.ordinal() - o.transfer_status.ordinal();
//        }

        // 状态相同则按创建时间倒序排序
        return Long.compare(o.created_at, this.created_at);
    }
}
