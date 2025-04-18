package com.seafile.seadroid2.framework.data.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.adapter.EncryptFieldJsonAdapter;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.lang3.StringUtils;

import java.util.Objects;

@Entity(tableName = "repos", primaryKeys = {"repo_id", "group_id"})
public class RepoModel extends BaseModel {

    @NonNull
    public String repo_id = "";
    public String repo_name;   //repo_name

    public String type;   //mine\group\shared

    public long group_id;
    public String group_name;

    public String owner_name;

    /**
     * xxx@auth.local
     */
    public String owner_email;

    /**
     * xxx@xxx.com
     */
    public String owner_contact_email;

    public String modifier_email;
    public String modifier_name;
    public String modifier_contact_email;


    public String related_account;  //related account

    public String last_modified;

    @JsonAdapter(EncryptFieldJsonAdapter.class)
    public boolean encrypted;

    public long size;
    public boolean starred;

    public String permission;
    public boolean monitored;
    public boolean is_admin;
    public String salt;
    public String status;

    public long last_modified_long;


    //
    public String root;
    public String magic;

    public String random_key;
    public int enc_version;
    public int file_count;

    public String getSubtitle() {

        String subTitle = Utils.readableFileSize(size) + " · " + Utils.translateCommitTime(last_modified_long);
        if ("shared".equals(type)) {
            subTitle += " · " + owner_name;
        }

        return subTitle;
    }

    public int getIcon() {
        if (encrypted) {
            return R.drawable.baseline_repo_encrypted_24;
        } else if (isCustomPermission()) {
            return R.drawable.baseline_repo_24;
        } else if (!hasWritePermission()) {
            return R.drawable.baseline_repo_readonly_24;
        }

        return R.drawable.baseline_repo_24;
    }

    /**
     * You should to check if it's a custom permission firstly
     */
    public boolean hasWritePermission() {
        if (TextUtils.isEmpty(permission)) {
            return false;
        }

        if (permission.equals("cloud-edit")) {
            return false;
        }

        if (permission.equals("preview")) {
            return false;
        }

        return permission.contains("w");
    }

    /**
     * if start with "custom-"
     */
    public boolean isCustomPermission() {
        return !TextUtils.isEmpty(permission) && permission.startsWith("custom-");
    }

    /**
     * please check {@link #isCustomPermission()} first
     */
    public int getCustomPermissionNum() {
        if (!isCustomPermission()) {
            throw new IllegalArgumentException("please check isCustomPermission() first");
        }

        String[] ss = StringUtils.split(permission, "-");
        return Integer.parseInt(ss[1]);
    }

//    /**
//     * If the result is true, and the decryption was successful,
//     * there is definitely one row of data in the {@link EncKeyCacheEntity}
//     */
//    public boolean canLocalDecrypt() {
//        return encrypted
//                && enc_version == SettingsManager.REPO_ENC_VERSION
//                && !TextUtils.isEmpty(magic)
//                && ClientEncryptSharePreferenceHelper.isEncryptEnabled();
//    }

    /**
     * new feature at 2024/10/22 with v3.0.5
     */
    public boolean canLocalDecrypt() {
        return false;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        RepoModel repoModel = (RepoModel) o;
        return group_id == repoModel.group_id
                && encrypted == repoModel.encrypted
                && size == repoModel.size
                && starred == repoModel.starred
                && monitored == repoModel.monitored
                && is_admin == repoModel.is_admin
                && last_modified_long == repoModel.last_modified_long
                && enc_version == repoModel.enc_version
                && file_count == repoModel.file_count
                && Objects.equals(repo_id, repoModel.repo_id)
                && Objects.equals(repo_name, repoModel.repo_name)
                && Objects.equals(type, repoModel.type)
                && Objects.equals(group_name, repoModel.group_name)
                && Objects.equals(owner_name, repoModel.owner_name)
                && Objects.equals(owner_email, repoModel.owner_email)
                && Objects.equals(owner_contact_email, repoModel.owner_contact_email)
//                && Objects.equals(modifier_email, repoModel.modifier_email)
//                && Objects.equals(modifier_name, repoModel.modifier_name)
//                && Objects.equals(modifier_contact_email, repoModel.modifier_contact_email)
                && Objects.equals(related_account, repoModel.related_account)
                && Objects.equals(last_modified, repoModel.last_modified)
                && Objects.equals(permission, repoModel.permission)
                && Objects.equals(salt, repoModel.salt)
                && Objects.equals(status, repoModel.status)
                && Objects.equals(root, repoModel.root)
                && Objects.equals(magic, repoModel.magic)
                && Objects.equals(random_key, repoModel.random_key);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                repo_id,
                repo_name,
                type,
                group_id,
                group_name,
                owner_name,
                owner_email,
                owner_contact_email,
//                modifier_email,
//                modifier_name,
//                modifier_contact_email,
                related_account,
                last_modified,
                encrypted,
                size,
                starred,
                permission,
                monitored,
                is_admin,
                salt,
                status,
                last_modified_long,
                root,
                magic,
                random_key,
                enc_version,
                file_count);
    }
}
