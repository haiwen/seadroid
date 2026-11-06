package com.seafile.seadroid2.framework.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.Ignore;
import androidx.room.TypeConverters;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.gson.annotations.JsonAdapter;
import com.google.gson.annotations.SerializedName;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.adapter.EncryptFieldJsonAdapter;
import com.seafile.seadroid2.framework.util.Utils;

import org.apache.commons.lang3.StringUtils;

import java.util.List;
import java.util.Objects;

@Entity(tableName = "repos", primaryKeys = {"repo_id", "group_id"})
public class RepoModel extends BaseModel {

    @SerializedName("repo_id")
    @NonNull
    public String repo_id = "";
    @SerializedName("repo_name")
    public String repo_name;   //repo_name

    @SerializedName("type")
    public String type;   //mine\group\shared

    @SerializedName("group_id")
    public long group_id;
    @SerializedName("group_name")
    public String group_name;

    @SerializedName("owner_name")
    public String owner_name;

    /**
     * xxx@auth.local
     */
    @SerializedName("owner_email")
    public String owner_email;

    /**
     * xxx@xxx.com
     */
    @SerializedName("owner_contact_email")
    public String owner_contact_email;

    @SerializedName("modifier_email")
    public String modifier_email;
    @SerializedName("modifier_name")
    public String modifier_name;
    @SerializedName("modifier_contact_email")
    public String modifier_contact_email;


    @SerializedName("related_account")
    public String related_account;  //related account


    @SerializedName("last_modified")
    public String last_modified;
    @SerializedName("last_modified_long")
    public long last_modified_long;//temp field

    @SerializedName("encrypted")
    @JsonAdapter(EncryptFieldJsonAdapter.class)
    public boolean encrypted;

    @SerializedName("size")
    public long size;
    @SerializedName("starred")
    public boolean starred;

    @SerializedName("permission")
    public String permission;
    @SerializedName("monitored")
    public boolean monitored;
    @SerializedName("is_admin")
    public boolean is_admin;
    @SerializedName("salt")
    public String salt;
    @SerializedName("status")
    public String status;


    //
    @SerializedName("root")
    public String root;
    @SerializedName("magic")
    public String magic;

    @SerializedName("random_key")
    public String random_key;
    @SerializedName("enc_version")
    public int enc_version;
    @SerializedName("file_count")
    public int file_count;


    //
//    public boolean enable_onlyoffice;
    /**
     *
     *
     */
    @SerializedName("group_admins")
    @Ignore
    public List<String> group_admins;
    @SerializedName("server_email")
    @Ignore
    public String server_email;


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

    public boolean hasManageRepoPermission() {
        if (StringUtils.equals(type, "mine")) {
            return true;
        }

        if (StringUtils.equals(type, "shared")) {
            return false;
        }

        if (CollectionUtils.isEmpty(group_admins)) {
            return false;
        }

        return group_admins.contains(server_email);
    }

    /**
     * Should check if it's a custom permission firstly
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
//                && Objects.equals(enable_onlyoffice, repoModel.enable_onlyoffice)
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
//                enable_onlyoffice,
                last_modified_long,
                root,
                magic,
                random_key,
                enc_version,
                file_count);
    }
}
