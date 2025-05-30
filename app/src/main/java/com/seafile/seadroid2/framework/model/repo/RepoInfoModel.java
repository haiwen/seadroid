package com.seafile.seadroid2.framework.model.repo;

import android.text.TextUtils;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.framework.db.entities.RepoModel;

import org.apache.commons.lang3.StringUtils;

import java.util.Objects;


public class RepoInfoModel {

    public String id;
    public String name;   //repo_name

    public String type;   //mine\group\shared
    public String owner;

    public long mtime;
    public long size;
    public boolean encrypted;
    public String root;
    public String permission;
    public String modifier_email;
    public String modifier_contact_email;
    public String modifier_name;
    public int file_count;
    public String head_commit_id;
    public int enc_version;

    public String magic;
    public String salt;
    public String random_key;

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
        RepoInfoModel repoModel = (RepoInfoModel) o;
        return encrypted == repoModel.encrypted
                && size == repoModel.size
                && enc_version == repoModel.enc_version
                && file_count == repoModel.file_count
                && mtime == repoModel.mtime
                && Objects.equals(id, repoModel.id)
                && Objects.equals(name, repoModel.name)
                && Objects.equals(type, repoModel.type)
                && Objects.equals(modifier_email, repoModel.modifier_email)
                && Objects.equals(modifier_name, repoModel.modifier_name)
                && Objects.equals(modifier_contact_email, repoModel.modifier_contact_email)
                && Objects.equals(permission, repoModel.permission)
                && Objects.equals(salt, repoModel.salt)
                && Objects.equals(root, repoModel.root)
                && Objects.equals(magic, repoModel.magic)
                && Objects.equals(random_key, repoModel.random_key);
    }

    @Override
    public int hashCode() {
        return Objects.hash(
                id,
                name,
                type,
                modifier_email,
                modifier_name,
                modifier_contact_email,
                encrypted,
                size,
                permission,
                salt,
                mtime,
                root,
                magic,
                random_key,
                enc_version,
                file_count);
    }

    public static RepoModel toRepoModel(Account account, RepoInfoModel repoInfoModel) {
        RepoModel repoModel = new RepoModel();
        repoModel.repo_id = repoInfoModel.id;
        repoModel.repo_name = repoInfoModel.name;
        repoModel.type = repoInfoModel.type;
        repoModel.file_count = repoInfoModel.file_count;
        repoModel.size = repoInfoModel.size;
        repoModel.encrypted = repoInfoModel.encrypted;
        repoModel.enc_version = repoInfoModel.enc_version;
        repoModel.last_modified_long = repoInfoModel.mtime;
        repoModel.related_account = account.getSignature();
        repoModel.permission = repoInfoModel.permission;
        repoModel.salt = repoInfoModel.salt;
        repoModel.magic = repoInfoModel.magic;
        repoModel.random_key = repoInfoModel.random_key;
        repoModel.root = repoInfoModel.root;
        repoModel.modifier_email = repoInfoModel.modifier_email;
        repoModel.modifier_name = repoInfoModel.modifier_name;
        repoModel.modifier_contact_email = repoInfoModel.modifier_contact_email;
        return repoModel;
    }
}
