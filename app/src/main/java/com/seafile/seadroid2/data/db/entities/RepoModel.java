package com.seafile.seadroid2.data.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.ColumnInfo;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.sp.SettingsManager;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.model.repo.deserializer.EncryptFieldJsonAdapter;
import com.seafile.seadroid2.util.Utils;

@Entity(tableName = "repos")
public class RepoModel extends BaseModel {
    @PrimaryKey
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

    public String root;
    public String magic;

    public String random_key;
    public int enc_version;
    public int file_count;

    public String getSubtitle() {
        return Utils.readableFileSize(size) + " · " + Utils.translateCommitTime(last_modified_long);
    }

    public int getIcon() {
        if (encrypted)
            return R.drawable.repo_encrypted;
        if (!hasWritePermission())
            return R.drawable.repo_readonly;

        return R.drawable.repo;
    }

    public boolean hasWritePermission() {
        return !TextUtils.isEmpty(permission) && permission.contains("w");
    }

    public boolean canLocalDecrypt() {
        return encrypted
                && enc_version == 2
                && !TextUtils.isEmpty(magic)
                && SettingsManager.getInstance().isEncryptEnabled();
    }
}
