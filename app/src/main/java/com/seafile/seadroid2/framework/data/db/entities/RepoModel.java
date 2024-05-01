package com.seafile.seadroid2.framework.data.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.repo.deserializer.EncryptFieldJsonAdapter;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.util.Utils;

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
        if (encrypted)
            return R.drawable.ic_repo_encrypted;
        if (!hasWritePermission())
            return R.drawable.ic_repo_readonly;

        return R.drawable.ic_repo;
    }

    public boolean hasWritePermission() {
        return !TextUtils.isEmpty(permission) && permission.contains("w");
    }

    /**
     * If the result is true, and the decryption was successful,
     * there is definitely one row of data in the {@link EncKeyCacheEntity}
     */
    public boolean canLocalDecrypt() {
        return encrypted
                && enc_version == SettingsManager.REPO_ENC_VERSION
                && !TextUtils.isEmpty(magic)
                && AppDataManager.isEncryptEnabled();
    }
}
