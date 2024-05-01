package com.seafile.seadroid2.framework.data.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.repo.deserializer.EncryptFieldJsonAdapter;
import com.seafile.seadroid2.framework.datastore.sp.AppDataManager;
import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;

@Entity(tableName = "repo_attributes")
public class RepoAttributeEntity extends BaseModel {
    @PrimaryKey
    @NonNull
    public String repo_id = "";
    public String repo_name;   //repo_name

    @JsonAdapter(EncryptFieldJsonAdapter.class)
    public boolean encrypted;

    public String root;
    public String magic;

    public String random_key;
    public int enc_version;
    public int file_count;

    public boolean canLocalDecrypt() {
        return encrypted
                && enc_version == 2
                && !TextUtils.isEmpty(magic)
                && AppDataManager.isEncryptEnabled();
    }
}
