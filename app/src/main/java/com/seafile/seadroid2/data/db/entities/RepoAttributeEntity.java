package com.seafile.seadroid2.data.db.entities;

import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.room.Entity;
import androidx.room.PrimaryKey;

import com.google.gson.annotations.JsonAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.model.BaseModel;
import com.seafile.seadroid2.data.model.repo.deserializer.EncryptFieldJsonAdapter;
import com.seafile.seadroid2.util.Utils;
import com.seafile.seadroid2.util.sp.SettingsManager;

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
                && SettingsManager.getInstance().isEncryptEnabled();
    }
}
