package com.seafile.seadroid2.data;

import android.text.TextUtils;

import com.seafile.seadroid2.SettingsManager;

import org.json.JSONException;
import org.json.JSONObject;


public class SeafRepoEncrypt {
    public String id;     // repo id
    public String name;
    public String owner;
    public long mtime;    // the last modification time
    public boolean isGroupRepo;
    public boolean isPersonalRepo;
    public boolean isSharedRepo;
    public boolean encrypted;
    public String permission;
    public String magic;
    public String encKey;
    public int encVersion;
    public long size;
    public String root; // the id of root directory

    static SeafRepoEncrypt fromJson(JSONObject obj) throws JSONException {
        SeafRepoEncrypt repo = new SeafRepoEncrypt();
        repo.magic = obj.optString("magic");
        repo.permission = obj.getString("permission");
        repo.encrypted = obj.getBoolean("encrypted");
        repo.encVersion = obj.optInt("enc_version");
        repo.mtime = obj.getLong("mtime");
        repo.owner = obj.getString("owner");
        repo.id = obj.getString("id");
        repo.size = obj.getLong("size");
        repo.name = obj.getString("name");
        repo.root = obj.getString("root");
        repo.encKey = obj.optString("random_key");
        repo.isGroupRepo = obj.getString("type").equals("grepo");
        repo.isPersonalRepo = obj.getString("type").equals("repo");
        repo.isSharedRepo = obj.getString("type").equals("srepo");
        return repo;
    }

    public SeafRepoEncrypt() {
    }

    public boolean canLocalDecrypt() {
        return encrypted
                && encVersion == 2
                && !TextUtils.isEmpty(magic)
                && SettingsManager.instance().isEncryptEnabled();
    }

}
