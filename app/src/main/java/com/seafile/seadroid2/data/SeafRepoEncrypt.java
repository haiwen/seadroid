package com.seafile.seadroid2.data;

import android.text.TextUtils;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONException;
import org.json.JSONObject;


/**
 * @Description:  [A encrypt bean]
 * @Author:       [Saud]
 * @CreateDate:   [2018/3/31 下午5:32]
 * @UpDate:       [2018/3/31 下午5:32]
 * @Version:      [v1.0]
 *
 */
public class SeafRepoEncrypt implements SeafItem {
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
    public long    size;
    public String  root; // the id of root directory
    private String modifierContactEmail;
    private String modifierName;
    private String modifierEmail;
    private int fileCount;

    static SeafRepoEncrypt fromJson(JSONObject obj) throws JSONException{
        SeafRepoEncrypt repo = new SeafRepoEncrypt();
        repo.magic = obj.optString("magic");
        repo.permission = obj.getString("permission");
        repo.encrypted = obj.getBoolean("encrypted");
        repo.encVersion = obj.optInt("enc_version");
        repo.mtime = obj.getLong("mtime");
        repo.owner = obj.getString("owner");
        repo.modifierContactEmail=obj.getString("modifier_contact_email");
        repo.id = obj.getString("id");
        repo.modifierName=obj.getString("modifier_name");
        repo.size = obj.getLong("size");
        repo.modifierEmail=obj.getString("modifier_email");
        repo.name = obj.getString("name");
        repo.root = obj.getString("root");
        repo.fileCount = obj.optInt("file_count");
        repo.encKey = obj.optString("random_key");
        repo.isGroupRepo = obj.getString("type").equals("grepo");
        repo.isPersonalRepo = obj.getString("type").equals("repo");
        repo.isSharedRepo = obj.getString("type").equals("srepo");
        return repo;
    }

    public SeafRepoEncrypt() {
    }

    public String getID() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getRootDirID() {
        return root;
    }



    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public String getSubtitle() {
        return Utils.translateCommitTime(mtime * 1000);
    }

    @Override
    public int getIcon() {
        if (encrypted)
            return R.drawable.repo_encrypted;
        if (!hasWritePermission())
            return R.drawable.repo_readonly;

        return R.drawable.repo;
    }

    public boolean canLocalDecrypt() {
        return encrypted
                && encVersion == 2
                && !TextUtils.isEmpty(magic)
                && SettingsManager.instance().isEncryptEnabled();
    }

    public boolean hasWritePermission() {
        return permission.indexOf('w') != -1;
    }


}
