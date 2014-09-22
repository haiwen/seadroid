package com.seafile.seadroid2.data;

import java.util.Date;

import org.json.JSONException;
import org.json.JSONObject;

import com.seafile.seadroid2.R;

/**
 * SeafRepo: A Seafile library
 * @author plt
 */
public class SeafRepo implements SeafItem {
    public String id;     // repo id
    public String name;
    public String description;
    public String owner;
    public Date mtime;    // the last modification time

    public boolean isGroupRepo;
    public boolean encrypted;
    public String permission;

    public long    size;
    public String  root; // the id of root directory

    static SeafRepo fromJson(JSONObject obj) {
        SeafRepo repo = new SeafRepo();
        try {
            repo.id = obj.getString("id");
            repo.name = obj.getString("name");
            repo.description = obj.getString("desc");
            repo.owner = obj.getString("owner");
            repo.permission = obj.getString("permission");
            long mt = obj.getLong("mtime");
            repo.mtime = new Date(mt);
            repo.encrypted = obj.getBoolean("encrypted");
            repo.root = obj.getString("root");
            repo.size = obj.getLong("size");
            if (obj.getString("type").equals("grepo")) {
                repo.isGroupRepo = true;
            } else
                repo.isGroupRepo = false;
            return repo;
        } catch (JSONException e) {
            return null;
        }
    }

    public SeafRepo() {
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
        return description;
    }

    @Override
    public int getIcon() {
        if (encrypted)
            return R.drawable.repo_encrypted;
        if (!hasWritePermission())
            return R.drawable.repo_readonly;
        
        return R.drawable.repo;
    }

    public boolean hasWritePermission() {
        return permission.indexOf('w') != -1;
    }
}
