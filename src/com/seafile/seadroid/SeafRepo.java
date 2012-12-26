package com.seafile.seadroid;

import java.util.Date;

import org.json.JSONException;
import org.json.JSONObject;

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
    
    public boolean passwordNeed;
    public boolean encrypted;
    
    public long    size;
    public String  root; // the id of root directory; 

    static SeafRepo fromJson(JSONObject obj) {
        SeafRepo repo = new SeafRepo();
        try {
            repo.id = obj.getString("id");
            repo.name = obj.getString("name");
            repo.description = obj.getString("desc");
            repo.owner = obj.getString("owner");
            long mt = obj.getLong("mtime");
            repo.mtime = new Date(mt);
            repo.encrypted = obj.getBoolean("encrypted");
            repo.root = obj.getString("root");
            repo.size = obj.getLong("size");
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

    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public String getSubtitle() {
        return description;
    }
    
}
