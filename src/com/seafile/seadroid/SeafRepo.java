package com.seafile.seadroid;

import java.util.Date;

import org.json.JSONException;
import org.json.JSONObject;

/**
 * SeafRepo: A Seafile library
 * @author plt
 */
public class SeafRepo {

    String id;     // repo id
    String name;
    String description;
    String owner;
    Date mtime;    // the last modification time
    
    boolean passwordNeed;
    boolean encrypted;
    
    long    size;
    String  root; // the id of root directory; 

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
    
}
