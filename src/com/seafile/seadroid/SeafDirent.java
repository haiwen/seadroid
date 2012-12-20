package com.seafile.seadroid;

import java.util.Date;

import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

public class SeafDirent {

    public enum DirentType { DIR, FILE };
    
    public String id;
    public DirentType type;
    public String name;
    public String mime; // like "text/x-c"
    public long size;    // size of file, 0 if type is dir
    
    
    static SeafDirent fromJson(JSONObject obj) {
        SeafDirent dirent = new SeafDirent();
        try {
            dirent.id = obj.getString("id");
            dirent.name = obj.getString("name");
            String type = obj.getString("type");
            if (type.equals("file"))
                dirent.type = DirentType.FILE;
            else
                dirent.type = DirentType.DIR;
            dirent.size = obj.getLong("size");
            return dirent;
        } catch (JSONException e) {
            Log.d("SeafDirent", e.getMessage());
            return null;
        }
    }
    
    public boolean isDir() {
        return (type == DirentType.DIR);
    }
    
}
