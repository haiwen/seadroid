package com.seafile.seadroid2.data;

import java.io.Serializable;

import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.Utils;

public class SeafDirent implements SeafItem, Serializable {
    public static final long serialVersionUID = 0L;
    private static final String DEBUG_TAG = "SeafDirent";

    public enum DirentType { DIR, FILE };

    public String id;
    public DirentType type;
    public String name;
    public long size;    // size of file, 0 if type is dir
    public long mtime;   // last modified timestamp


    static SeafDirent fromJson(JSONObject obj) {
        SeafDirent dirent = new SeafDirent();
        try {
            dirent.id = obj.getString("id");
            dirent.name = obj.getString("name");
            dirent.mtime = obj.getLong("mtime");
            String type = obj.getString("type");
            if (type.equals("file")) {
                dirent.type = DirentType.FILE;
                dirent.size = obj.getLong("size");
            } else
                dirent.type = DirentType.DIR;
            return dirent;
        } catch (JSONException e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return null;
        }
    }

    public boolean isDir() {
        return (type == DirentType.DIR);
    }

    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public String getSubtitle() {
        String timestamp = Utils.translateCommitTime(mtime * 1000);
        if (isDir())
            return timestamp;
        return Utils.readableFileSize(size) + ", " + timestamp;
    }


    @Override
    public int getIcon() {
        if (isDir())
            return R.drawable.folder;
        return Utils.getFileIcon(name);
    }
}
