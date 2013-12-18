package com.seafile.seadroid2.data;

import org.json.JSONException;
import org.json.JSONObject;

import android.util.Log;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.Utils;


public class SeafStarredFile implements SeafItem {

    public enum FileType { DIR, FILE };

    public String repoID;
    public long mtime;
    public String path;
    public FileType type;
    public long size;    // size of file, 0 if type is dir
    
    
    static SeafStarredFile fromJson(JSONObject obj) {
        SeafStarredFile starredFile = new SeafStarredFile();
        try {
            starredFile.repoID = obj.getString("repo");
            starredFile.mtime = obj.getLong("mtime");
            starredFile.path = obj.getString("path");
            boolean type = obj.getBoolean("dir");
            if (!type) {
                starredFile.type = FileType.FILE;
                starredFile.size = obj.getLong("size");
            } else
                starredFile.type = FileType.DIR;
            return starredFile;
        } catch (JSONException e) {
            Log.d("SeafStarredFile", e.getMessage());
            return null;
        }
    }
    
    public boolean isDir() {
        return (type == FileType.DIR);
    }
    
    @Override
    public String getTitle() {
        return path.substring(path.lastIndexOf('/') + 1);
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
        return Utils.getFileIcon(getTitle());
    }

}
