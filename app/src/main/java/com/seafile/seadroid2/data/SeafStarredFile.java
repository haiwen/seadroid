package com.seafile.seadroid2.data;

import android.util.Log;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.SystemSwitchUtils;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONObject;

public class SeafStarredFile implements SeafItem {
    public enum FileType { DIR, FILE };
    private static final String DEBUG_TAG = "SeafStarredFile";

    private String repoID;
    private long mtime;
    private String path;
    private String obj_name;
    private boolean repo_encrypted;
    private FileType type;
    private long size;    // size of file, 0 if type is dir
    private String repoName;

    static SeafStarredFile fromJson(JSONObject obj) {
        SeafStarredFile starredFile = new SeafStarredFile();
        try {
            starredFile.repoID = obj.optString("repo_id");
            starredFile.mtime = SystemSwitchUtils.parseISODateTime(obj.optString("mtime"));
            starredFile.path = obj.optString("path");
            starredFile.obj_name = obj.optString("obj_name");
            starredFile.size = obj.optLong("size");
            starredFile.repo_encrypted = obj.optBoolean("repo_encrypted");
            boolean type = obj.optBoolean("is_dir");
            starredFile.repoName = obj.optString("repo_name");
            if (!type) {
                starredFile.type = FileType.FILE;
            } else
                starredFile.type = FileType.DIR;
            return starredFile;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return null;
        }
    }

    public String getRepoName() {
        return repoName;
    }

    public void setRepoName(String repoName) {
        this.repoName = repoName;
    }

    public long getSize() {
        return size;
    }

    public long getMtime() {
        return mtime;
    }

    public String getObj_name() {
        return obj_name;
    }

    public boolean isDir() {
        return (type == FileType.DIR);
    }

    public boolean isRepo_encrypted() {
        return repo_encrypted;
    }

    public String getRepoID() {
        return repoID;
    }

    public String getPath() {
        return path;
    }

    @Override
    public String getTitle() {
//        return path.substring(path.lastIndexOf('/') + 1);
        return getObj_name();
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
