package com.seafile.seadroid2.data;

import android.util.Log;

import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.DateFormatType;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONObject;

import java.util.Date;

public class SeafStarredFile implements SeafItem {
    public enum FileType {DIR, FILE}

    private static final String DEBUG_TAG = "SeafStarredFile";

    private String repo_id;
    private String repo_name;
    private boolean repo_encrypted;
    private boolean is_dir;

    private long mtime;
    private String path;
    private String obj_name;
    private String user_email;
    private String user_name;
    private String user_contact_email;
    private String encoded_thumbnail_src;
    private long size;
    private FileType type;

    static SeafStarredFile fromJson(JSONObject obj) {
        SeafStarredFile starredFile = new SeafStarredFile();
        try {
            starredFile.repo_id = obj.optString("repo_id");
            starredFile.repo_name = obj.getString("repo_name");
            starredFile.path = obj.getString("path");
            starredFile.obj_name = obj.getString("obj_name");
            starredFile.repo_encrypted = obj.getBoolean("repo_encrypted");
            starredFile.user_email = obj.getString("user_email");
            starredFile.user_name = obj.getString("user_name");
            starredFile.user_contact_email = obj.getString("user_contact_email");

            starredFile.size = obj.optLong("size");

            String t = obj.getString("mtime");
            starredFile.mtime = convertLastModified2Mtime(t);

            if (obj.has("encoded_thumbnail_src")) {
                starredFile.encoded_thumbnail_src = obj.getString("encoded_thumbnail_src");
            }

            boolean type = obj.optBoolean("is_dir");
            if (!type) {
                starredFile.type = FileType.FILE;
            } else {
                starredFile.type = FileType.DIR;
            }
            return starredFile;
        } catch (Exception e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return null;
        }
    }

    private static long convertLastModified2Mtime(String t) {
        Date date = TimeUtils.string2Date(t, DateFormatType.DATE_XXX);
        return TimeUtils.date2Millis(date);
    }

    public String getRepoName() {
        return repo_name;
    }

    public void setRepoName(String repoName) {
        this.repo_name = repoName;
    }

    public long getMtime() {
        return 0;
    }

    public String getObjName() {
        return obj_name;
    }

    public long getSize() {
        return size;
    }

    public boolean isDir() {
        return (type == FileType.DIR);
    }

    public boolean isRepoEncrypted() {
        return repo_encrypted;
    }

    public String getRepoID() {
        return repo_id;
    }

    public String getPath() {
        return path;
    }

    @Override
    public String getTitle() {
        return getObjName();
    }

    @Override
    public String getSubtitle() {
        return repo_name + " " + Utils.translateCommitTime(mtime);
    }

    @Override
    public int getIcon() {
        if (isDir())
            return R.drawable.folder;
        return Utils.getFileIcon(getTitle());
    }
}
