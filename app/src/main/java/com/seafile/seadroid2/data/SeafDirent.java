package com.seafile.seadroid2.data;

import android.util.Log;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.util.PinyinUtils;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;
import java.util.Comparator;

public class SeafDirent implements SeafItem, Serializable {
    public static final long serialVersionUID = 0L;
    private static final String DEBUG_TAG = "SeafDirent";

    public enum DirentType {DIR, FILE}

    public String permission;
    public String id;
    public DirentType type;
    public String name;
    public long size;    // size of file, 0 if type is dir
    public long mtime;   // last modified timestamp
    public boolean starred;
    public String parent_dir;
    public String dir_id;

    //lock
    public boolean is_locked;
    public boolean locked_by_me;
    public long lock_time;
    public String lock_owner;
    public String lock_owner_name;
    public String lock_owner_contact_email;
    public String modifier_email;
    public String modifier_name;
    public String modifier_contact_email;

    static SeafDirent fromJson(String dir_id,JSONObject obj) {
        SeafDirent dirent = new SeafDirent();
        try {
            dirent.dir_id = dir_id;
            dirent.id = obj.optString("id");
            dirent.name = obj.optString("name");
            dirent.mtime = obj.optLong("mtime");
            dirent.permission = obj.optString("permission");
            dirent.parent_dir = obj.optString("parent_dir");
            dirent.starred = obj.optBoolean("starred");

            String type = obj.getString("type");
            if (type.equals("file")) {
                dirent.type = DirentType.FILE;
                dirent.size = obj.optLong("size");
                dirent.modifier_contact_email = obj.optString("modifier_contact_email");
                dirent.modifier_name = obj.optString("modifier_name");
                dirent.modifier_email = obj.optString("modifier_email");
                dirent.is_locked = obj.optBoolean("is_locked");
                dirent.lock_time = obj.optLong("lock_time");
                dirent.locked_by_me = obj.optBoolean("locked_by_me");
                dirent.lock_owner = obj.optString("lock_owner");
                dirent.lock_owner_contact_email = obj.optString("lock_owner_contact_email");
                dirent.lock_owner_name = obj.optString("lock_owner_name");
            } else
                dirent.type = DirentType.DIR;
            return dirent;
        } catch (JSONException e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return null;
        }
    }

    public boolean isStarred() {
        return starred;
    }

    public void setStarred(boolean starred) {
        this.starred = starred;
    }

    public boolean isDir() {
        return (type == DirentType.DIR);
    }

    public long getFileSize() {
        return size;
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
        if (isDir()) {
            if (!hasWritePermission()) {
                return R.drawable.folder_read_only;
            } else {
                return R.drawable.folder;
            }
        }
        return Utils.getFileIcon(name);
    }

    public boolean hasWritePermission() {
        return permission.indexOf('w') != -1;
    }

    /**
     * SeafDirent last modified time comparator class
     */
    public static class DirentLastMTimeComparator implements Comparator<SeafDirent> {

        @Override
        public int compare(SeafDirent itemA, SeafDirent itemB) {
            return (int) (itemA.mtime - itemB.mtime);
        }
    }

    /**
     * SeafDirent name comparator class
     */
    public static class DirentNameComparator implements Comparator<SeafDirent> {

        @Override
        public int compare(SeafDirent itemA, SeafDirent itemB) {
            // get the first character unicode from each file name
            int unicodeA = itemA.name.codePointAt(0);
            int unicodeB = itemB.name.codePointAt(0);

            String strA, strB;

            // both are Chinese words
            if ((19968 < unicodeA && unicodeA < 40869) && (19968 < unicodeB && unicodeB < 40869)) {
                strA = PinyinUtils.toPinyin(SeadroidApplication.getAppContext(), itemA.name).toLowerCase();
                strB = PinyinUtils.toPinyin(SeadroidApplication.getAppContext(), itemB.name).toLowerCase();
            } else if ((19968 < unicodeA && unicodeA < 40869) && !(19968 < unicodeB && unicodeB < 40869)) {
                // itemA is Chinese and itemB is English
                return 1;
            } else if (!(19968 < unicodeA && unicodeA < 40869) && (19968 < unicodeB && unicodeB < 40869)) {
                // itemA is English and itemB is Chinese
                return -1;
            } else {
                // both are English words
                strA = itemA.name.toLowerCase();
                strB = itemB.name.toLowerCase();
            }

            return strA.compareTo(strB);
        }
    }
}