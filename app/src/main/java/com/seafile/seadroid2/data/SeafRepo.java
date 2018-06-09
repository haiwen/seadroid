package com.seafile.seadroid2.data;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.util.PinyinUtils;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Comparator;

/**
 * SeafRepo: A Seafile library
 * @author plt
 */
public class SeafRepo implements SeafItem {
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
    public long    size;
    public String  root; // the id of root directory

    static SeafRepo fromJson(JSONObject obj) throws JSONException{
        SeafRepo repo = new SeafRepo();
        repo.id = obj.getString("id");
        repo.name = obj.getString("name");
        repo.owner = obj.getString("owner");
        repo.permission = obj.getString("permission");
        repo.mtime = obj.getLong("mtime");
        repo.encrypted = obj.getBoolean("encrypted");
        repo.root = obj.getString("root");
        repo.size = obj.getLong("size");
        repo.isGroupRepo = obj.getString("type").equals("grepo");
        repo.isPersonalRepo = obj.getString("type").equals("repo");
        repo.isSharedRepo = obj.getString("type").equals("srepo");
        repo.magic = obj.optString("magic");
        repo.encKey = obj.optString("random_key");
        return repo;
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
        return encrypted && SettingsManager.instance().isEncryptEnabled();
    }

    public boolean hasWritePermission() {
        return permission.indexOf('w') != -1;
    }

    /**
     * Repository last modified time comparator class
     */
    public static class RepoLastMTimeComparator implements Comparator<SeafRepo> {

        @Override
        public int compare(SeafRepo itemA, SeafRepo itemB) {
            return (int) (itemA.mtime - itemB.mtime);
        }
    }

    /**
     * Repository name comparator class
     */
    public static class RepoNameComparator implements Comparator<SeafRepo> {

        @Override
        public int compare(SeafRepo itemA, SeafRepo itemB) {
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
