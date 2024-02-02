package com.seafile.seadroid2.data;

import android.text.TextUtils;

import com.blankj.utilcode.util.TimeUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.util.sp.SettingsManager;
import com.seafile.seadroid2.config.DateFormatType;
import com.seafile.seadroid2.util.PinyinUtils;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Comparator;
import java.util.Date;

/**
 * SeafRepo: A Seafile library
 *
 * @author plt
 */
public class SeafRepo implements SeafItem {
    public String type;   //mine\group\shared
    public long group_id;
    public String group_name;

    public String repo_id;     // repo id
    public String repo_name;   //repo_name
    public String owner_name;  //owner_name
    public String owner_email;  //owner_email
    public String owner_contact_email;  //owner_contact_email
    public String modifier_email;
    public String modifier_name;
    public String modifier_contact_email;

    public String last_modified;

    public boolean encrypted; //last_modified
    public long size;
    public boolean starred;

    public String permission;
    public boolean monitored;
    public boolean is_admin;
    public String salt;
    public String status;


    //local used
//    public String root; // the id of root directory

    public long mtime;
    public boolean isGroupRepo;
    public boolean isPersonalRepo;
    public boolean isSharedRepo;

    static SeafRepo fromJson(JSONObject obj) throws JSONException {
        SeafRepo repo = new SeafRepo();
        repo.type = obj.optString("type");

        //group
        repo.group_id = obj.optLong("group_id");
        repo.group_name = obj.optString("group_name");

        //repo
        repo.repo_id = obj.optString("repo_id");
        repo.repo_name = obj.optString("repo_name");

        //owner
        repo.owner_name = obj.optString("owner_name");
        repo.owner_email = obj.optString("owner_email");
        repo.owner_contact_email = obj.optString("owner_contact_email");

        //modifier
        repo.modifier_email = obj.getString("modifier_email");
        repo.modifier_name = obj.getString("modifier_name");
        repo.modifier_contact_email = obj.getString("modifier_contact_email");

        repo.is_admin = obj.optBoolean("is_admin");

        repo.size = obj.optLong("size");
        repo.encrypted = obj.optBoolean("encrypted");
        repo.permission = obj.optString("permission");
        repo.starred = obj.optBoolean("starred");
        repo.monitored = obj.optBoolean("monitored");
        repo.status = obj.optString("status");
        repo.salt = obj.optString("salt");

        repo.last_modified = obj.optString("last_modified");

//        repo.root = obj.optString("root");

        repo.mtime = convertLastModified2Mtime(repo.last_modified);

        repo.isGroupRepo = TextUtils.equals(repo.type, "group");
        repo.isPersonalRepo = TextUtils.equals(repo.type, "mine");
        repo.isSharedRepo = TextUtils.equals(repo.type, "shared");

        return repo;
    }

    public String getRepoId() {
        return repo_id;
    }

    public String getRepoName() {
        return repo_name;
    }

//    public String getRootDirID() {
//        return root;
//    }

    @Override
    public String getTitle() {
        return repo_name;
    }

    @Override
    public String getSubtitle() {
        return Utils.readableFileSize(size) + "  " + Utils.translateCommitTime(mtime);
    }

    private static long convertLastModified2Mtime(String last_modified) {
        Date date = TimeUtils.string2Date(last_modified, DateFormatType.DATE_XXX);
        return TimeUtils.date2Millis(date);
    }

    @Override
    public int getIcon() {
        if (encrypted)
            return R.drawable.repo_encrypted;
        if (!hasWritePermission())
            return R.drawable.repo_readonly;

        return R.drawable.repo;
    }

    public boolean isStarred() {
        return starred;
    }

    public void setStarred(boolean starred) {
        this.starred = starred;
    }

    public boolean canLocalDecrypt() {
        return encrypted && SettingsManager.getInstance().isEncryptEnabled();
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
            int unicodeA = itemA.repo_name.codePointAt(0);
            int unicodeB = itemB.repo_name.codePointAt(0);

            String strA, strB;

            // both are Chinese words
            if ((19968 < unicodeA && unicodeA < 40869) && (19968 < unicodeB && unicodeB < 40869)) {
                strA = PinyinUtils.toPinyin(SeadroidApplication.getAppContext(), itemA.repo_name).toLowerCase();
                strB = PinyinUtils.toPinyin(SeadroidApplication.getAppContext(), itemB.repo_name).toLowerCase();
            } else if ((19968 < unicodeA && unicodeA < 40869) && !(19968 < unicodeB && unicodeB < 40869)) {
                // itemA is Chinese and itemB is English
                return 1;
            } else if (!(19968 < unicodeA && unicodeA < 40869) && (19968 < unicodeB && unicodeB < 40869)) {
                // itemA is English and itemB is Chinese
                return -1;
            } else {
                // both are English words
                strA = itemA.repo_name.toLowerCase();
                strB = itemB.repo_name.toLowerCase();
            }

            return strA.compareTo(strB);
        }
    }
}
