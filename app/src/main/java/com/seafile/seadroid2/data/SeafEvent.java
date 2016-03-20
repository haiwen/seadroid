package com.seafile.seadroid2.data;

import android.util.Log;

import com.seafile.seadroid2.R;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

/**
 * Seafile event entity
 */
public class SeafEvent implements SeafItem {
    public static final String DEBUG_TAG = SeafItem.class.getSimpleName();

    private String repo_id;
    private String author;
    private String nick;
    private long time;
    private String etype;
    private String repo_name;
    private String desc;
    private String commit_id;
    private String date;
    private String name;
    private String time_relative;
    private String converted_cmmt_desc;
    private String avatar;
    private boolean repo_encrypted;
    private boolean more_files;

    public static SeafEvent fromJson(JSONObject obj) {
        SeafEvent event = new SeafEvent();
        try {
            event.repo_id = obj.getString("repo_id");
            event.author = obj.getString("author");
            event.nick = obj.getString("nick");
            event.etype = obj.getString("etype");
            event.repo_name = obj.getString("repo_name");
            event.desc = obj.getString("desc");
            event.time = obj.getLong("time");
            event.avatar = obj.getString("avatar");
            event.commit_id = obj.getString("commit_id");
            event.date = obj.getString("date");
            event.name = obj.getString("name");
            event.time_relative = obj.getString("time_relative");
            event.converted_cmmt_desc = obj.getString("converted_cmmt_desc");
            event.repo_encrypted = obj.getBoolean("repo_encrypted");
            event.more_files = obj.getBoolean("more_files");
            return event;
        } catch (JSONException e) {
            Log.d(DEBUG_TAG, e.getMessage());
            return null;
        }
    }

    public void setRepo_id(String repo_id) {
        this.repo_id = repo_id;
    }

    public void setAuthor(String author) {
        this.author = author;
    }

    public void setNick(String nick) {
        this.nick = nick;
    }

    public void setTime(int time) {
        this.time = time;
    }

    public void setEtype(String etype) {
        this.etype = etype;
    }

    public void setRepo_name(String repo_name) {
        this.repo_name = repo_name;
    }

    public void setDesc(String desc) {
        this.desc = desc;
    }

    public String getRepo_id() {
        return repo_id;
    }

    public String getAuthor() {
        return author;
    }

    public String getNick() {
        return nick;
    }

    public long getTime() {
        return time;
    }

    public String getEtype() {
        return etype;
    }

    public String getRepo_name() {
        return repo_name;
    }

    public String getDesc() {
        return desc;
    }

    @Override
    public String getTitle() {
        return desc;
    }

    @Override
    public String getSubtitle() {
        return nick;
    }

    @Override
    public int getIcon() {
        return R.drawable.repo;
    }

    public void setTime(long time) {
        this.time = time;
    }

    public String getCommit_id() {
        return commit_id;
    }

    public void setCommit_id(String commit_id) {
        this.commit_id = commit_id;
    }

    public String getDate() {
        return date;
    }

    public void setDate(String date) {
        this.date = date;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getTime_relative() {
        return time_relative;
    }

    public void setTime_relative(String time_relative) {
        this.time_relative = time_relative;
    }

    public String getConverted_cmmt_desc() {
        return converted_cmmt_desc;
    }

    public void setConverted_cmmt_desc(String converted_cmmt_desc) {
        this.converted_cmmt_desc = converted_cmmt_desc;
    }

    public String getAvatar() {
        return avatar;
    }

    public void setAvatar(String avatar) {
        this.avatar = avatar;
    }

    public boolean isRepo_encrypted() {
        return repo_encrypted;
    }

    public void setRepo_encrypted(boolean repo_encrypted) {
        this.repo_encrypted = repo_encrypted;
    }

    public boolean isMore_files() {
        return more_files;
    }

    public void setMore_files(boolean more_files) {
        this.more_files = more_files;
    }

}
