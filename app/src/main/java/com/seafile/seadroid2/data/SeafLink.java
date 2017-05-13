package com.seafile.seadroid2.data;

import org.json.JSONObject;

/**
 * Seafile share link bean
 */
public class SeafLink  {

    public static final String DEBUG_TAG = SeafLink.class.getSimpleName();


    private String username;
    private String repo_id;
    private String ctime;
    private String expire;
    private String token;
    private String count;
    private String link;
    private String name;
    private String path;
    private String isDir;
    private String isExpired;
    private String repoName;


    public static SeafLink fromJson(JSONObject obj) {
        SeafLink link = new SeafLink();
        link.username = obj.optString("username");
        link.repo_id = obj.optString("repo_id");
        link.ctime = obj.optString("ctime");
        link.expire = obj.optString("expire_date");
        link.token = obj.optString("token");
        link.count = obj.optString("view_cnt");
        link.link = obj.optString("link");
        link.name = obj.optString("obj_name");
        link.path = obj.optString("path");
        link.isDir = obj.optString("is_dir");
        link.isExpired = obj.optString("is_expired");
        link.repoName = obj.optString("repo_name");
        return link;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getRepo_id() {
        return repo_id;
    }

    public void setRepo_id(String repo_id) {
        this.repo_id = repo_id;
    }

    public String getCtime() {
        return ctime;
    }

    public void setCtime(String ctime) {
        this.ctime = ctime;
    }

    public String getExpire() {
        return expire;
    }

    public void setExpire(String expire) {
        this.expire = expire;
    }

    public String getToken() {
        return token;
    }

    public void setToken(String token) {
        this.token = token;
    }

    public String getCount() {
        return count;
    }

    public void setCount(String count) {
        this.count = count;
    }

    public String getLink() {
        return link;
    }

    public void setLink(String link) {
        this.link = link;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public String getIsDir() {
        return isDir;
    }

    public void setIsDir(String isDir) {
        this.isDir = isDir;
    }

    public String getIsExpired() {
        return isExpired;
    }

    public void setIsExpired(String isExpired) {
        this.isExpired = isExpired;
    }

    public String getRepoName() {
        return repoName;
    }

    public void setRepoName(String repoName) {
        this.repoName = repoName;
    }

    @Override
    public String toString() {
        return "SeafLink{" +
                "username='" + username + '\'' +
                ", repo_id='" + repo_id + '\'' +
                ", ctime='" + ctime + '\'' +
                ", expire='" + expire + '\'' +
                ", token='" + token + '\'' +
                ", count='" + count + '\'' +
                ", link='" + link + '\'' +
                ", name='" + name + '\'' +
                ", path='" + path + '\'' +
                ", isDir='" + isDir + '\'' +
                ", isExpired='" + isExpired + '\'' +
                ", repoName='" + repoName + '\'' +
                '}';
    }
}
