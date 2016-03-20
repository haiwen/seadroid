package com.seafile.seadroid2.data;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.List;

/**
 * Commit details for activities history changes
 */
public class CommitDetails {
    public List<String> added_files;
    public List<String> deleted_files;
    public List<String> modified_files;
    public List<String> renamed_files;
    public List<String> added_dirs;
    public List<String> deleted_dirs;

    public CommitDetails() {
        added_files = Lists.newArrayList();
        deleted_files = Lists.newArrayList();
        modified_files = Lists.newArrayList();
        renamed_files = Lists.newArrayList();
        added_dirs = Lists.newArrayList();
        deleted_dirs = Lists.newArrayList();
    }

    public static CommitDetails fromJson(String json) throws JSONException {
        final JSONObject jsonObject = Utils.parseJsonObject(json);
        final JSONArray addedFiles = jsonObject.optJSONArray("added_files");
        final JSONArray deletedFiles = jsonObject.optJSONArray("deleted_files");
        final JSONArray modifiedFiles = jsonObject.optJSONArray("modified_files");
        final JSONArray renamedFiles = jsonObject.optJSONArray("renamed_files");
        final JSONArray addedDirs = jsonObject.optJSONArray("added_dirs");
        final JSONArray deletedDirs = jsonObject.optJSONArray("deleted_dirs");

        CommitDetails details = new CommitDetails();
        // the order matters, it affects the adapter for rending ui layout
        processFileList(details.added_files, addedFiles);
        processFileList(details.deleted_files, deletedFiles);
        processFileList(details.modified_files, modifiedFiles);
        processFileList(details.renamed_files, renamedFiles);
        processFileList(details.added_dirs, addedDirs);
        processFileList(details.deleted_dirs, deletedDirs);

        return details;
    }

    private static void processFileList(List<String> list, JSONArray jsonArray) throws JSONException {
        if (jsonArray == null) return;

        for (int i = 0; i < jsonArray.length(); i++) {
            list.add(jsonArray.getString(i));
        }
    }

    public List<String> getDeleted_dirs() {
        return deleted_dirs;
    }

    public void setDeleted_dirs(List<String> deleted_dirs) {
        this.deleted_dirs = deleted_dirs;
    }

    public List<String> getRenamed_files() {
        return renamed_files;
    }

    public void setRenamed_files(List<String> renamed_files) {
        this.renamed_files = renamed_files;
    }

    public List<String> getModified_files() {
        return modified_files;
    }

    public void setModified_files(List<String> modified_files) {
        this.modified_files = modified_files;
    }

    public List<String> getAdded_files() {
        return added_files;
    }

    public void setAdded_files(List<String> added_files) {
        this.added_files = added_files;
    }

    public List<String> getDeleted_files() {
        return deleted_files;
    }

    public void setDeleted_files(List<String> deleted_files) {
        this.deleted_files = deleted_files;
    }

    public List<String> getAdded_dirs() {
        return added_dirs;
    }

    public void setAdded_dirs(List<String> added_dirs) {
        this.added_dirs = added_dirs;
    }

}
