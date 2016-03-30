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
    public List<String> addedFiles;
    public List<String> deletedFiles;
    public List<String> modifiedFiles;
    public List<String> renamedFiles;
    public List<String> addedDirs;
    public List<String> deletedDirs;

    public CommitDetails() {
        addedFiles = Lists.newArrayList();
        deletedFiles = Lists.newArrayList();
        modifiedFiles = Lists.newArrayList();
        renamedFiles = Lists.newArrayList();
        addedDirs = Lists.newArrayList();
        deletedDirs = Lists.newArrayList();
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
        processFileList(details.addedFiles, addedFiles);
        processFileList(details.deletedFiles, deletedFiles);
        processFileList(details.modifiedFiles, modifiedFiles);
        processFileList(details.renamedFiles, renamedFiles);
        processFileList(details.addedDirs, addedDirs);
        processFileList(details.deletedDirs, deletedDirs);

        return details;
    }

    private static void processFileList(List<String> list, JSONArray jsonArray) throws JSONException {
        if (jsonArray == null) return;

        for (int i = 0; i < jsonArray.length(); i++) {
            list.add(jsonArray.getString(i));
        }
    }

    public List<String> getDeletedDirs() {
        return deletedDirs;
    }

    public void setDeletedDirs(List<String> deletedDirs) {
        this.deletedDirs = deletedDirs;
    }

    public List<String> getRenamedFiles() {
        return renamedFiles;
    }

    public void setRenamedFiles(List<String> renamedFiles) {
        this.renamedFiles = renamedFiles;
    }

    public List<String> getModifiedFiles() {
        return modifiedFiles;
    }

    public void setModifiedFiles(List<String> modifiedFiles) {
        this.modifiedFiles = modifiedFiles;
    }

    public List<String> getAddedFiles() {
        return addedFiles;
    }

    public void setAddedFiles(List<String> addedFiles) {
        this.addedFiles = addedFiles;
    }

    public List<String> getDeletedFiles() {
        return deletedFiles;
    }

    public void setDeletedFiles(List<String> deletedFiles) {
        this.deletedFiles = deletedFiles;
    }

    public List<String> getAddedDirs() {
        return addedDirs;
    }

    public void setAddedDirs(List<String> addedDirs) {
        this.addedDirs = addedDirs;
    }

}
