package com.seafile.seadroid2.framework.datastore;

import android.text.TextUtils;

import org.json.JSONException;
import org.json.JSONObject;

import java.util.Locale;
import java.util.UUID;

/**
 * Represents a sync mapping from a Seafile repo/folder to a local Android folder.
 */
public class SyncRule {
    public String id;
    public String repoId;
    public String repoName;
    public String remotePath;  // path within repo, "/" for root
    public String localUri;    // SAF tree URI string
    public String localName;   // display name for the local folder
    public boolean enabled;

    /** Maximum file size in bytes to sync. 0 = unlimited. */
    public long maxFileSize;

    /**
     * Comma-separated list of file extensions (without leading dot).
     * e.g. "mp3,flac,wav". Empty string = no filter.
     */
    public String extensionFilter;

    /** "include" means only sync matching extensions; "exclude" means skip them. */
    public String filterMode; // "include" or "exclude"

    public SyncRule() {
        this.id = UUID.randomUUID().toString();
        this.enabled = true;
        this.maxFileSize = 0;
        this.extensionFilter = "";
        this.filterMode = "include";
    }

    public SyncRule(String repoId, String repoName,
            String remotePath, String localUri, String localName) {
        this.id = UUID.randomUUID().toString();
        this.repoId = repoId;
        this.repoName = repoName;
        this.remotePath = remotePath;
        this.localUri = localUri;
        this.localName = localName;
        this.enabled = true;
        this.maxFileSize = 0;
        this.extensionFilter = "";
        this.filterMode = "include";
    }

    public JSONObject toJson() throws JSONException {
        JSONObject obj = new JSONObject();
        obj.put("id", id);
        obj.put("repoId", repoId);
        obj.put("repoName", repoName);
        obj.put("remotePath", remotePath);
        obj.put("localUri", localUri);
        obj.put("localName", localName);
        obj.put("enabled", enabled);
        obj.put("maxFileSize", maxFileSize);
        obj.put("extensionFilter", extensionFilter);
        obj.put("filterMode", filterMode);
        return obj;
    }

    public static SyncRule fromJson(JSONObject obj) throws JSONException {
        SyncRule rule = new SyncRule();
        rule.id = obj.getString("id");
        rule.repoId = obj.getString("repoId");
        rule.repoName = obj.getString("repoName");
        rule.remotePath = obj.getString("remotePath");
        rule.localUri = obj.getString("localUri");
        rule.localName = obj.optString("localName", "");
        rule.enabled = obj.optBoolean("enabled", true);
        rule.maxFileSize = obj.optLong("maxFileSize", 0);
        rule.extensionFilter = obj.optString("extensionFilter", "");
        rule.filterMode = obj.optString("filterMode", "include");
        return rule;
    }

    /**
     * Check if a file should be excluded based on rule-level filters
     * (max size and extension filter). Does NOT check per-file exclusions.
     *
     * @param fileName the file name (e.g. "song.mp3")
     * @param fileSize the file size in bytes
     * @return true if the file should be excluded from sync
     */
    public boolean isFilteredOut(String fileName, long fileSize) {
        // Check size limit
        if (maxFileSize > 0 && fileSize > maxFileSize) {
            return true;
        }

        // Check extension filter
        if (!TextUtils.isEmpty(extensionFilter) && fileName != null) {
            String ext = getFileExtension(fileName);
            String[] allowed = extensionFilter.toLowerCase(Locale.US).split(",");
            boolean matchesFilter = false;
            for (String a : allowed) {
                if (a.trim().equals(ext)) {
                    matchesFilter = true;
                    break;
                }
            }
            if ("include".equals(filterMode)) {
                // Include mode: only sync matching extensions
                return !matchesFilter;
            } else {
                // Exclude mode: skip matching extensions
                return matchesFilter;
            }
        }
        return false;
    }

    private static String getFileExtension(String fileName) {
        int dot = fileName.lastIndexOf('.');
        if (dot >= 0 && dot < fileName.length() - 1) {
            return fileName.substring(dot + 1).toLowerCase(Locale.US);
        }
        return "";
    }

    /**
     * Check if a given repo/path falls within this sync rule's scope.
     * A file matches if it's in the same repo and under the rule's remote path.
     */
    public boolean matches(String fileRepoId, String filePath) {
        if (!enabled || repoId == null || fileRepoId == null
                || remotePath == null) {
            return false;
        }
        if (!repoId.equals(fileRepoId)) {
            return false;
        }
        if ("/".equals(remotePath)) {
            return true;
        }
        String normalizedRemote = remotePath.endsWith("/") ? remotePath : remotePath + "/";
        return filePath.startsWith(normalizedRemote) || filePath.equals(remotePath);
    }

    /**
     * Returns the relative path of the file within this sync rule's scope.
     * For example, if remotePath is "/Documents/" and filePath is "/Documents/work/report.pdf",
     * this returns "work/report.pdf".
     */
    public String getRelativePath(String filePath) {
        if ("/".equals(remotePath)) {
            return filePath.startsWith("/") ? filePath.substring(1) : filePath;
        }
        String normalizedRemote = remotePath.endsWith("/") ? remotePath : remotePath + "/";
        if (filePath.startsWith(normalizedRemote)) {
            return filePath.substring(normalizedRemote.length());
        }
        // File is at the root of the sync path
        int lastSlash = filePath.lastIndexOf('/');
        if (lastSlash >= 0) {
            return filePath.substring(lastSlash + 1);
        }
        return filePath;
    }

    public String getDisplaySummary() {
        String remote = repoName;
        if (remotePath != null && !"/".equals(remotePath)) {
            remote += remotePath;
        }
        return remote + " → " + (localName != null && !localName.isEmpty() ? localName : "...");
    }
}
