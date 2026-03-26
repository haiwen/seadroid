package com.seafile.seadroid2.framework.datastore;

import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;

import org.json.JSONArray;
import org.json.JSONException;

import java.util.HashSet;
import java.util.Set;

/**
 * Manages per-file exclusions for each sync rule. When a user unchecks
 * a file in the per-file toggle UI, its relative path is stored here.
 * Excluded files are not downloaded and are deleted locally if present.
 * New server files are NOT in the exclusion set (i.e. they are auto-included).
 */
public class SyncExclusionManager {
    private static final Object sLock = new Object();
    private static final String KEY_PREFIX = SettingsManager.PKG + ".sync_exclusions.";

    /** Returns the set of excluded relative paths for the given rule. */
    public static Set<String> getExcludedFiles(String ruleId) {
        synchronized (sLock) {
            String json = DataStoreManager.getCommonSharePreference()
                    .readString(KEY_PREFIX + ruleId);
            return parseSet(json);
        }
    }

    /** Overwrite the full exclusion set for a rule. */
    public static void setExcludedFiles(String ruleId, Set<String> paths) {
        synchronized (sLock) {
            JSONArray arr = new JSONArray();
            for (String p : paths) {
                arr.put(p);
            }
            DataStoreManager.getCommonSharePreference()
                    .writeString(KEY_PREFIX + ruleId, arr.toString());
        }
    }

    /** Add a single file to the exclusion set. */
    public static void excludeFile(String ruleId, String relativePath) {
        synchronized (sLock) {
            Set<String> set = getExcludedFilesInternal(ruleId);
            set.add(relativePath);
            persist(ruleId, set);
        }
    }

    /** Remove a single file from the exclusion set (re-include it). */
    public static void includeFile(String ruleId, String relativePath) {
        synchronized (sLock) {
            Set<String> set = getExcludedFilesInternal(ruleId);
            set.remove(relativePath);
            persist(ruleId, set);
        }
    }

    /** Clear exclusions when a sync rule is deleted. */
    public static void clearForRule(String ruleId) {
        synchronized (sLock) {
            DataStoreManager.getCommonSharePreference()
                    .writeString(KEY_PREFIX + ruleId, "");
        }
    }

    // Internal helpers (caller must hold sLock)

    private static Set<String> getExcludedFilesInternal(String ruleId) {
        String json = DataStoreManager.getCommonSharePreference()
                .readString(KEY_PREFIX + ruleId);
        return parseSet(json);
    }

    private static void persist(String ruleId, Set<String> set) {
        JSONArray arr = new JSONArray();
        for (String p : set) {
            arr.put(p);
        }
        DataStoreManager.getCommonSharePreference()
                .writeString(KEY_PREFIX + ruleId, arr.toString());
    }

    private static Set<String> parseSet(String json) {
        Set<String> result = new HashSet<>();
        if (json == null || json.isEmpty()) {
            return result;
        }
        try {
            JSONArray arr = new JSONArray(json);
            for (int i = 0; i < arr.length(); i++) {
                result.add(arr.getString(i));
            }
        } catch (JSONException ignored) {
        }
        return result;
    }
}
