package com.seafile.seadroid2.framework.datastore;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;

import org.json.JSONArray;
import org.json.JSONException;

import java.util.HashSet;
import java.util.Set;

/**
 * Tracks the set of file relative paths that were successfully synced for
 * each sync rule. Used to distinguish "new remote file" from "locally deleted
 * file" during bidirectional sync.
 */
public class SyncStateManager {
    private static final Object sLock = new Object();

    /** Returns the set of relative paths last synced for the given rule. */
    public static Set<String> getSyncedFiles(String ruleId) {
        synchronized (sLock) {
            String key = SettingsManager.SHARED_PREF_SYNC_STATE_PREFIX
                    + ruleId;
            String json = DataStoreManager.getCommonSharePreference()
                    .readString(key);
            return parseSet(json);
        }
    }

    /**
     * Atomically persists the sync state after a sync cycle completes.
     *
     * <p>Because download/upload threads call {@link #addSyncedFile} while
     * the sync worker is building {@code newState}, a plain overwrite would
     * stomp those concurrent writes. This method detects entries added since
     * the cycle started (present in persisted state but absent from the
     * snapshot taken at the start of the cycle) and preserves them.
     *
     * @param ruleId        the sync rule ID
     * @param previousState the snapshot from {@link #getSyncedFiles} at cycle start
     * @param newState      the computed set of currently-synced files
     */
    public static void setSyncedFiles(
            String ruleId, Set<String> previousState, Set<String> newState) {
        synchronized (sLock) {
            String key = SettingsManager.SHARED_PREF_SYNC_STATE_PREFIX
                    + ruleId;
            // Find entries added by concurrent transfers since the cycle
            // started (present now but were NOT in the snapshot)
            String persisted = DataStoreManager.getCommonSharePreference()
                    .readString(key);
            Set<String> current = parseSet(persisted);
            Set<String> concurrentAdds = new HashSet<>(current);
            concurrentAdds.removeAll(previousState);

            // Final state = cycle result + concurrent additions
            Set<String> merged = new HashSet<>(newState);
            merged.addAll(concurrentAdds);

            JSONArray arr = new JSONArray();
            for (String p : merged) {
                arr.put(p);
            }
            DataStoreManager.getCommonSharePreference()
                    .writeString(key, arr.toString());
        }
    }

    /** Removes persisted sync state for a deleted rule. */
    public static void clearForRule(String ruleId) {
        synchronized (sLock) {
            String key = SettingsManager.SHARED_PREF_SYNC_STATE_PREFIX
                    + ruleId;
            DataStoreManager.getCommonSharePreference()
                    .writeString(key, "");
        }
    }

    /**
     * Record a single file as synced. Called when a download completes
     * and the file lands on local disk, so the sync state is populated
     * immediately rather than waiting for the next sync cycle.
     */
    public static void addSyncedFile(String ruleId, String relativePath) {
        synchronized (sLock) {
            String key = SettingsManager.SHARED_PREF_SYNC_STATE_PREFIX
                    + ruleId;
            String json = DataStoreManager.getCommonSharePreference()
                    .readString(key);
            Set<String> files = parseSet(json);
            files.add(relativePath);
            JSONArray arr = new JSONArray();
            for (String p : files) {
                arr.put(p);
            }
            DataStoreManager.getCommonSharePreference()
                    .writeString(key, arr.toString());
        }
    }

    private static Set<String> parseSet(String json) {
        Set<String> result = new HashSet<>();
        if (TextUtils.isEmpty(json)) {
            return result;
        }
        try {
            JSONArray arr = new JSONArray(json);
            for (int i = 0; i < arr.length(); i++) {
                result.add(arr.getString(i));
            }
        } catch (JSONException ignored) {
            // Corrupted data — start fresh
        }
        return result;
    }
}
