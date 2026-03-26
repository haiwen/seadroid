package com.seafile.seadroid2.framework.datastore;

import android.text.TextUtils;

import com.seafile.seadroid2.framework.datastore.sp.SettingsManager;
import com.seafile.seadroid2.framework.util.SLogs;

import org.json.JSONArray;
import org.json.JSONException;

import java.util.ArrayList;
import java.util.List;

/**
 * Manages persistence and lookup of folder sync rules.
 * Rules are stored as a JSON array in SharedPreferences.
 */
public class SyncRuleManager {
    private static final String TAG = "SyncRuleManager";
    private static final Object sLock = new Object();

    public static List<SyncRule> getAll() {
        synchronized (sLock) {
            String json = DataStoreManager.getCommonSharePreference()
                    .readString(SettingsManager.SHARED_PREF_SYNC_RULES);
            return parseRules(json);
        }
    }

    public static void saveAll(List<SyncRule> rules) {
        synchronized (sLock) {
            try {
                JSONArray arr = new JSONArray();
                for (SyncRule rule : rules) {
                    arr.put(rule.toJson());
                }
                DataStoreManager.getCommonSharePreference()
                        .writeString(
                                SettingsManager.SHARED_PREF_SYNC_RULES,
                                arr.toString());
            } catch (JSONException e) {
                SLogs.e(TAG, "Failed to save sync rules: "
                        + e.getMessage());
            }
        }
    }

    public static void add(SyncRule rule) {
        synchronized (sLock) {
            List<SyncRule> rules = getAll();
            rules.add(rule);
            saveAll(rules);
        }
    }

    public static void remove(String ruleId) {
        synchronized (sLock) {
            List<SyncRule> rules = getAll();
            rules.removeIf(r -> r.id.equals(ruleId));
            saveAll(rules);
        }
        SyncStateManager.clearForRule(ruleId);
        SyncExclusionManager.clearForRule(ruleId);
    }

    public static void update(SyncRule updated) {
        synchronized (sLock) {
            List<SyncRule> rules = getAll();
            for (int i = 0; i < rules.size(); i++) {
                if (rules.get(i).id.equals(updated.id)) {
                    rules.set(i, updated);
                    break;
                }
            }
            saveAll(rules);
        }
    }

    /** Find a sync rule by its ID. Returns null if not found. */
    public static SyncRule findById(String ruleId) {
        List<SyncRule> rules = getAll();
        for (SyncRule rule : rules) {
            if (rule.id.equals(ruleId)) {
                return rule;
            }
        }
        return null;
    }

    /**
     * Find the best matching sync rule for a given file download.
     * If multiple rules match, returns the most specific one (longest remotePath).
     */
    public static SyncRule findMatch(String repoId, String filePath) {
        List<SyncRule> rules = getAll();
        SyncRule best = null;
        for (SyncRule rule : rules) {
            if (rule.matches(repoId, filePath)) {
                if (best == null
                        || (rule.remotePath != null
                        && rule.remotePath.length()
                        > best.remotePath.length())) {
                    best = rule;
                }
            }
        }
        return best;
    }

    private static List<SyncRule> parseRules(String json) {
        List<SyncRule> rules = new ArrayList<>();
        if (TextUtils.isEmpty(json)) {
            return rules;
        }
        try {
            JSONArray arr = new JSONArray(json);
            for (int i = 0; i < arr.length(); i++) {
                rules.add(SyncRule.fromJson(arr.getJSONObject(i)));
            }
        } catch (JSONException e) {
            SLogs.e(TAG, "Failed to parse sync rules: " + e.getMessage());
        }
        return rules;
    }
}
