package com.seafile.seadroid2.util.sp;

import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.GsonUtils;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.ui.folder_backup.RepoConfig;

import java.lang.reflect.Type;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

public class FolderBackupConfigSPs {

    private static final String FOLDER_BACKUP_REPO_CONFIG = "sp_folder_backup_config_list";
    public static final String FOLDER_BACKUP_PATHS = "folder_backup_paths";
    private static final String FOLDER_BACKUP_ACCOUNT_EMAIL = "folder_backup_account_email";

    private static final String FOLDER_BACKUP_PATHS_PREFIX = "folder_backup_paths_";


    @Deprecated
    public static String getBackupPaths() {
        return SPs.getString(FOLDER_BACKUP_PATHS, null);
    }

    public static void saveBackupPathsByCurrentAccount(String path) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        if (account == null) {
            return;
        }

        String key = FOLDER_BACKUP_PATHS_PREFIX + account.getSignature();
        SPs.put(key, path);
    }

    public static List<String> getBackupPathListByCurrentAccount() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return null;
        }

        String json = getBackupPathsBySpecialAccount(account);

        if (TextUtils.isEmpty(json) || "[]".equals(json)) {
            return Collections.emptyList();
        }
        
        Type listType = new TypeToken<List<String>>() {
        }.getType();
        return GsonUtils.fromJson(json, listType);
    }

    public static String getBackupPathsByCurrentAccount() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return null;
        }

        return getBackupPathsBySpecialAccount(account);
    }

    public static String getBackupPathsBySpecialAccount(Account account) {
        if (account == null) {
            return null;
        }

        String key = FOLDER_BACKUP_PATHS_PREFIX + account.getSignature();
        String json = SPs.getString(key, null);
        if (TextUtils.isEmpty(json) || "[]".equals(json)) {
            return null;
        }
        return json;
    }

    public static List<String> getBackupPathList() {
        String json = getBackupPaths();
        if (TextUtils.isEmpty(json) || "[]".equals(json)) {
            return Collections.emptyList();
        }
        Type listType = new TypeToken<List<String>>() {
        }.getType();
        return GsonUtils.fromJson(json, listType);
    }

    @Deprecated
    public static void saveBackupEmail(String path) {
        SPs.put(FOLDER_BACKUP_ACCOUNT_EMAIL, path);
    }

    @Deprecated
    public static String getBackupEmail() {
        return SPs.getString(FOLDER_BACKUP_ACCOUNT_EMAIL, null);
    }

    public static RepoConfig getBackupConfigByCurrentAccount() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return null;
        }
        return getBackupConfigByAccount(account.getSignature());
    }

    public static RepoConfig getBackupConfigByAccount(String signature) {

        String s = SPs.getString(FOLDER_BACKUP_REPO_CONFIG, null);
        if (TextUtils.isEmpty(s)) {
            return null;
        }
        Type listType = new TypeToken<List<RepoConfig>>() {
        }.getType();

        List<RepoConfig> list = GsonUtils.fromJson(s, listType);
        Optional<RepoConfig> optional = list.stream().filter(f -> TextUtils.equals(signature, f.getSignature())).findFirst();
        return optional.orElse(null);
    }

    public static void setBackupRepoConfig(RepoConfig repoConfig) {
        String s = SPs.getString(FOLDER_BACKUP_REPO_CONFIG, null);
        List<RepoConfig> list;
        if (TextUtils.isEmpty(s)) {
            list = CollectionUtils.newArrayList(repoConfig);
        } else {
            Type listType = new TypeToken<List<RepoConfig>>() {
            }.getType();

            list = GsonUtils.fromJson(s, listType);
            list.removeIf(f -> TextUtils.equals(f.getSignature(), repoConfig.getSignature()));
            list.add(repoConfig);
        }

        SPs.put(FOLDER_BACKUP_REPO_CONFIG, GsonUtils.toJson(list));
    }
}
