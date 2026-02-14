package com.seafile.seadroid2.context;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.GsonUtils;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.ContextModel;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.preferences.Settings;

import java.lang.reflect.Type;
import java.util.Stack;

public class ContextStackPreferenceHelper {
    public static void save(NavContext context, String key) {
        if (context == null) {
            return;
        }

        Stack<ContextModel> stack = new Stack<>();
        for (BaseModel baseModel : context.getNavStack()) {
            ContextModel contextModel = new ContextModel();
//            if (baseModel instanceof Account a) {
//                contextModel.type = "account";
//                contextModel.email = a.email;
//                contextModel.server = a.server;
//                contextModel.name = a.name;
//
//            } else
            if (baseModel instanceof RepoModel e) {
                contextModel.type = "repo";
                contextModel.repo_id = e.repo_id;
                contextModel.repo_name = e.repo_name;
                contextModel.full_path = "/";
                contextModel.permission = e.permission;
                contextModel.encrypted = e.encrypted;

            } else if (baseModel instanceof DirentModel e) {
                contextModel.type = "dirent";
                contextModel.repo_id = e.repo_id;
                contextModel.repo_name = e.repo_name;
                contextModel.full_path = e.full_path;
                contextModel.permission = e.permission;

            }
            stack.add(contextModel);
        }

        String json = GsonUtils.toJson(stack);
        Settings.getCommonPreferences().edit().putString(key, json).apply();
    }

    public static void clear(String key) {
        Settings.getCommonPreferences().edit().remove(key).apply();
    }

    private static Stack<ContextModel> getStack(String key) {
        String json = Settings.getCommonPreferences().getString(key, null);
        if (json == null) {
            return new Stack<>();
        }

        Type type = new TypeToken<Stack<ContextModel>>() {
        }.getType();
        return GsonUtils.fromJson(json, type);
    }

    @NonNull
    public static NavContext getNavContextStack(String key) {
        NavContext navContext = new NavContext();

        Stack<ContextModel> stack = getStack(key);
        if (stack == null || stack.isEmpty()) {
            return navContext;
        }

        for (ContextModel contextModel : stack) {
//            if (contextModel.type.equals("account")) {
//                Account account = new Account();
//                account.setServer(contextModel.server);
//                account.setEmail(contextModel.email);
//                account.setName(contextModel.name);
//
//                navContext.push(account);
//            } else
            if (contextModel.type.equals("repo")) {
                RepoModel repoModel = new RepoModel();
                repoModel.repo_id = contextModel.repo_id;
                repoModel.repo_name = contextModel.repo_name;
                repoModel.permission = contextModel.permission;
                repoModel.encrypted = contextModel.encrypted;

                navContext.push(repoModel);
            } else if (contextModel.type.equals("dirent")) {
                DirentModel direntModel = new DirentModel();
                direntModel.repo_id = contextModel.repo_id;
                direntModel.repo_name = contextModel.repo_name;
                direntModel.full_path = contextModel.full_path;
                direntModel.parent_dir = Utils.getParentPath(direntModel.full_path);
                direntModel.name = Utils.getFileNameFromPath(contextModel.full_path);
                direntModel.uid = direntModel.getUID();
                direntModel.permission = contextModel.permission;

                navContext.push(direntModel);
            }
        }
        return navContext;
    }
}
