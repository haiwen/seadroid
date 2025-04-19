package com.seafile.seadroid2.preferences;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.GsonUtils;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.data.db.entities.RepoModel;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.ContextModel;
import com.seafile.seadroid2.framework.datastore.DataStoreKeys;
import com.seafile.seadroid2.framework.util.Utils;

import java.lang.reflect.Type;
import java.util.Stack;

public class ContextStackPreferenceHelper {
    private static final String STACK_KEY = DataStoreKeys.KEY_NAV_CONTEXT_STACK;

    public static void save(NavContext context) {
        if (context == null) {
            return;
        }

        Stack<ContextModel> stack = new Stack<>();
        for (BaseModel baseModel : context.getNavStack()) {
            ContextModel contextModel = new ContextModel();

            if (baseModel instanceof RepoModel e) {
                contextModel.repo_id = e.repo_id;
                contextModel.repo_name = e.repo_name;
                contextModel.type = "repo";
                contextModel.full_path = "/";
                contextModel.permission = e.permission;
                contextModel.encrypted = e.encrypted;
            } else if (baseModel instanceof DirentModel e) {
                contextModel.repo_id = e.repo_id;
                contextModel.repo_name = e.repo_name;
                contextModel.type = "dirent";
                contextModel.full_path = e.full_path;
                contextModel.permission = e.permission;
            }
            stack.add(contextModel);
        }

        String json = GsonUtils.toJson(stack);
        Settings.getCommonPreferences().edit().putString(STACK_KEY, json).apply();
    }

    public static void clear() {
        Settings.getCommonPreferences().edit().remove(STACK_KEY).apply();
    }

    private static Stack<ContextModel> getStack() {
        String json = Settings.getCommonPreferences().getString(STACK_KEY, null);
        if (json == null) {
            return new Stack<>();
        }

        Type type = new TypeToken<Stack<ContextModel>>() {
        }.getType();
        return GsonUtils.fromJson(json, type);
    }

    @NonNull
    public static NavContext getNavContextStack() {
        NavContext navContext = new NavContext();

        Stack<ContextModel> stack = getStack();
        if (stack == null || stack.isEmpty()) {
            return navContext;
        }

        for (ContextModel contextModel : stack) {
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
