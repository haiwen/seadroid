package com.seafile.seadroid2.preferences;

import com.blankj.utilcode.util.GsonUtils;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.framework.data.model.ContextModel;
import com.seafile.seadroid2.framework.datastore.DataStoreKeys;

import java.lang.reflect.Type;
import java.util.Stack;

public class ContextStackPreferenceHelper {
    private static final String STACK_KEY = DataStoreKeys.KEY_NAV_CONTEXT_STACK;

    // save Stack<BaseModel> to SharedPreferences
    public static void saveStack(Stack<ContextModel> stack) {
        String json = GsonUtils.toJson(stack);
        Settings.getCommonPreferences().edit().putString(STACK_KEY, json).apply();
    }

    public static void clearStack() {
        Settings.getCommonPreferences().edit().remove(STACK_KEY).apply();
    }

    public static Stack<ContextModel> getStack() {
        String json = Settings.getCommonPreferences().getString(STACK_KEY, null);
        if (json == null) {
            return new Stack<>();
        }

        Type type = new TypeToken<Stack<ContextModel>>() {
        }.getType();
        return GsonUtils.fromJson(json, type);
    }
}
