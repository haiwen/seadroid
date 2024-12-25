package com.seafile.seadroid2.preferences;

import com.blankj.utilcode.util.GsonUtils;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.framework.data.model.ContextModel;
import com.seafile.seadroid2.framework.datastore.DataStoreKeys;

import java.lang.reflect.Type;
import java.util.Stack;

public class ContextStackPreferenceHelper {
    private static final String STACK_KEY = DataStoreKeys.KEY_NAV_CONTEXT_STACK;

    // 保存 Stack<BaseModel> 到 SharedPreferences
    public static void saveStack(Stack<ContextModel> stack) {
        String json = GsonUtils.toJson(stack); // 将 Stack 转换为 JSON 字符串
        Settings.getCommonPreferences().edit().putString(STACK_KEY, json).apply();
    }

    public static void clearStack() {
        Settings.getCommonPreferences().edit().remove(STACK_KEY).apply();
    }

    // 从 SharedPreferences 中获取 Stack<BaseModel>
    public static Stack<ContextModel> getStack() {
        String json = Settings.getCommonPreferences().getString(STACK_KEY, null);
        if (json == null) {
            return new Stack<>(); // 返回空栈
        }

        Type type = new TypeToken<Stack<ContextModel>>() {
        }.getType();
        return GsonUtils.fromJson(json, type); // 从 JSON 恢复 Stack
    }
}
