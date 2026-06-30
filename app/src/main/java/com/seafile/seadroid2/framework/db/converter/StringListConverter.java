package com.seafile.seadroid2.framework.db.converter;

import android.text.TextUtils;

import androidx.room.TypeConverter;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

public class StringListConverter {

    private static final Gson GSON = new Gson();

    private static final Type TYPE =
            new TypeToken<List<String>>() {}.getType();

    @TypeConverter
    public static String fromList(List<String> value) {
        if (value == null) {
            return null;
        }
        return GSON.toJson(value);
    }

    @TypeConverter
    public static List<String> toList(String value) {
        if (TextUtils.isEmpty(value)) {
            return new ArrayList<>();
        }
        return GSON.fromJson(value, TYPE);
    }
}