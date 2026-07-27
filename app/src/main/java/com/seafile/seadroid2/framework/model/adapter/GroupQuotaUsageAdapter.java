package com.seafile.seadroid2.framework.model.adapter;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonNull;
import com.google.gson.JsonParseException;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import java.lang.reflect.Type;

public class GroupQuotaUsageAdapter implements JsonSerializer<Long>, JsonDeserializer<Long> {
    @Override
    public JsonElement serialize(Long src, Type typeOfSrc, JsonSerializationContext context) {
        return src == null ? JsonNull.INSTANCE : new JsonPrimitive(src);
    }

    @Override
    public Long deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        if (json == null || json.isJsonNull()) {
            return 0L;
        }

        try {
            if (json.isJsonPrimitive()) {
                JsonPrimitive primitive = json.getAsJsonPrimitive();
                if (primitive.isNumber()) {
                    return primitive.getAsLong();
                }
                if (primitive.isString()) {
                    String value = primitive.getAsString();
                    if (value == null || value.trim().isEmpty()) {
                        return 0L;
                    }
                    return Long.parseLong(value.trim());
                }
            }
        } catch (NumberFormatException | UnsupportedOperationException e) {
            return 0L;
        }

        return 0L;
    }
}
