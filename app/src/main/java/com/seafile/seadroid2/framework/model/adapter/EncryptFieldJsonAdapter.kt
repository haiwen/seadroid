package com.seafile.seadroid2.framework.model.adapter

import android.text.TextUtils
import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonParseException
import com.google.gson.JsonPrimitive
import com.google.gson.JsonSerializationContext
import com.google.gson.JsonSerializer
import java.lang.reflect.Type

class EncryptFieldJsonAdapter : JsonSerializer<Boolean>, JsonDeserializer<Boolean> {
    override fun serialize(src: Boolean, typeOfSrc: Type, context: JsonSerializationContext): JsonElement {
        return JsonPrimitive(src)
    }

    @Throws(JsonParseException::class)
    override fun deserialize(json: JsonElement?, typeOfT: Type, context: JsonDeserializationContext): Boolean {
        if (json == null || json.isJsonNull) {
            return false
        }

        if (TextUtils.equals(json.toString(), "")) {
            return false
        }

        if (json.isJsonPrimitive) {
            val jsonPrimitive = json.asJsonPrimitive
            return when {
                jsonPrimitive.isNumber -> jsonPrimitive.asInt == 1
                jsonPrimitive.isBoolean -> jsonPrimitive.asBoolean
                jsonPrimitive.isString -> !TextUtils.isEmpty(jsonPrimitive.asString)
                else -> false
            }
        }

        return false
    }
}
