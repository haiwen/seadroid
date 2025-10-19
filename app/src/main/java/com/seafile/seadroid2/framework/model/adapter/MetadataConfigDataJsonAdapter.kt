package com.seafile.seadroid2.framework.model.adapter

import android.text.TextUtils
import com.blankj.utilcode.util.CollectionUtils
import com.google.gson.Gson
import com.google.gson.JsonArray
import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import com.google.gson.JsonObject
import com.google.gson.JsonParseException
import com.google.gson.JsonSerializationContext
import com.google.gson.JsonSerializer
import com.google.gson.reflect.TypeToken
import com.seafile.seadroid2.framework.model.sdoc.MetadataConfigDataModel
import com.seafile.seadroid2.framework.model.sdoc.OptionsTagModel
import java.lang.reflect.Type

class MetadataConfigDataJsonAdapter : JsonDeserializer<List<MetadataConfigDataModel>>, JsonSerializer<List<MetadataConfigDataModel>> {
    override fun serialize(
        list: List<MetadataConfigDataModel>?,
        typeOfSrc: Type,
        context: JsonSerializationContext
    ): JsonElement? {
        if (CollectionUtils.isEmpty(list)) {
            return null
        }

        val jsonList = JsonArray()
        list?.forEach { item ->
            val jsonObject = JsonObject()
            jsonObject.addProperty("format", item.format)
            jsonObject.addProperty("geo_format", item.geo_format)

            if (!CollectionUtils.isEmpty(item.options)) {
                val optionsJsonArray = JsonArray()
                item.options?.forEach { option: OptionsTagModel ->
                    val optionObject = JsonObject()
                    optionObject.addProperty("borderColor", option.borderColor)
                    optionObject.addProperty("color", option.color)
                    optionObject.addProperty("id", option.id)
                    optionObject.addProperty("name", option.name)
                    optionObject.addProperty("textColor", option.textColor)
                    optionsJsonArray.add(optionObject)
                }
                jsonObject.add("options", optionsJsonArray)
            }

            jsonList.add(jsonObject)
        }
        return jsonList
    }

    override fun deserialize(
        json: JsonElement?,
        typeOfT: Type,
        context: JsonDeserializationContext
    ): List<MetadataConfigDataModel> {
        val values: MutableList<MetadataConfigDataModel> = ArrayList()

        if (json == null || json.isJsonNull) {
            return values
        }

        val jsonString = json.toString()
        if (TextUtils.isEmpty(jsonString) || jsonString == "{}" || jsonString == "[]") {
            return values
        }

        val gson = Gson()
        if (json.isJsonObject) {
            val value = gson.fromJson(jsonString, MetadataConfigDataModel::class.java)
            values.add(value)
        } else if (json.isJsonArray) {
            val typeToken = object : TypeToken<ArrayList<MetadataConfigDataModel>>() {}.type
            val list: List<MetadataConfigDataModel> = gson.fromJson(jsonString, typeToken)
            values.addAll(list)
        }
        return values
    }
}
