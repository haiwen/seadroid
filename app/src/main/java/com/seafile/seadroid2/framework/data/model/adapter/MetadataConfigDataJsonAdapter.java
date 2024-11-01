package com.seafile.seadroid2.framework.data.model.adapter;

import android.text.TextUtils;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.gson.Gson;
import com.google.gson.JsonArray;
import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import com.google.gson.reflect.TypeToken;
import com.seafile.seadroid2.framework.data.model.sdoc.MetadataConfigDataModel;
import com.seafile.seadroid2.framework.data.model.sdoc.OptionsTagModel;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.List;

public class MetadataConfigDataJsonAdapter implements JsonDeserializer<List<MetadataConfigDataModel>>, JsonSerializer<List<MetadataConfigDataModel>> {
    @Override
    public JsonElement serialize(List<MetadataConfigDataModel> list, Type typeOfSrc, JsonSerializationContext context) {

        if (CollectionUtils.isEmpty(list)) {
            return null;
        }

        JsonArray jsonList = new JsonArray();

        for (MetadataConfigDataModel src : list) {
            JsonObject jsonObject = new JsonObject();
            jsonObject.addProperty("format", src.format);
            jsonObject.addProperty("geo_format", src.geo_format);

            if (!CollectionUtils.isEmpty(src.options)) {
                JsonArray optionsJsonArray = new JsonArray();
                for (OptionsTagModel option : src.options) {
                    JsonObject jsonObject1 = new JsonObject();
                    jsonObject1.addProperty("borderColor", option.borderColor);
                    jsonObject1.addProperty("color", option.color);
                    jsonObject1.addProperty("id", option.id);
                    jsonObject1.addProperty("name", option.name);
                    jsonObject1.addProperty("textColor", option.textColor);
                    optionsJsonArray.add(jsonObject1);
                }

                jsonObject.add("options", optionsJsonArray);
            }


            jsonList.add(jsonObject);
        }

        return jsonList;
    }

    @Override
    public List<MetadataConfigDataModel> deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        List<MetadataConfigDataModel> values = new ArrayList<>();

        if (json == null || json.isJsonNull()) {
            return values;
        }

        String valuesString = json.toString();
        if (TextUtils.isEmpty(valuesString)) {
            return values;
        }

        if ("{}".equals(valuesString) || "[]".equals(valuesString)) {
            return values;
        }

        if (json.isJsonObject()) {
            MetadataConfigDataModel valuesTemp = new Gson().fromJson(valuesString, MetadataConfigDataModel.class);
            values.add(valuesTemp);
        } else if (json.isJsonArray()) {
            List<MetadataConfigDataModel> valuesTemp = new Gson().fromJson(valuesString, new TypeToken<ArrayList<MetadataConfigDataModel>>() {
            }.getType());
            values.addAll(valuesTemp);
        }
        return values;
    }
}