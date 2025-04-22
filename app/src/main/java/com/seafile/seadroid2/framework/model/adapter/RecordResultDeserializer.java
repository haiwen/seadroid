package com.seafile.seadroid2.framework.model.adapter;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.seafile.seadroid2.framework.model.sdoc.RecordResultModel;

import java.lang.reflect.Type;

public class RecordResultDeserializer implements JsonDeserializer<RecordResultModel> {
    @Override
    public RecordResultModel deserialize(JsonElement json, Type typeOfT, JsonDeserializationContext context) throws JsonParseException {
        return null;
//        RecordResultModel result = new Gson().fromJson(json, RecordResultModel.class);
//        JsonObject jsonObject = json.getAsJsonObject();
//
//        Map<String, Object> dynamicFields = new HashMap<>();
//
//        for (Map.Entry<String, JsonElement> entry : jsonObject.entrySet()) {
//            String key = entry.getKey();
//
//            if (!isFixedField(key)) {
//                dynamicFields.put(key, context.deserialize(entry.getValue(), Object.class));
//            }
//        }
//
//        result.dynamicFields = dynamicFields;
//        return result;
    }

    private boolean isFixedField(String key) {
        return key.startsWith("_");
    }
}
