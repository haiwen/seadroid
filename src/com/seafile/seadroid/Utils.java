package com.seafile.seadroid;

import org.json.*;

class Utils {

    public static JSONObject parseJsonObjectInArray0(String json) {
        try {
            JSONArray array = (JSONArray) new JSONTokener(json).nextValue();
            return array.getJSONObject(0);
        } catch (Exception e) {
            return null;
        }
    }
    
    public static JSONObject parseJsonObject(String json) throws JSONException {
    	return (JSONObject) new JSONTokener(json).nextValue();
    }

}
