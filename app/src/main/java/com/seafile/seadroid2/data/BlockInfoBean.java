package com.seafile.seadroid2.data;

import com.seafile.seadroid2.util.Utils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.util.ArrayList;
import java.util.List;


public class BlockInfoBean {
    public List<String> blkIds;
    public String rawblksurl;
    public String commiturl;

    public static BlockInfoBean fromJson(String json) throws JSONException {
        BlockInfoBean bean = new BlockInfoBean();
        JSONObject obj = Utils.parseJsonObject(json);
        bean.rawblksurl = obj.optString("rawblksurl");
        bean.commiturl = obj.optString("commiturl");
        JSONArray jsonArray = obj.optJSONArray("blklist");
        bean.blkIds = new ArrayList<>();
        for (int i = 0; i < jsonArray.length(); i++) {
            bean.blkIds.add(jsonArray.getString(i));
        }
        return bean;
    }

    public BlockInfoBean() {
    }
}
