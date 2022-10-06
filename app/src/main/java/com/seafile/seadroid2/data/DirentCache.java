package com.seafile.seadroid2.data;

import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.util.PinyinUtils;

import org.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.Comparator;
import java.util.List;

public class DirentCache extends CacheItem<SeafDirent>{

    public DirentCache(String name) throws IOException {
        super(name);
    }

    public DirentCache(String name, List<SeafDirent> caches) throws IOException{
        super(name, caches, new SeafDirent.DirentNameComparator());
    }

    protected byte[] toBytes(SeafDirent item){
        JSONObject json = item.toJson();
        String content = json.toString();
        return content.getBytes(StandardCharsets.UTF_8);
    }

    protected SeafDirent fromBytes(byte[] bytes){
        String content = new String(bytes, StandardCharsets.UTF_8);
        try {
            JSONObject json = new JSONObject(content);
            return SeafDirent.fromJson(json);
        }catch (Exception e){
            return null;
        }
    }
}
