package com.seafile.seadroid2.data;

import android.util.Log;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.Utils;

import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

/**
 * Seafile large file
 */
public class SeafLargeFile implements SeafItem, Serializable {
    public static final String DEBUG_TAG = SeafLargeFile.class.getSimpleName();

    public ArrayList<byte[]> chunks;
    public ArrayList<String> blockids;
    public ArrayList<String> blockpaths;

    public String rawblksurl;
    public String commiturl;
    public List<String> missingblocks;
    public int blkidx;

    public enum DirentType { DIR, FILE }

    public DirentType type;
    public String name;
    public long size;    // size of file, 0 if type is dir
    public long mtime;   // last modified timestamp

    public SeafLargeFile() {
        chunks = new ArrayList<>();
        blockids = new ArrayList<>();
        blockpaths = new ArrayList<>();
    }

    static SeafLargeFile fromJson(JSONObject obj) throws JSONException {
        SeafLargeFile file = new SeafLargeFile();
        file.rawblksurl = obj.getString("rawblksurl");
        file.commiturl = obj.getString("commiturl");
        file.missingblocks = parseMissingBlocks(obj.getString("blklist"));
        file.blkidx = 0;
        return file;
    }

    private static List<String> parseMissingBlocks(String json) {
        try {
            JSONArray array = Utils.parseJsonArray(json);
            if (array == null)
                return null;

            ArrayList<String> list = Lists.newArrayList();
            for (int i = 0; i < array.length(); i++) {
                JSONObject obj = array.getJSONObject(i);
                String blkId = obj.getString("id");
                if (blkId != null)
                    list.add(blkId);
            }
            return list;
        } catch (JSONException e) {
            Log.e(DEBUG_TAG, "Could not parse cached dirent", e);
            return null;
        }
    }

    public String getBlockList() {
        if (blockids == null) return null;

        StringBuffer sb = new StringBuffer();
        for (String blkId : blockids) {
            sb.append(blkId);
            sb.append(",");
        }
        return sb.toString();
    }

    public boolean isDir() {
        return (type == DirentType.DIR);
    }

    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public String getSubtitle() {
        String timestamp = Utils.translateCommitTime(mtime * 1000);
        if (isDir())
            return timestamp;
        return Utils.readableFileSize(size) + ", " + timestamp;
    }

    @Override
    public int getIcon() {
        if (isDir())
            return R.drawable.folder;
        return Utils.getFileIcon(name);
    }
}
