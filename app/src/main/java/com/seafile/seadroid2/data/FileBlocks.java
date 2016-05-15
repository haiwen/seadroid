package com.seafile.seadroid2.data;

import com.google.common.collect.Lists;

import org.json.JSONException;
import org.json.JSONObject;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * Seafile file blocks
 */
public class FileBlocks implements Serializable {
    public static final String DEBUG_TAG = FileBlocks.class.getSimpleName();

    public ArrayList<byte[]> chunks;
    public ArrayList<String> blockids;
    public ArrayList<String> blockpaths;

    public int encVersion;
    public String blklist;
    public String fileID;

    public FileBlocks() {
        chunks = new ArrayList<>();
        blockids = new ArrayList<>();
        blockpaths = new ArrayList<>();
    }

    static FileBlocks fromJson(JSONObject obj) throws JSONException {
        FileBlocks blocks = new FileBlocks();
        blocks.blklist = obj.optString("blklist");
        blocks.fileID = obj.optString("file_id");
        blocks.blockids = getBlockIds(blocks.blklist);
        blocks.encVersion = obj.optInt("enc_version");
        return blocks;
    }

    private static ArrayList<String> getBlockIds(String blklist) {
        final List<String> blkIds = Arrays.asList(blklist.split("\\s*,\\s*"));

        ArrayList<String> ids = Lists.newArrayList();
        for (String block : blkIds) {
            final String substring = block.substring(block.indexOf("\"") + 1, block.lastIndexOf("\""));
            ids.add(substring);
        }

        return ids;
    }

}
