package com.seafile.seadroid2.data;

import android.support.annotation.NonNull;

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
    public static final String DEBUG_TAG = "FileBlocks";

    public ArrayList<Block> blocks;

    public int encVersion;
    public String blklist;
    public String fileID;
    private int remaining;

    public FileBlocks() {
        blocks = new ArrayList<>();
    }

    public long getSize() {
        long size = 0L;
        for (Block block : blocks) {
            size += block.size;
        }
        return size;
    }

    public long getFinished() {
        long finished = 0L;
        for (Block block : blocks) {
            finished += block.finished;
        }
        return finished;
    }

    public void decrease() {
        if (remaining >= 1)
            remaining--;
    }

    public boolean remaining() {
        return remaining > 0;
    }

    public Block getBlock(@NonNull String blkId) {
        for (Block block : blocks) {
            if (blkId.equals(block.blockId)) {
                return block;
            }
        }
        return null;
    }

    static FileBlocks fromJson(JSONObject obj) throws JSONException {
        FileBlocks blocks = new FileBlocks();
        blocks.blklist = obj.optString("blklist");
        blocks.fileID = obj.optString("file_id");
        blocks.blocks = getBlockIds(blocks.blklist);
        blocks.remaining = blocks.blocks.size();
        blocks.encVersion = obj.optInt("enc_version");
        return blocks;
    }

    private static ArrayList<Block> getBlockIds(String blklist) {
        final List<String> blkIds = Arrays.asList(blklist.split("\\s*,\\s*"));

        ArrayList<Block> ids = Lists.newArrayList();
        for (String blkid : blkIds) {
            final String substring = blkid.substring(blkid.indexOf("\"") + 1, blkid.lastIndexOf("\""));
            Block block = new Block(substring, null, 0, 0, null);
            ids.add(block);
        }

        return ids;
    }

}
