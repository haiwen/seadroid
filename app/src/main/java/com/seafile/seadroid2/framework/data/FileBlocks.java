package com.seafile.seadroid2.framework.data;

import androidx.annotation.NonNull;

import com.blankj.utilcode.util.CollectionUtils;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Seafile file blocks
 */
public class FileBlocks implements Serializable {
    public static final String DEBUG_TAG = "FileBlocks";

    private List<Block> blocks = CollectionUtils.newArrayList();

    private int enc_version;
    private boolean encrypted;
    private List<String> blklist;
    private String file_id;

    public String getFileId() {
        return file_id;
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

    public Block getBlock(@NonNull String blkId) {
        if (CollectionUtils.isEmpty(blocks)) {
            return null;
        }

        for (Block block : blocks) {
            if (blkId.equals(block.blockId)) {
                return block;
            }
        }
        return null;
    }

    public List<Block> getBlocks() {
        if (!CollectionUtils.isEmpty(blocks)) {
            return blocks;
        }

        if (blklist == null || blklist.isEmpty())
            return blocks;

        final List<String> blkIds = new ArrayList<>(blklist);

        for (String blkid : blkIds) {
            Block block = new Block(blkid, null, 0, 0);
            blocks.add(block);
        }

        return blocks;
    }

}
