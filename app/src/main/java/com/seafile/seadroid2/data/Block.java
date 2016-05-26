package com.seafile.seadroid2.data;

import java.io.Serializable;
import java.util.Arrays;

/**
 * Block entity
 */
public class Block implements Serializable {

    public String blockId;
    public String path;
    public long size;
    public long finished;

    public Block(String blockId, String path, long size, long finished) {
        this.blockId = blockId;
        this.path = path;
        this.size = size;
        this.finished = finished;
    }

    public String getPath() {
        return path;
    }

    public void setPath(String path) {
        this.path = path;
    }

    public float getSize() {
        return size;
    }

    public void setSize(long size) {
        this.size = size;
    }

    public float getFinished() {
        return finished;
    }

    public void setFinished(long finished) {
        this.finished = finished;
    }

    public String getBlockId() {

        return blockId;
    }

    public void setBlockId(String blockId) {
        this.blockId = blockId;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Block block = (Block) o;

        if (size != block.size) return false;
        if (finished != block.finished) return false;
        if (!blockId.equals(block.blockId)) return false;
        return path != null ? path.equals(block.path) : block.path == null;

    }

    @Override
    public int hashCode() {
        int result = blockId.hashCode();
        result = 31 * result + (path != null ? path.hashCode() : 0);
        result = 31 * result + (int) (size ^ (size >>> 32));
        result = 31 * result + (int) (finished ^ (finished >>> 32));
        return result;
    }
}
