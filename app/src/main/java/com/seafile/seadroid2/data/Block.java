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
    public byte[] chunk;

    public Block(String blockId, String path, long size, long finished, byte[] chunk) {
        this.blockId = blockId;
        this.path = path;
        this.size = size;
        this.finished = finished;
        this.chunk = chunk;
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

    public byte[] getChunk() {
        return chunk;
    }

    public void setChunk(byte[] chunk) {
        this.chunk = chunk;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Block block = (Block) o;

        if (size != block.size) return false;
        if (finished != block.finished) return false;
        if (!blockId.equals(block.blockId)) return false;
        if (path != null ? !path.equals(block.path) : block.path != null) return false;
        return Arrays.equals(chunk, block.chunk);

    }

    @Override
    public int hashCode() {
        int result = blockId.hashCode();
        result = 31 * result + (path != null ? path.hashCode() : 0);
        result = 31 * result + (int) (size ^ (size >>> 32));
        result = 31 * result + (int) (finished ^ (finished >>> 32));
        result = 31 * result + Arrays.hashCode(chunk);
        return result;
    }
}
