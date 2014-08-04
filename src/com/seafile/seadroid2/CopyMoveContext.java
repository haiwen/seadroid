package com.seafile.seadroid2;

public class CopyMoveContext {
    enum OP {
        COPY,
        MOVE
    }
    public OP op;

    public String srcRepoId;
    public String srcRepoName;
    public String srcDir;
    public String srcFn;
    public boolean isdir;

    public String dstRepoId;
    public String dstDir;

    public CopyMoveContext(String srcRepoId, String srcRepoName, String srcDir, String srcFn, boolean isdir, OP op) {
        this.srcRepoId = srcRepoId;
        this.srcRepoName = srcRepoName;
        this.srcDir = srcDir;
        this.srcFn = srcFn;
        this.isdir = isdir;
        this.op = op;
    }

    public void setDest(String dstRepoId, String dstDir) {
        this.dstRepoId = dstRepoId;
        this.dstDir = dstDir;
    }

    public boolean isCopy() {
        return op == OP.COPY;
    }

    public boolean isMove() {
        return op == OP.MOVE;
    }
}
