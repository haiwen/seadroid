package com.seafile.seadroid2.ui;

import com.seafile.seadroid2.util.Utils;

public class CopyMoveContext {
    public enum OP {
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

    /**
     * Avoid copy/move a folder into its subfolder E.g. situations like:
     *
     * srcDir: /
     * srcFn: dirX
     * dstDir: /dirX/dirY
     *
     */
    public boolean checkCopyMoveToSubfolder() {
        if (isdir && srcRepoId.equals(dstRepoId)) {
            String srcFolder = Utils.pathJoin(srcDir, srcFn);
            return !dstDir.startsWith(srcFolder);
        }
        return true;
    }
}
