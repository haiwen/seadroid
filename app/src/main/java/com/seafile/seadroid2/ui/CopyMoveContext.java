package com.seafile.seadroid2.ui;

import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.util.Utils;

import java.util.List;

public class CopyMoveContext {
    public enum OP {
        COPY,
        MOVE
    }
    public OP op;

    public List<SeafDirent> dirents;
    public String srcRepoId;
    public String srcRepoName;
    public String srcDir;
    public String srcFn;
    public boolean isdir;
    /** flag to mark multiple selection & operations */
    public boolean batch;

    public String dstRepoId;
    public String dstDir;

    /**
     * Constructor for a single file operations
     *
     * @param srcRepoId
     * @param srcRepoName
     * @param srcDir
     * @param srcFn
     * @param isdir
     * @param op
     */
    public CopyMoveContext(String srcRepoId, String srcRepoName, String srcDir, String srcFn, boolean isdir, OP op) {
        this.srcRepoId = srcRepoId;
        this.srcRepoName = srcRepoName;
        this.srcDir = srcDir;
        this.srcFn = srcFn;
        this.isdir = isdir;
        this.op = op;
        this.batch = false;
    }

    /**
     * Constructor for multiple files operations
     *
     * @param srcRepoId
     * @param srcRepoName
     * @param srcDir
     * @param dirents
     * @param op
     */
    public CopyMoveContext(String srcRepoId, String srcRepoName, String srcDir, List<SeafDirent> dirents, OP op) {
        this.srcRepoId = srcRepoId;
        this.srcRepoName = srcRepoName;
        this.srcDir = srcDir;
        this.dirents = dirents;
        this.batch = true;
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
