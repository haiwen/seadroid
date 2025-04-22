package com.seafile.seadroid2.context;

import com.seafile.seadroid2.enums.OpType;
import com.seafile.seadroid2.framework.db.entities.DirentModel;

import java.util.List;

public class CopyMoveContext {

    public OpType op;

    public List<DirentModel> dirents;
    public String srcRepoId;
    public String srcRepoName;
    public String srcDir;

    public boolean isdir;

    public String dstRepoId;
    public String dstRepoName;
    public String dstDir;

    /**
     * Constructor for multiple files operations
     */
    public CopyMoveContext(String srcRepoId, String srcRepoName, String srcDir, List<DirentModel> dirents, OpType op) {
        this.srcRepoId = srcRepoId;
        this.srcRepoName = srcRepoName;
        this.srcDir = srcDir;
        this.dirents = dirents;
        this.op = op;
    }

    public void setDest(String dstRepoId, String dstDir, String dstRepoName) {
        this.dstRepoId = dstRepoId;
        this.dstDir = dstDir;
        this.dstRepoName = dstRepoName;
    }

    public boolean isCopy() {
        return op == OpType.COPY;
    }

    public boolean isMove() {
        return op == OpType.MOVE;
    }

    /**
     * Avoid copy/move a folder into its subfolder E.g. situations like:
     * <p>
     * srcDir: /dirX
     * dstDir: /dirX/dirY
     */
    public boolean checkCopyMoveToSubfolder() {
        if (isdir && srcRepoId.equals(dstRepoId)) {
            return !dstDir.startsWith(srcDir);
        }
        return true;
    }
}
