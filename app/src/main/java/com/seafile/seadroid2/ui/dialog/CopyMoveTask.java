package com.seafile.seadroid2.ui.dialog;

import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafDirent;
import com.seafile.seadroid2.ui.CopyMoveContext;

/**
 * AsyncTask for copying/moving files
 */
public class CopyMoveTask extends TaskDialog.Task {
    public static final String DEBUG_TAG = "CopyMoveTask";
    private DataManager dataManager;
    private CopyMoveContext ctx;

    public CopyMoveTask(CopyMoveContext ctx, DataManager dataManager) {
        this.ctx = ctx;
        this.dataManager = dataManager;
    }

    @Override
    protected void runTask() {

        if (ctx.batch) {
            String fileNames = "";
            for (SeafDirent dirent : ctx.dirents) {
                fileNames += ":" + dirent.name;
            }

            fileNames = fileNames.substring(1, fileNames.length());

            try {
                if (ctx.isCopy()) {
                    dataManager.copy(ctx.srcRepoId, ctx.srcDir, fileNames, ctx.dstRepoId, ctx.dstDir);
                } else if (ctx.isMove()) {
                    dataManager.move(ctx.srcRepoId, ctx.srcDir, fileNames, ctx.dstRepoId, ctx.dstDir, true);
                }
            } catch (SeafException e) {
                setTaskException(e);
            }
            return;
        }

        try {
            if (ctx.isCopy()) {
                dataManager.copy(ctx.srcRepoId, ctx.srcDir, ctx.srcFn, ctx.dstRepoId, ctx.dstDir);
            } else if (ctx.isMove()) {
                dataManager.move(ctx.srcRepoId, ctx.srcDir, ctx.srcFn, ctx.dstRepoId, ctx.dstDir, false);
            }
        } catch (SeafException e) {
            setTaskException(e);
        }
    }

}
