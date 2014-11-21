package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.ui.CopyMoveContext;
import com.seafile.seadroid2.ui.dialog.TaskDialog.Task;

class CopyMoveTask extends TaskDialog.Task {
    private DataManager dataManager;
    private CopyMoveContext ctx;

    public CopyMoveTask(CopyMoveContext ctx, DataManager dataManager) {
        this.ctx = ctx;
        this.dataManager = dataManager;
    }

    @Override
    protected void runTask() {
        try {
            if (ctx.isCopy()) {
                dataManager.copy(ctx.srcRepoId, ctx.srcDir, ctx.srcFn, ctx.dstRepoId, ctx.dstDir, ctx.isdir);
            }
            else {
                dataManager.move(ctx.srcRepoId, ctx.srcDir, ctx.srcFn, ctx.dstRepoId, ctx.dstDir, ctx.isdir);
            }
        } catch (SeafException e) {
            setTaskException(e);
        }
    }
}

public class CopyMoveDialog extends TaskDialog {
    private DataManager dataManager;
    private Account account;

    CopyMoveContext ctx;

    public void init(Account account, CopyMoveContext ctx) {
        this.account = account;
        this.ctx = ctx;
    }

    private DataManager getDataManager() {
        if (dataManager == null) {
            dataManager = new DataManager(account);
        }

        return dataManager;
    }

    @Override
    protected View createDialogContentView(LayoutInflater inflater, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.dialog_copy_move_file, null);

        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        int strId;
        if (ctx.isdir) {
            strId = ctx.isCopy() ? R.string.copy_folder_ing : R.string.move_folder_ing;
        } else {
            strId = ctx.isCopy() ? R.string.copy_file_ing : R.string.move_file_ing;
        }
        String str = getActivity().getString(strId);
        setTitle(str + " " + ctx.srcFn);
        // dialog.setTitle(str + " " + ctx.srcFn);
    }

    @Override
    protected CopyMoveTask prepareTask() {
        CopyMoveTask task = new CopyMoveTask(ctx, getDataManager());
        return task;
    }

    @Override
    protected boolean executeTaskImmediately() {
        return true;
    }
}
