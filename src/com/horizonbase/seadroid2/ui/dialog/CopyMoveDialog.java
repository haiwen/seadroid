package com.horizonbase.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.horizonbase.seadroid2.account.Account;
import com.horizonbase.seadroid2.data.DataManager;
import com.horizonbase.seadroid2.ui.CopyMoveContext;
import com.horizonbase.seadroid2.R;

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
        if (ctx.batch) {
            setTitle(str);
        } else
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
