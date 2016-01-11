package com.seafile.seadroid2.ui.dialog;

import android.app.Dialog;
import android.os.Bundle;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.TextView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountManager;
import com.seafile.seadroid2.data.DataManager;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.CopyMoveContext;
import com.seafile.seadroid2.util.Utils;

public class CopyMoveDialog extends TaskDialog {
    private DataManager dataManager;
    private Account account;
    private TextView tvMessage;
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
        tvMessage = (TextView) view.findViewById(R.id.copy_move_file_hint);
        return view;
    }

    @Override
    protected void onDialogCreated(Dialog dialog) {
        int strTitleId;
        int strMsgId;
        if (ctx.isdir) {
            strTitleId = ctx.isCopy() ? R.string.copy_folder_ing : R.string.move_folder_ing;
            strMsgId = ctx.isCopy() ? R.string.copy_file_from : R.string.move_file_from;
        } else {
            strTitleId = ctx.isCopy() ? R.string.copy_file_ing : R.string.move_file_ing;
            strMsgId = ctx.isCopy() ? R.string.copy_file_from : R.string.move_file_from;
        }
        String strTitle = getActivity().getString(strTitleId);
        String strMsg = getActivity().getString(strMsgId);

        String srcDir = Utils.pathJoin(ctx.srcRepoName, ctx.srcDir);
        String srcDirPath = Utils.removeLastPathSeperator(srcDir);

        String dstDirPath = null;
        AccountManager manager = new AccountManager(this.getActivity());
        SeafRepo repo = new DataManager(manager.getCurrentAccount()).getCachedRepoByID(ctx.dstRepoId);
        if (repo != null) {
            String dstPath = Utils.pathJoin(repo.name, ctx.dstDir);
            dstDirPath = Utils.removeLastPathSeperator(dstPath);
        }

        if (ctx.batch) {
            dialog.setTitle(strTitle);
            if (srcDirPath != null && dstDirPath != null) {
                tvMessage.setText(String.format(strMsg, srcDirPath, dstDirPath));
            } else {
                tvMessage.setText(strTitle);
            }
        } else {
            String srcFilePath = Utils.pathJoin(ctx.srcRepoName, ctx.srcDir, ctx.srcFn);
            dialog.setTitle(strTitle);
            if (srcFilePath != null && dstDirPath != null) {
                tvMessage.setText(String.format(strMsg, srcFilePath, dstDirPath));
            } else {
                tvMessage.setText(Utils.pathJoin(strTitle, ctx.srcFn));
            }
        }
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
