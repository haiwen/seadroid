package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.TextUtils;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.CopyMoveViewModel;
import com.seafile.seadroid2.framework.util.Utils;

public class CopyMoveDialogFragment extends RequestCustomDialogFragmentWithVM<CopyMoveViewModel> {
    private CopyMoveContext ctx;


    public static CopyMoveDialogFragment newInstance() {

        Bundle args = new Bundle();

        CopyMoveDialogFragment fragment = new CopyMoveDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    public void initData(CopyMoveContext context) {
        ctx = context;
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_message_textview;
    }

    @Override
    public int getDialogTitleRes() {
        if (ctx != null) {
//            if (ctx.isdir) {
//                return ctx.isCopy() ? R.string.copy_folder_ing : R.string.move_folder_ing;
//            } else {
//                return ctx.isCopy() ? R.string.copy_file_ing : R.string.move_file_ing;
//            }

            return ctx.isCopy() ? R.string.copy_file_ing : R.string.move_file_ing;

        }
        return super.getDialogTitleRes();

    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        int strMsgId;
//        if (ctx.isdir) {
//            strMsgId = ctx.isCopy() ? R.string.copy_file_from : R.string.move_file_from;
//        } else {
//            strMsgId = ctx.isCopy() ? R.string.copy_file_from : R.string.move_file_from;
//        }

        strMsgId = ctx.isCopy() ? R.string.copy_file_from : R.string.move_file_from;

        String strMsg = getString(strMsgId);

        String srcDir = Utils.pathJoin(ctx.srcRepoName, ctx.srcDir);
        String srcDirPath = Utils.removeLastPathSeparator(srcDir);

        String dstPath = Utils.pathJoin(ctx.dstRepoName, ctx.dstDir);
        String dstDirPath = Utils.removeLastPathSeparator(dstPath);

        //set message
        TextView tvMessage = containerView.findViewById(R.id.message_view);
        if (srcDirPath != null && dstDirPath != null) {
            tvMessage.setText(String.format(strMsg, srcDirPath, dstDirPath));
        } else {
            tvMessage.setText(getDialogTitleRes());
        }

    }

    private boolean isOping = false;

    @Override
    public void onResume() {
        super.onResume();

        if (!isOping) {
            onPositiveClick();
            isOping = true;
        }
    }

    @Override
    protected void onPositiveClick() {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_error);
            dismiss();
            return;
        }

        if (!checkData()) {
            return;
        }
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (ctx.isCopy()) {
            getViewModel().copy(account, ctx.dstDir, ctx.dstRepoId, ctx.dstRepoName, ctx.srcDir, ctx.srcRepoId, ctx.srcRepoName, ctx.dirents);
        } else {
            getViewModel().move(account, ctx.dstDir, ctx.dstRepoId, ctx.dstRepoName, ctx.srcDir, ctx.srcRepoId, ctx.srcRepoName, ctx.dirents);
        }
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                refreshData(false);
                dismiss();
            }
        });

        getViewModel().getResultLiveData().observe(this, resultModel -> {
            refreshData();

            dismiss();
        });

        getViewModel().getRefreshLiveData().observe(this, this::showLoading);

    }

    private boolean checkData() {
        if (ctx == null) {
            return false;
        }

        if (CollectionUtils.isEmpty(ctx.dirents)) {
            return false;
        }

        if (TextUtils.isEmpty(ctx.srcRepoId)
                || TextUtils.isEmpty(ctx.srcRepoName)
                || TextUtils.isEmpty(ctx.srcDir)
                || TextUtils.isEmpty(ctx.dstRepoId)
                || TextUtils.isEmpty(ctx.dstDir)) {
            return false;
        }

        return true;
    }
}
