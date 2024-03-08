package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.TextUtils;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.context.CopyMoveContext;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.CopyMoveViewModel;
import com.seafile.seadroid2.util.Utils;

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
            if (ctx.isdir) {
                return ctx.isCopy() ? R.string.copy_folder_ing : R.string.move_folder_ing;
            } else {
                return ctx.isCopy() ? R.string.copy_file_ing : R.string.move_file_ing;
            }
        }
        return super.getDialogTitleRes();

    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        int strMsgId;
        if (ctx.isdir) {
            strMsgId = ctx.isCopy() ? R.string.copy_file_from : R.string.move_file_from;
        } else {
            strMsgId = ctx.isCopy() ? R.string.copy_file_from : R.string.move_file_from;
        }

        String strMsg = getString(strMsgId);

        String srcDir = Utils.pathJoin(ctx.srcRepoName, ctx.srcDir);
        String srcDirPath = Utils.removeLastPathSeperator(srcDir);

        String dstPath = Utils.pathJoin(ctx.dstRepoName, ctx.dstDir);
        String dstDirPath = Utils.removeLastPathSeperator(dstPath);

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
        if (!checkData()) {
            return;
        }

        if (ctx.isCopy()) {
            getViewModel().copy(ctx.dstDir, ctx.dstRepoId, ctx.srcDir, ctx.srcRepoId, ctx.dirents);
        } else {
            getViewModel().move(ctx.dstDir, ctx.dstRepoId, ctx.srcDir, ctx.srcRepoId, ctx.dirents);
        }
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

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
