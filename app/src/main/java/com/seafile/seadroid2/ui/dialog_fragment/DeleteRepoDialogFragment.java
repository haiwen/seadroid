package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.TextUtils;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.framework.data.model.ResultModel;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.DeleteRepoViewModel;

public class DeleteRepoDialogFragment extends RequestCustomDialogFragmentWithVM<DeleteRepoViewModel> {
    private String repoId;

    public static DeleteRepoDialogFragment newInstance(String repoId) {
        Bundle args = new Bundle();
        args.putString("repoId", repoId);
        DeleteRepoDialogFragment fragment = new DeleteRepoDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (getArguments() == null) {
            throw new IllegalArgumentException("this dialogFragment need Arguments");
        }

        if (!getArguments().containsKey("repoId")) {
            throw new IllegalArgumentException("this dialogFragment need repoId param");
        }

        repoId = getArguments().getString("repoId", null);

        if (TextUtils.isEmpty(repoId)) {
            throw new IllegalArgumentException("this dialogFragment need repoId param");
        }
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_message_textview;
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.delete_repo_title;
    }

    @Override
    protected void onPositiveClick() {
        getViewModel().deleteRepo(repoId);
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getActionLiveData().observe(this, new Observer<ResultModel>() {
            @Override
            public void onChanged(ResultModel resultModel) {

                if (!TextUtils.isEmpty(resultModel.error_msg)) {
                    ToastUtils.showLong(resultModel.error_msg);
                    dismiss();
                    return;
                }

                refreshData();

                dismiss();
            }
        });

        getViewModel().getRefreshLiveData().observe(this, this::showLoading);
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        //set message
        TextView textView = containerView.findViewById(R.id.message_view);
        textView.setText(R.string.delete_repo);
    }
}
