package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.repo.RepoViewModel;

public class LeaveShareDialogFragment extends RequestCustomDialogFragmentWithVM<RepoViewModel> {

    public static LeaveShareDialogFragment newInstance(String repoId,String from) {
        Bundle args = new Bundle();
        args.putString("repoId", repoId);
        args.putString("from", from);
        LeaveShareDialogFragment fragment = new LeaveShareDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    private String repoId;
    private String from;

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle bundle = getArguments();
        if (bundle != null) {
            repoId = bundle.getString("repoId");
            from = bundle.getString("from");
        }
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_message_textview;
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.file_share_leave;
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);


        //set message
        TextView textView = containerView.findViewById(R.id.message_view);
        textView.setText(R.string.tip_leave_share);
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getRefreshLiveData().observe(this, this::showLoading);

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                Toasts.show(e.getMessage());
                refreshData(false);
                dismiss();
            }
        });

        getViewModel().getLeaveShareLiveData().observe(this, new Observer<ResultModel>() {
            @Override
            public void onChanged(ResultModel resultModel) {
                refreshData();

                dismiss();
            }
        });
    }

    @Override
    protected void onPositiveClick() {
        getViewModel().leaveShare(repoId, from);
    }
}
