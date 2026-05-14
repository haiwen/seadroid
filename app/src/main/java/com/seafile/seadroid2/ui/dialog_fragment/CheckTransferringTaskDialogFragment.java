package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.AccountUtils;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.ui.base.fragment.CustomDialogFragment;

public class CheckTransferringTaskDialogFragment extends CustomDialogFragment {
    public static CheckTransferringTaskDialogFragment newInstance() {

        Bundle args = new Bundle();

        CheckTransferringTaskDialogFragment fragment = new CheckTransferringTaskDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_message_textview;
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.channel_name_transfer;
    }

    @Override
    protected void onPositiveClick() {
        if (onRefreshDataListener != null) {
            onRefreshDataListener.onActionStatus(true);
        }
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        //set message
        TextView textView = containerView.findViewById(R.id.message_view);
        textView.setText(R.string.tip_stop_transfer_when_switch_or_logout);
    }

}
