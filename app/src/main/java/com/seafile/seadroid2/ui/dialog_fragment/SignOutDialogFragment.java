package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.account.AccountUtils;
import com.seafile.seadroid2.ssl.CertsManager;
import com.seafile.seadroid2.ui.base.fragment.CustomDialogFragment;

public class SignOutDialogFragment extends CustomDialogFragment {
    public static SignOutDialogFragment newInstance() {

        Bundle args = new Bundle();

        SignOutDialogFragment fragment = new SignOutDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_message_textview;
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.settings_account_sign_out_title;
    }

    @Override
    protected void onPositiveClick() {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();

        CertsManager.instance().deleteCertForAccount(account);
        AccountUtils.logout(account);

        refreshData();
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        //set message
        TextView textView = containerView.findViewById(R.id.message_view);
        textView.setText(R.string.settings_account_sign_out_confirm);
    }

}
