package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.google.firebase.analytics.FirebaseAnalytics;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.AnalyticsEvent;
import com.seafile.seadroid2.framework.datastore.DataStoreManager;
import com.seafile.seadroid2.framework.datastore.sp.AlbumBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.FolderBackupManager;
import com.seafile.seadroid2.framework.datastore.sp.GestureLockManager;
import com.seafile.seadroid2.framework.http.IO;
import com.seafile.seadroid2.framework.util.AccountUtils;
import com.seafile.seadroid2.framework.worker.BackgroundJobManagerImpl;
import com.seafile.seadroid2.ui.base.fragment.CustomDialogFragment;
import com.seafile.seadroid2.ui.camera_upload.CameraUploadManager;

import java.util.List;

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

        //firebase - event -login
        Bundle eventBundle = new Bundle();
        eventBundle.putString(FirebaseAnalytics.Param.METHOD, SignOutDialogFragment.class.getSimpleName());
        FirebaseAnalytics.getInstance(requireContext()).logEvent(AnalyticsEvent.SING_OUT, eventBundle);


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
