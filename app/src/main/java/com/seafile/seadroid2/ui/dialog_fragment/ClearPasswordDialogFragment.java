package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.widget.LinearLayout;
import android.widget.TextView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.fragment.CustomDialogFragment;
import com.seafile.seadroid2.framework.datastore.DataManager;
import com.seafile.seadroid2.framework.data.DatabaseHelper;

public class ClearPasswordDialogFragment extends CustomDialogFragment {
    public static ClearPasswordDialogFragment newInstance() {

        Bundle args = new Bundle();

        ClearPasswordDialogFragment fragment = new ClearPasswordDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_message_textview;
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.clear_password_title;
    }

    @Override
    protected void onPositiveClick() {
        //TODO 清除密码
        // clear cached data from database
        DatabaseHelper dbHelper = DatabaseHelper.getDatabaseHelper();
        dbHelper.clearEnckeys();
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        //set message
        TextView textView = containerView.findViewById(R.id.message_view);
        textView.setText(R.string.clear_password_warning);
    }

}
