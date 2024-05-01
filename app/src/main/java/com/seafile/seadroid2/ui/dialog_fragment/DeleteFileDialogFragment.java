package com.seafile.seadroid2.ui.dialog_fragment;

import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.DeleteDirsViewModel;

import java.util.ArrayList;
import java.util.List;

public class DeleteFileDialogFragment extends RequestCustomDialogFragmentWithVM<DeleteDirsViewModel> {
    private List<DirentModel> dirents;
    private boolean isDir;

    public static DeleteFileDialogFragment newInstance() {
        return new DeleteFileDialogFragment();
    }

    public void initData(DirentModel dirent) {
        this.dirents = new ArrayList<>();
        dirents.add(dirent);
        isDir = dirent.isDir();
    }

    public void initData(List<DirentModel> dirents) {
        this.dirents = dirents;
        isDir = false;
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_delete_dirent;
    }

    @Override
    protected void onPositiveClick() {

        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        getViewModel().deleteDirents(account.getSignature(), dirents, false);
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getRefreshLiveData().observe(this, this::showLoading);

        getViewModel().getActionLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                refreshData();

                dismiss();
            }
        });
    }

    @Override
    public int getDialogTitleRes() {
        return isDir ? R.string.delete_dir : R.string.delete_file_f;
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        //set message
        TextView textView = containerView.findViewById(R.id.message_view);
        textView.setText(R.string.delete_file);


    }


}
