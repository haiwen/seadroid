package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.CollectionUtils;
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.DeleteDirsViewModel;

import java.util.ArrayList;
import java.util.List;

public class DeleteFileDialogFragment extends RequestCustomDialogFragmentWithVM<DeleteDirsViewModel> {
    private List<String> dirents;
    private boolean isDir = false;

    public static DeleteFileDialogFragment newInstance(List<String> direntIds) {
        DeleteFileDialogFragment fragment = new DeleteFileDialogFragment();
        Bundle bundle = new Bundle();
        bundle.putStringArrayList("dirent_ids", new ArrayList<>(direntIds));
        fragment.setArguments(bundle);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle bundle = getArguments();

        if (bundle == null || !bundle.containsKey("dirent_ids")) {
            throw new RuntimeException("need a dirent_ids param");
        }

        dirents = bundle.getStringArrayList("dirent_ids");

    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_delete_dirent;
    }

    @Override
    protected void onPositiveClick() {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_error);
            return;
        }

        if (CollectionUtils.isEmpty(dirents)) {
            return;
        }

        getViewModel().delete(dirents);
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
