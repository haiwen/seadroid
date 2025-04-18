package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.widget.EditText;
import android.widget.LinearLayout;

import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.framework.data.model.dirents.FileCreateModel;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.RenameRepoViewModel;

import java.util.ArrayList;
import java.util.List;

public class RenameDialogFragment extends RequestCustomDialogFragmentWithVM<RenameRepoViewModel> {

    /**
     * "repo" or "dir" or "file"
     */
    private String type;
    private String curName, repoId, repoName, curPath;

    public static RenameDialogFragment newInstance(String curName, String repoId, String type) {

        RenameDialogFragment fragment = new RenameDialogFragment();
        Bundle bundle = new Bundle();
        bundle.putString("name", curName);
        bundle.putString("path", "/");
        bundle.putString("repoId", repoId);
        bundle.putString("type", type);
        fragment.setArguments(bundle);
        return fragment;
    }

    public static RenameDialogFragment newInstance(String curName, String curPath, String repoId, String repoName, String type) {

        RenameDialogFragment fragment = new RenameDialogFragment();
        Bundle bundle = new Bundle();
        bundle.putString("name", curName);
        bundle.putString("path", curPath);
        bundle.putString("repoId", repoId);
        bundle.putString("repoName", repoName);
        bundle.putString("type", type);
        fragment.setArguments(bundle);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle bundle = getArguments();

        if (bundle == null || !bundle.containsKey("repoId")) {
            throw new RuntimeException("need a dirent param");
        }

        curName = bundle.getString("name");
        curPath = bundle.getString("path");
        repoId = bundle.getString("repoId");
        if (bundle.containsKey("repoName")) {
            repoName = bundle.getString("repoName");
        } else {
            repoName = curName;
        }
        type = bundle.getString("type");
    }


    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_new_file;
    }

    @Override
    public int getDialogTitleRes() {
        if (TextUtils.equals("repo", type)) {
            return R.string.rename_repo;
        } else if (TextUtils.equals("dir", type)) {
            return R.string.rename_dir;
        } else if (TextUtils.equals("file", type)) {
            return R.string.rename_file;
        }
        return 0;
    }

    @Override
    protected void onPositiveClick() {
        if (!checkData()) {
            return;
        }

        EditText editText = getDialogView().findViewById(R.id.new_file_name);
        String newName = editText.getText().toString();
        newName = StringUtils.trimEnd(newName, " ");

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (TextUtils.equals("repo", type)) {
            getViewModel().renameRepo(account, repoName, newName, repoId);
        } else if (TextUtils.equals("dir", type)) {
            getViewModel().renameDir(account, repoId, repoName, curPath, curName, newName);
        } else if (TextUtils.equals("file", type)) {
            getViewModel().renameFile(account, repoId, repoName, curPath, curName, newName);
        }
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        if (TextUtils.isEmpty(repoId)) {
            throw new IllegalArgumentException("this dialogFragment need a repoId param");
        }

        EditText editText = getDialogView().findViewById(R.id.new_file_name);
        editText.setText(curName);
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getActionLiveData().observe(this, new Observer<String>() {
            @Override
            public void onChanged(String s) {
                if (TextUtils.equals("success", s)) {
                    ToastUtils.showLong(R.string.rename_successful);

                    refreshData();

                    dismiss();
                } else {
                    setInputError(R.id.text_input, s);
                }
            }
        });

        getViewModel().getRenameFileLiveData().observe(this, new Observer<FileCreateModel>() {
            @Override
            public void onChanged(FileCreateModel fileCreateModel) {
                if (TextUtils.isEmpty(fileCreateModel.error_msg)) {
                    ToastUtils.showLong(R.string.rename_successful);

                    refreshData();

                    dismiss();
                } else {
                    setInputError(R.id.text_input, fileCreateModel.error_msg);
                }
            }
        });

        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                showLoading(aBoolean);
            }
        });
    }

    private boolean checkData() {
        EditText editText = getDialogView().findViewById(R.id.new_file_name);
        Editable editable = editText.getText();
        if (editable == null || editable.length() == 0) {
            return false;
        }

        String newName = editable.toString();
        newName = StringUtils.trim(newName, " ");
        if (TextUtils.equals(curName, newName)) {
            return false;
        }

        return true;
    }
}
