package com.seafile.seadroid2.ui.dialog_fragment;

import android.text.Editable;
import android.text.TextUtils;
import android.widget.EditText;
import android.widget.LinearLayout;

import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.ToastUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.framework.data.model.dirents.FileCreateModel;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.RenameRepoViewModel;

public class RenameDialogFragment extends RequestCustomDialogFragmentWithVM<RenameRepoViewModel> {
    private String curName, repoId, curPath, type;

    public static RenameDialogFragment newInstance() {
        return new RenameDialogFragment();
    }

    /**
     * @param type "repo" or "dir" or "file"
     */
    public void initData(String curName, String curPath, String repoId, String type) {
        this.curName = curName;
        this.curPath = curPath;
        this.repoId = repoId;
        this.type = type;
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

        //TODO 更新本地数据库数据 、DataStore、SP
        if (TextUtils.equals("repo", type)) {
            getViewModel().renameRepo(newName, repoId);
        } else if (TextUtils.equals("dir", type)) {
            getViewModel().renameDir(repoId, curPath, newName);
        } else if (TextUtils.equals("file", type)) {
            getViewModel().renameFile(repoId, curPath, newName);
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
        if (TextUtils.equals(curName, newName)) {
            return false;
        }

        return true;
    }
}
