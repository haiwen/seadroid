package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.view.View;
import android.widget.EditText;
import android.widget.LinearLayout;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.KeyboardUtils;
import com.blankj.utilcode.util.ToastUtils;
import com.google.android.material.textfield.TextInputEditText;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.ui.base.fragment.RequestBottomSheetDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.RenameRepoViewModel;

public class BottomSheetRenameDialogFragment extends RequestBottomSheetDialogFragmentWithVM<RenameRepoViewModel> {
    /**
     * "repo" or "dir" or "file"
     */
    private String type;
    private String curName, repoId, repoName, curPath;

    public static BottomSheetRenameDialogFragment newInstance(String curName, String repoId, String type) {
        BottomSheetRenameDialogFragment fragment = new BottomSheetRenameDialogFragment();
        Bundle bundle = new Bundle();
        bundle.putString("name", curName);
        bundle.putString("path", "/");
        bundle.putString("repoId", repoId);
        bundle.putString("type", type);
        fragment.setArguments(bundle);
        return fragment;
    }

    public static BottomSheetRenameDialogFragment newInstance(String curName, String curPath, String repoId, String repoName, String type) {

        BottomSheetRenameDialogFragment fragment = new BottomSheetRenameDialogFragment();
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

        if (TextUtils.isEmpty(repoId)) {
            throw new IllegalArgumentException("this dialogFragment need a repoId param");
        }

        if (bundle.containsKey("repoName")) {
            repoName = bundle.getString("repoName");
        } else {
            repoName = curName;
        }
        type = bundle.getString("type");
    }


    @Override
    protected int getLayoutId() {
        return R.layout.dialog_new_dir_file;
    }

    @Override
    protected String getTitle() {
        int res = getTitleRes();
        if (res != 0) {
            return getString(res);
        }
        return null;
    }


    public int getTitleRes() {
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

        EditText editText = getDialogView().findViewById(R.id.edit_name);
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

        EditText editText = getDialogView().findViewById(R.id.edit_name);
        if (!TextUtils.isEmpty(curName)) {
            editText.setText(curName);
            editText.setSelection(curName.length());
        }
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        EditText editText = getDialogView().findViewById(R.id.edit_name);
        if (editText == null) {
            return;
        }
        editText.postDelayed(new Runnable() {
            @Override
            public void run() {
                editText.requestFocus();
                KeyboardUtils.showSoftInput(editText);
            }
        }, 200);
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                if (e != null) {
                    ToastUtils.showLong(e.getMessage());
                }
            }
        });

        getViewModel().getActionLiveData().observe(this, new Observer<String>() {
            @Override
            public void onChanged(String s) {
                ToastUtils.showLong(R.string.rename_successful);

                refreshData();

                dismissDialogWithIme();
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
        EditText editText = getDialogView().findViewById(R.id.edit_name);
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
