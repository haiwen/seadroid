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
import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.model.ResultModel;
import com.seafile.seadroid2.framework.model.dirents.FileCreateModel;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.fragment.RequestBottomSheetDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.NewDirViewModel;

public class BottomSheetNewDirFileDialogFragment extends RequestBottomSheetDialogFragmentWithVM<NewDirViewModel> {

    private Account account;
    private String parentDir, repoId;
    private boolean isDir;

    public static BottomSheetNewDirFileDialogFragment newInstance(String repoId, String parentDir, boolean isDir) {
        Bundle args = new Bundle();
        args.putString("repo_id", repoId);
        args.putString("parent_dir", parentDir);
        args.putBoolean("is_dir", isDir);
        BottomSheetNewDirFileDialogFragment fragment = new BottomSheetNewDirFileDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    public static BottomSheetNewDirFileDialogFragment newInstance(Account account, String repoId, String parentDir, boolean isDir) {
        Bundle args = new Bundle();
        args.putString("repo_id", repoId);
        args.putString("parent_dir", parentDir);
        args.putBoolean("is_dir", isDir);
        args.putParcelable("account", account);
        BottomSheetNewDirFileDialogFragment fragment = new BottomSheetNewDirFileDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle args = getArguments();
        if (args == null) {
            return;
        }

        repoId = args.getString("repo_id");
        parentDir = args.getString("parent_dir");
        isDir = args.getBoolean("is_dir");

        account = args.getParcelable("account");
        if (account == null) {
            account = SupportAccountManager.getInstance().getCurrentAccount();
        }

        if (TextUtils.isEmpty(parentDir)) {
            throw new IllegalArgumentException("this dialogFragment need parentDir param");
        }

        if (TextUtils.isEmpty(repoId)) {
            throw new IllegalArgumentException("this dialogFragment need repoId param");
        }
    }


    @Override
    protected int getLayoutId() {
        return R.layout.dialog_new_dir_file;
    }

    @Override
    protected String getTitle() {
        return getString(isDir ? R.string.create_new_dir : R.string.create_new_file);
    }

    @Override
    protected void initView(LinearLayout parentView) {
        super.initView(parentView);
    }

    @Override
    protected void onPositiveClick() {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_error);
            return;
        }

        if (!checkData()) {
            return;
        }

        EditText name = getDialogView().findViewById(R.id.edit_name);
        String pathName = name.getText().toString();
        pathName = Utils.pathJoin(parentDir, pathName).trim();

        if (isDir) {
            getViewModel().createNewDir(account, pathName, repoId);
        } else {
            getViewModel().createNewFile(account, pathName, repoId);
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

                Toasts.show(e.getMessage());

                refreshData(false);

                dismissDialogWithIme();
            }
        });

        if (isDir) {
            getViewModel().getCreateDirLiveData().observe(this, new Observer<ResultModel>() {
                @Override
                public void onChanged(ResultModel resultModel) {
                    if (!TextUtils.isEmpty(resultModel.error_msg)) {
                        Toasts.show(resultModel.error_msg);
                    } else {

                        EditText name = getDialogView().findViewById(R.id.edit_name);
                        String pathName = name.getText().toString();

                        Toasts.show(getString(R.string.create_new_folder_success, pathName));

                        refreshData();

                        dismissDialogWithIme();
                    }
                }
            });
        } else {
            getViewModel().getCreateFileLiveData().observe(this, new Observer<FileCreateModel>() {
                @Override
                public void onChanged(FileCreateModel fileCreateModel) {
                    if (!TextUtils.isEmpty(fileCreateModel.error_msg)) {
                        Toasts.show(fileCreateModel.error_msg);
                    } else {
                        EditText name = getDialogView().findViewById(R.id.edit_name);
                        String pathName = name.getText().toString();

                        Toasts.show(getString(R.string.create_new_file_success, pathName));

                        refreshData();

                        dismissDialogWithIme();
                    }
                }
            });
        }


        getViewModel().getRefreshLiveData().observe(this, aBoolean -> showLoading(aBoolean));
    }

    private boolean checkData() {
        EditText editText = getDialogView().findViewById(R.id.edit_name);
        Editable editable = editText.getText();
        if (editable == null || editable.length() == 0) {
            if (isDir) {
                Toasts.show(R.string.dir_name_empty);
            } else {
                Toasts.show(R.string.file_name_empty);
            }
            return false;
        }

        String t = editable.toString().trim();
        if (TextUtils.isEmpty(t)) {
            if (isDir) {
                Toasts.show(R.string.dir_name_empty);
            } else {
                Toasts.show(R.string.file_name_empty);
            }
            return false;
        }

        if (t.contains("/")) {
            Toasts.show(R.string.name_contains_slash);
            return false;
        }
        if (t.contains("`")) {
            Toasts.show(R.string.name_contains_backtick);
            return false;
        }

        if (t.contains("\\\\")) {
            Toasts.show(R.string.name_contains_backslash);
            return false;
        }
        if (t.equals("..")) {
            Toasts.show(R.string.name_cannot_be_double_dots);
            return false;
        }

        return true;
    }
}
