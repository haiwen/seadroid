package com.seafile.seadroid2.ui.dialog_fragment;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.widget.EditText;

import androidx.annotation.Nullable;

import com.blankj.utilcode.util.FileUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.fragment.RequestBottomSheetDialogFragmentWithVM;
import com.seafile.seadroid2.ui.dialog_fragment.viewmodel.NewDirViewModel;

public class BottomSheetNewLocalFolderDialogFragment extends RequestBottomSheetDialogFragmentWithVM<NewDirViewModel> {

    private String parentDir;

    public static BottomSheetNewLocalFolderDialogFragment newInstance(String parentDir) {
        Bundle args = new Bundle();
        args.putString("parent_dir", parentDir);
        BottomSheetNewLocalFolderDialogFragment fragment = new BottomSheetNewLocalFolderDialogFragment();
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

        parentDir = args.getString("parent_dir");

        if (TextUtils.isEmpty(parentDir)) {
            throw new IllegalArgumentException("this dialogFragment need parentDir param");
        }
    }

    @Override
    protected int getLayoutId() {
        return R.layout.dialog_new_local_folder;
    }

    @Override
    protected String getTitle() {
        return getString(R.string.create_new_dir);
    }

    @Override
    protected void onPositiveClick() {
        if (!checkData()) {
            return;
        }

        EditText edit = getDialogView().findViewById(R.id.edit_name);
        String folderName = edit.getText().toString();

        boolean success = FileUtils.createOrExistsDir(Utils.pathJoin(parentDir,folderName));
        if (success) {
            Toasts.show(getString(R.string.create_new_folder_success, folderName));
        } else {
            Toasts.show(getString(R.string.saf_failed_to_create_directory, folderName));
        }
        refreshData(success);
        dismissDialogWithIme();
    }


    private boolean checkData() {
        EditText editText = getDialogView().findViewById(R.id.edit_name);
        Editable editable = editText.getText();
        if (editable == null || editable.length() == 0) {
            Toasts.show(R.string.dir_name_empty);
            return false;
        }

        String t = editable.toString().trim();
        if (TextUtils.isEmpty(t)) {
            Toasts.show(R.string.dir_name_empty);
            return false;
        }

        return true;
    }
}
