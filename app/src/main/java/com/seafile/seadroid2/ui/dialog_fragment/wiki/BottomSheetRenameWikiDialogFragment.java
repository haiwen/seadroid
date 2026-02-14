package com.seafile.seadroid2.ui.dialog_fragment.wiki;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.widget.EditText;
import android.widget.LinearLayout;

import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.RequestBottomSheetDialogFragmentWithVM;
import com.seafile.seadroid2.ui.wiki.WikiViewModel;

public class BottomSheetRenameWikiDialogFragment extends RequestBottomSheetDialogFragmentWithVM<WikiViewModel> {
    private String wikiId, wikiName;

    public static BottomSheetRenameWikiDialogFragment newInstance(String wikiId, String wikiName) {
        BottomSheetRenameWikiDialogFragment fragment = new BottomSheetRenameWikiDialogFragment();
        Bundle bundle = new Bundle();
        bundle.putString("wikiId", wikiId);
        bundle.putString("wikiName", wikiName);
        fragment.setArguments(bundle);
        return fragment;
    }


    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        Bundle bundle = getArguments();

        if (bundle == null) {
            throw new RuntimeException("need a bundle");
        }

        wikiId = bundle.getString("wikiId");
        wikiName = bundle.getString("wikiName");

        if (TextUtils.isEmpty(wikiId)) {
            throw new IllegalArgumentException("this dialogFragment need a wikiId param");
        }
    }


    @Override
    protected int getLayoutId() {
        return R.layout.dialog_new_dir_file;
    }

    @Override
    protected String getTitle() {
        return getString(R.string.rename_wiki);
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

        EditText editText = getDialogView().findViewById(R.id.edit_name);
        String newName = editText.getText().toString();
        newName = StringUtils.trimEnd(newName, " ");

        getViewModel().renameWiki(wikiId, newName);
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        EditText editText = getEditText();
        if (!TextUtils.isEmpty(wikiName)) {
            editText.setText(wikiName);
            editText.setSelection(wikiName.length());
        }
    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                if (SeafException.SUCCESS == e) {
                    Toasts.show(R.string.rename_successful);

                    refreshData();

                    dismissDialogWithIme();
                } else if (e != null) {
                    Toasts.show(e.getMessage());
                }
            }
        });

        getViewModel().getRefreshLiveData().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                getEditText().setEnabled(!aBoolean);
                showLoading(aBoolean);
            }
        });
    }

    EditText editText;

    public EditText getEditText() {
        if (editText != null) {
            return editText;
        }
        editText = getDialogView().findViewById(R.id.edit_name);
        return editText;
    }

    private boolean checkData() {
        EditText editText = getEditText();
        Editable editable = editText.getText();
        if (editable == null || editable.isEmpty()) {
            return false;
        }

        String newName = editable.toString();
        newName = StringUtils.trim(newName, " ");
        return !TextUtils.equals(wikiName, newName);
    }
}
