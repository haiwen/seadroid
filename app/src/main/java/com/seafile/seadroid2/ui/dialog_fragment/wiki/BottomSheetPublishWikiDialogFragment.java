package com.seafile.seadroid2.ui.dialog_fragment.wiki;

import android.os.Bundle;
import android.text.Editable;
import android.text.TextUtils;
import android.widget.EditText;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.util.StringUtils;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.fragment.RequestBottomSheetDialogFragmentWithVM;
import com.seafile.seadroid2.ui.wiki.WikiViewModel;

public class BottomSheetPublishWikiDialogFragment extends RequestBottomSheetDialogFragmentWithVM<WikiViewModel> {
    private String wikiId;
    private String urlPrefix;

    public static BottomSheetPublishWikiDialogFragment newInstance(String wikiId) {
        BottomSheetPublishWikiDialogFragment fragment = new BottomSheetPublishWikiDialogFragment();
        Bundle bundle = new Bundle();
        bundle.putString("wikiId", wikiId);
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

        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account != null) {
            urlPrefix = Utils.pathJoin(account.getServer(), "wiki", "publish");
        }

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
        return getString(R.string.publish_wiki);
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

        getViewModel().publishWiki(wikiId, newName);
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        EditText editText = getEditText();
        editText.setHint(R.string.publish_wiki_custom_url);

        if (TextUtils.isEmpty(urlPrefix)) {
            editText.setText(urlPrefix);
            editText.setSelection(urlPrefix.length());
        }

        TextView textView = getDialogView().findViewById(R.id.edit_desc);
        textView.setVisibility(TextView.VISIBLE);
        textView.setText(R.string.tip_publish_wiki_desc);

    }

    @Override
    protected void initViewModel() {
        super.initViewModel();

        getViewModel().getSeafExceptionLiveData().observe(this, new Observer<SeafException>() {
            @Override
            public void onChanged(SeafException e) {
                if (SeafException.SUCCESS == e) {
                    Toasts.show(R.string.success);

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
        if (newName.startsWith(urlPrefix)) {
            String t = newName.replace(urlPrefix, "");
            if (t.length() < 5 || t.length() > 30) {
                return false;
            }
        }

        return true;
    }
}
