package com.seafile.seadroid2.ui.dialog_fragment.wiki;

import android.os.Bundle;
import android.text.TextUtils;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.blankj.utilcode.util.NetworkUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.framework.util.Toasts;
import com.seafile.seadroid2.ui.base.fragment.RequestCustomDialogFragmentWithVM;
import com.seafile.seadroid2.ui.wiki.WikiViewModel;

public class UnpublishWikiDialogFragment extends RequestCustomDialogFragmentWithVM<WikiViewModel> {
    private String wikiId;

    public static UnpublishWikiDialogFragment newInstance(String wikiId) {
        Bundle args = new Bundle();
        args.putString("wikiId", wikiId);
        UnpublishWikiDialogFragment fragment = new UnpublishWikiDialogFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (getArguments() == null) {
            throw new IllegalArgumentException("this dialogFragment need Arguments");
        }

        wikiId = getArguments().getString("wikiId");
        if (TextUtils.isEmpty(wikiId)) {
            throw new IllegalArgumentException("need wikiId param");
        }
    }

    @Override
    protected int getLayoutId() {
        return R.layout.view_dialog_message_textview;
    }

    @Override
    public int getDialogTitleRes() {
        return R.string.unpublish;
    }

    @Override
    protected void onPositiveClick() {
        if (!NetworkUtils.isConnected()) {
            Toasts.show(R.string.network_error);
            return;
        }

        getViewModel().cancelPublishWiki(wikiId);
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
                    dismiss();
                } else if (e != null) {
                    Toasts.show(e.getMessage());
                    refreshData(false);
                    dismiss();
                }
            }
        });

        getViewModel().getRefreshLiveData().observe(this, this::showLoading);
    }

    @Override
    protected void initView(LinearLayout containerView) {
        super.initView(containerView);

        //set message
        TextView textView = containerView.findViewById(R.id.message_view);
        textView.setText(R.string.unpublish_wiki_tip);
    }
}
