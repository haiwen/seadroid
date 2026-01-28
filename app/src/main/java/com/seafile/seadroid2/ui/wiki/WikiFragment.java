package com.seafile.seadroid2.ui.wiki;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.GridLayoutManager;

import com.chad.library.adapter4.BaseQuickAdapter;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.model.wiki.WikiInfoModel;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.ui.webview.SeaWebViewActivity;

import java.util.List;

public class WikiFragment extends BaseFragmentWithVM<WikiViewModel> {
    private LayoutFrameSwipeRvBinding binding;
    private WikiAdapter adapter;

    public static WikiFragment newInstance() {

        Bundle args = new Bundle();

        WikiFragment fragment = new WikiFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        binding.swipeRefreshLayout.setOnRefreshListener(this::loadData);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initLiveData();

        init();

        loadData();
    }

    private void init() {
        adapter = new WikiAdapter();
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<BaseModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<BaseModel, ?> baseQuickAdapter, @NonNull View view, int i) {

                Account account = SupportAccountManager.getInstance().getCurrentAccount();
                if (account == null) {
                    return;
                }

                BaseModel model = adapter.getItems().get(i);
                WikiInfoModel wikiInfoModel = (WikiInfoModel) model;
                String url = Utils.pathJoin(account.getServer(), "wikis", wikiInfoModel.id);

                SeaWebViewActivity.openUrl(requireContext(), url, true);
            }
        });
        binding.rv.setAdapter(adapter);

        GridLayoutManager manager = new GridLayoutManager(requireContext(), 2);
        manager.setSpanSizeLookup(new GridLayoutManager.SpanSizeLookup() {
            @Override
            public int getSpanSize(int position) {
                BaseModel model = adapter.getItems().get(position);
                if (model instanceof GroupItemModel) {
                    return 2;
                }
                return 1;
            }
        });

        binding.rv.setLayoutManager(manager);
        binding.rv.setClipToPadding(false);
        binding.rv.setPadding(Constants.DP.DP_8, Constants.DP.DP_8, Constants.DP.DP_8, Constants.DP.DP_8);
    }

    private void initLiveData() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);

            }
        });
        getViewModel().getWikisLiveData().observe(getViewLifecycleOwner(), new Observer<List<BaseModel>>() {
            @Override
            public void onChanged(List<BaseModel> baseModels) {
                adapter.submitList(baseModels);
            }
        });
    }

    private void loadData() {
        getViewModel().loadWikis();
    }
}
