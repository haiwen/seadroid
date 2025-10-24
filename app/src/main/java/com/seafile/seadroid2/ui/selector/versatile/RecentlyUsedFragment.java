package com.seafile.seadroid2.ui.selector.versatile;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.blankj.utilcode.util.CollectionUtils;
import com.chad.library.adapter4.BaseQuickAdapter;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.compat.ContextCompatKt;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.framework.model.versatile.RecentlyUsedModel;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;

import java.util.List;

public class RecentlyUsedFragment extends BaseFragment {
    private LayoutFrameSwipeRvBinding binding;
    private RecentlyUsedListAdapter adapter;

    public static RecentlyUsedFragment newInstance() {
        return new RecentlyUsedFragment();
    }

    @Override
    public void onCreate(@Nullable Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        binding.getRoot().setBackgroundColor(ContextCompatKt.getColorCompat(requireContext(),R.color.bar_background_color));
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        binding.swipeRefreshLayout.setEnabled(false);
        init();
    }

    @Override
    public void onResume() {
        super.onResume();

        loadData();
    }

    private void init() {
        adapter = new RecentlyUsedListAdapter();
        adapter.setOnItemClickListener(new BaseQuickAdapter.OnItemClickListener<RecentlyUsedModel>() {
            @Override
            public void onClick(@NonNull BaseQuickAdapter<RecentlyUsedModel, ?> baseQuickAdapter, @NonNull View view, int i) {
                changeSelected(i);
            }
        });
        binding.rv.setAdapter(adapter);

        binding.rv.setLayoutManager(new LinearLayoutManager(requireContext()));
    }

    public RecentlyUsedModel getBackupInfo(){
        List<RecentlyUsedModel> list = adapter.getItems();
        if (CollectionUtils.isEmpty(list)) {
            return null;
        }

        for (RecentlyUsedModel recentlyUsedModel : list) {
            if (recentlyUsedModel.isSelected){
                return recentlyUsedModel;
            }
        }

        return null;
    }
    private void changeSelected(int index) {
        List<RecentlyUsedModel> list = adapter.getItems();
        if (CollectionUtils.isEmpty(list)) {
            return;
        }

        for (int i = 0; i < list.size(); i++) {
            RecentlyUsedModel r = list.get(i);
            if (i == index) {
                r.isSelected = true;
            } else {
                r.isSelected = false;
            }
        }

        adapter.notifyDataSetChanged();
    }


    private void loadData() {
        List<RecentlyUsedModel> list = VersatileSelectorActivity.getRecentUsedList();
        if (CollectionUtils.isEmpty(list)) {
            return;
        }

        adapter.submitList(list);

    }
}
