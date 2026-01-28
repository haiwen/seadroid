package com.seafile.seadroid2.ui.repo.tag_view;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;
import androidx.recyclerview.widget.LinearLayoutManager;

import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.context.GlobalNavContext;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.framework.db.entities.RepoModel;
import com.seafile.seadroid2.framework.model.repo.tags.TagResultModel;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;

import java.util.List;

public class TagListFragment extends BaseFragmentWithVM<TagListViewModel> {
    private TagListAdapter adapter;
    private LayoutFrameSwipeRvBinding binding;

    public static TagListFragment newInstance() {

        Bundle args = new Bundle();

        TagListFragment fragment = new TagListFragment();
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
        // back pressed callback
//        requireActivity().getOnBackPressedDispatcher().addCallback(getViewLifecycleOwner(), new OnBackPressedCallback(true) {
//            @Override
//            public void handleOnBackPressed() {
//
//            }
//        });

        initRv();

        initViewModel();

        loadData();
    }

    private void initRv() {
        adapter = new TagListAdapter();
        binding.rv.setAdapter(adapter);

        binding.rv.setPadding(0, Constants.DP.DP_16, 0, Constants.DP.DP_32);
    }

    private void initViewModel() {
        getViewModel().getRepoTagsLiveData().observe(getViewLifecycleOwner(), new Observer<List<TagResultModel>>() {
            @Override
            public void onChanged(List<TagResultModel> models) {
                if (binding.swipeRefreshLayout.isRefreshing()){
                    binding.swipeRefreshLayout.setRefreshing(false);
                }
                adapter.submitList(models);
            }
        });
    }


    private void loadData() {
        if (GlobalNavContext.getCurrentNavContext().inRoot()) {
            return;
        }

        RepoModel rm = GlobalNavContext.getCurrentNavContext().getRepoModel();
        if (rm == null) {
            return;
        }
        String repoId = rm.repo_id;
        getViewModel().loadRepoViews(repoId);
    }

}