package com.seafile.seadroid2.ui.activities;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.Observer;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SeafException;
import com.seafile.seadroid2.annotation.NotSupport;
import com.seafile.seadroid2.databinding.LayoutFrameSwipeRvBinding;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.ui.base.fragment.BaseFragmentWithVM;
import com.seafile.seadroid2.view.TipsViews;

import java.util.List;

import kotlin.Pair;

@NotSupport
@Deprecated
public class MineActivitiesFragment extends BaseFragmentWithVM<ActivityViewModel> {

    private LayoutFrameSwipeRvBinding binding;
    private ActivityAdapter adapter;

    public static MineActivitiesFragment newInstance() {
        Bundle args = new Bundle();
        MineActivitiesFragment fragment = new MineActivitiesFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = LayoutFrameSwipeRvBinding.inflate(inflater, container, false);
        binding.swipeRefreshLayout.setOnRefreshListener(this::reload);
        binding.swipeRefreshLayout.setColorSchemeResources(R.color.fancy_orange);

        return binding.getRoot();
    }


    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initAdapter();

        initViewModel();

        reload();
    }

    private void initAdapter() {
        adapter = new ActivityAdapter();
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.no_starred_file);
        tipView.setOnClickListener(v -> reload());
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(false);

        binding.rv.setAdapter(createMuiltAdapterHelper(adapter).getAdapter());
    }

    private void showErrorTip() {
        TextView tipView = TipsViews.getTipTextView(requireContext());
        tipView.setText(R.string.error_when_load_starred);
        tipView.setOnClickListener(v -> reload());
        adapter.setStateView(tipView);
        adapter.setStateViewEnable(false);
    }

    private void initViewModel() {
        getViewModel().getRefreshLiveData().observe(getViewLifecycleOwner(), new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                binding.swipeRefreshLayout.setRefreshing(aBoolean);
            }
        });

        getViewModel().getExceptionLiveData().observe(getViewLifecycleOwner(), new Observer<Pair<Integer, SeafException>>() {
            @Override
            public void onChanged(Pair<Integer, SeafException> exceptionPair) {
                showErrorTip();
            }
        });

        getViewModel().getListLiveData().observe(getViewLifecycleOwner(), new Observer<List<ActivityModel>>() {
            @Override
            public void onChanged(List<ActivityModel> activityModels) {
                adapter.setStateViewEnable(true);

                adapter.submitList(activityModels);
            }
        });
    }

    private void reload() {
        adapter.setStateViewEnable(false);
        getViewModel().loadAllData(1);
    }
}
