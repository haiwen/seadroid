package com.seafile.seadroid2.ui.repo;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.fragment.app.Fragment;
import androidx.lifecycle.Observer;
import androidx.viewpager2.widget.ViewPager2;

import com.blankj.utilcode.util.CollectionUtils;
import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.bus.BusHelper;
import com.seafile.seadroid2.context.NavContext;
import com.seafile.seadroid2.databinding.FragmentRepoContainerBinding;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;
import com.seafile.seadroid2.ui.repo.repo_list.RepoQuickFragment;
import com.seafile.seadroid2.ui.repo.repo_view.RepoViewListFragment;
import com.seafile.seadroid2.ui.repo.tag_view.TagListFragment;

import java.util.List;

public class RepoContainerFragment extends BaseFragment {
    private FragmentRepoContainerBinding binding;

    public static RepoContainerFragment newInstance() {

        Bundle args = new Bundle();

        RepoContainerFragment fragment = new RepoContainerFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentRepoContainerBinding.inflate(inflater, container, false);
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);

        initLiveData();
        initView();
    }

    private void initLiveData() {
        BusHelper.getNavContextObserver().observe(getViewLifecycleOwner(), new Observer<NavContext>() {
            @Override
            public void onChanged(NavContext navContext) {
                if (navContext.inRoot()) {
                    binding.slidingTabs.setVisibility(View.GONE);
                } else {
                    binding.slidingTabs.setVisibility(View.VISIBLE);
                }
            }
        });
    }

    private void initView() {
//        binding.slidingTabs.removeAllTabs();

        binding.slidingTabs.setVisibility(View.VISIBLE);
        binding.slidingTabs.setTabIndicatorAnimationMode(TabLayout.INDICATOR_ANIMATION_MODE_ELASTIC);
        binding.slidingTabs.setSelectedTabIndicator(R.drawable.cat_tabs_rounded_line_indicator);
        binding.slidingTabs.setTabIndicatorFullWidth(false);
        binding.slidingTabs.setTabGravity(TabLayout.GRAVITY_START);

        ViewPager2Adapter viewPager2Adapter = new ViewPager2Adapter(requireActivity());
        viewPager2Adapter.addFragments(_fragment_list);
        binding.pager.setOffscreenPageLimit(1);
        binding.pager.setAdapter(viewPager2Adapter);
        binding.pager.setUserInputEnabled(false);

        String[] tabs = getResources().getStringArray(R.array.repo_container_fragment_titles);

        new TabLayoutMediator(binding.slidingTabs, binding.pager, true, new TabLayoutMediator.TabConfigurationStrategy() {
            @Override
            public void onConfigureTab(@NonNull TabLayout.Tab tab, int position) {
                tab.setText(tabs[position]);
            }
        }).attach();
    }

    private final List<Fragment> _fragment_list = CollectionUtils.newUnmodifiableListNotNull(
            RepoQuickFragment.newInstance(),
            RepoViewListFragment.newInstance(),
            TagListFragment.newInstance()
    );
}
