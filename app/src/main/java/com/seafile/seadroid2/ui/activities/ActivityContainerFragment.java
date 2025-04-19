package com.seafile.seadroid2.ui.activities;

import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.android.material.tabs.TabLayout;
import com.google.android.material.tabs.TabLayoutMediator;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.annotation.Todo;
import com.seafile.seadroid2.databinding.FragmentActivityBinding;
import com.seafile.seadroid2.ui.adapter.ViewPager2Adapter;
import com.seafile.seadroid2.ui.base.fragment.BaseFragment;

@Todo("not support")
public class ActivityContainerFragment extends BaseFragment {

    private FragmentActivityBinding binding;

    public static ActivityContainerFragment newInstance() {

        Bundle args = new Bundle();

        ActivityContainerFragment fragment = new ActivityContainerFragment();
        fragment.setArguments(args);
        return fragment;
    }

    @Nullable
    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, @Nullable ViewGroup container, @Nullable Bundle savedInstanceState) {
        binding = FragmentActivityBinding.inflate(getLayoutInflater());
        return binding.getRoot();
    }

    @Override
    public void onViewCreated(@NonNull View view, @Nullable Bundle savedInstanceState) {
        super.onViewCreated(view, savedInstanceState);
        initTabLayout();

        initViewPager();
    }

    private void initTabLayout() {
        binding.tabs.setTabIndicatorAnimationMode(TabLayout.INDICATOR_ANIMATION_MODE_ELASTIC);
        binding.tabs.setSelectedTabIndicator(R.drawable.cat_tabs_rounded_line_indicator);
        binding.tabs.setTabIndicatorFullWidth(false);
        binding.tabs.setTabGravity(TabLayout.GRAVITY_START);
    }

    private void initViewPager() {
        ViewPager2Adapter adapter = new ViewPager2Adapter(getChildFragmentManager(), getLifecycle());
        adapter.addFragment(AllActivitiesFragment.newInstance());
        adapter.addFragment(MineActivitiesFragment.newInstance());
        binding.viewPager.setAdapter(adapter);

        String[] tabArray = getResources().getStringArray(R.array.activity_fragment_titles);
        for (String s : tabArray) {
            new TabLayoutMediator(binding.tabs, binding.viewPager, new TabLayoutMediator.TabConfigurationStrategy() {
                @Override
                public void onConfigureTab(@NonNull TabLayout.Tab tab, int position) {
                    tab.setText(s);
                }
            }).attach();
        }
    }
}
