package com.seafile.seadroid2.ui.adapter;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;

public class ViewPager2Adapter extends FragmentStateAdapter {
    private final List<Fragment> fragments = new ArrayList<>();
    private final List<Long> fragmentIds = new ArrayList<>();
    private final HashSet<Long> createIds = new HashSet<>();

    public ViewPager2Adapter(FragmentActivity fa) {
        super(fa);
    }

    public void addFragments(List<Fragment> fragments) {
        this.fragments.clear();
        this.fragments.addAll(fragments);

        this.fragmentIds.clear();
        for (Fragment fragment : fragments) {
            fragmentIds.add((long) fragment.hashCode());
        }
    }

    public List<Fragment> getFragments() {
        return fragments;
    }

    public void replaceFragment(int position, Fragment fragment) {
        fragments.set(position, fragment);
        fragmentIds.set(position, (long) fragment.hashCode());
        createIds.add(fragmentIds.get(position));
    }

    public void removeFragment(int position) {
        fragments.remove(position);
        fragmentIds.remove(position);
//        createIds.add(fragmentIds.get(position));
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
        createIds.add(fragmentIds.get(position));
        return fragments.get(position);
    }

    @Override
    public int getItemCount() {
        return fragments.size();
    }

    @Override
    public long getItemId(int position) {
        return fragmentIds.get(position);
    }

    @Override
    public boolean containsItem(long itemId) {
        return createIds.contains(itemId);
    }
}