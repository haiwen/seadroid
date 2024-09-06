package com.seafile.seadroid2.ui.adapter;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import com.blankj.utilcode.util.CollectionUtils;

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

    public int removeByClass(Class<?> clz) {
        int i = indexByClass(clz);
        if (i == -1) {
            return i;
        }

        removeFragment(i);

        return i;
    }

    public int indexByClass(Class<?> clz) {
        if (!CollectionUtils.isEmpty(fragments)) {
            for (int i = 0; i < fragments.size(); i++) {
                String n = fragments.get(i).getClass().getName();
                if (clz.getName().equals(n)) {
                    return i;
                }
            }
        }

        return -1;
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