package com.seafile.seadroid2.ui.adapter;

import android.os.Bundle;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import com.blankj.utilcode.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;

public class ViewPager2Adapter extends FragmentStateAdapter {
    private final List<Fragment> fragments = new ArrayList<>();
    private final List<Long> fragmentIds = new ArrayList<>();

    public ViewPager2Adapter(FragmentActivity fa) {
        super(fa);
    }

    public void addFragments(List<Fragment> fts) {
        this.fragments.clear();
        this.fragmentIds.clear();

        for (int i = 0; i < fts.size(); i++) {
            Fragment f = fts.get(i);
            this.fragments.add(f);
            this.fragmentIds.add(getStableId(f, i));
        }
    }

    public void addFragment(Fragment ft) {
        long id = getStableId(ft, this.fragments.size());
        this.fragments.add(ft);
        this.fragmentIds.add(id);
    }

    public List<Fragment> getFragments() {
        return fragments;
    }

    public void removeFragment(int position) {
        if (position >= 0 && position < fragments.size()) {
            fragments.remove(position);
            fragmentIds.remove(position);
            notifyItemRemoved(position);
        }
    }

    /**
     * Remove Fragments of the specified class<br>
     * <b>
     * note that if they have the same name, ALL be deleted
     * </b>
     *
     * @param clz class of the fragment to be removed
     * @return index of the removed fragment, or -1 if not found
     */
    public boolean removeAllByClass(Class<?> clz) {
        if (CollectionUtils.isEmpty(fragments)) {
            return false;
        }

        String cName = clz.getName();
        boolean removed = false;
        for (int i = fragments.size() - 1; i >= 0; i--) {
            if (cName.equals(fragments.get(i).getClass().getName())) {
                removeFragment(i);
                removed = true;
            }
        }
        return removed;
    }

    /**
     * Remove Fragments of the specified class<br>
     * <b>
     * note that if they have the same name, only the first one can be deleted
     * </b>
     *
     * @param clz class of the fragment to be removed
     * @return index of the removed fragment, or -1 if not found
     */
    public int removeByClass(Class<?> clz) {
        int i = indexByClass(clz);
        if (i == -1) {
            return i;
        }

        removeFragment(i);

        return i;
    }

    public int indexByClass(Class<?> clz) {
        if (CollectionUtils.isEmpty(fragments)) {
            return -1;
        }

        for (int i = 0; i < fragments.size(); i++) {
            String n = fragments.get(i).getClass().getName();
            if (clz.getName().equals(n)) {
                return i;
            }
        }

        return -1;
    }

    @NonNull
    @Override
    public Fragment createFragment(int position) {
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
        return fragmentIds.contains(itemId);
    }

    private long getStableId(Fragment fragment, int position) {
        long id = computeBaseId(fragment, position);
        while (fragmentIds.contains(id)) {
            id++;
        }
        return id;
    }

    private long computeBaseId(Fragment fragment, int position) {
        if (fragment != null) {
            Bundle args = fragment.getArguments();
            if (args != null) {
                String url = args.getString("image_url");
                if (url != null) {
                    return url.hashCode();
                }
                String repoId = args.getString("repoId");
                String fullPath = args.getString("fullPath");
                if (repoId != null && fullPath != null) {
                    return (repoId + ":" + fullPath).hashCode();
                }
            }
            return fragment.getClass().getName().hashCode();
        }
        return position;
    }
}
