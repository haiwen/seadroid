package com.seafile.seadroid2.ui.adapter;

import androidx.annotation.NonNull;
import androidx.collection.LongSparseArray;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentActivity;
import androidx.fragment.app.FragmentManager;
import androidx.lifecycle.Lifecycle;
import androidx.viewpager2.adapter.FragmentStateAdapter;

import com.blankj.utilcode.util.CollectionUtils;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

public class ViewPager2Adapter extends FragmentStateAdapter {
    private final List<Fragment> fragments = new ArrayList<>();
    private final List<Long> fragmentIds = new ArrayList<>();

    private final SparseLongCompat itemIds = new SparseLongCompat();
    private static final AtomicLong nextId = new AtomicLong();

    public ViewPager2Adapter(FragmentActivity fa) {
        super(fa);
    }

    public void addFragments(List<Fragment> fts) {
        this.fragments.clear();
        this.fragmentIds.clear();

        for (int i = 0; i < fts.size(); i++) {
            this.fragments.add(fts.get(i));
            this.fragmentIds.add(itemIds.get(i));
        }
    }

    public void addFragment(Fragment ft) {
        this.fragments.add(ft);
        this.fragmentIds.add(itemIds.get(this.fragments.size()));
    }

    public List<Fragment> getFragments() {
        return fragments;
    }

    public void removeFragment(int position) {
        fragments.remove(position);
        fragmentIds.remove(position);

        if (position != -1) {
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
        return fragments.removeIf(fragment -> cName.equals(fragment.getClass().getName()));
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

    private static class SparseLongCompat {
        private final LongSparseArray<Long> mKeyToIdMap = new LongSparseArray<>();
        private final LongSparseArray<Integer> mIdToKeyMap = new LongSparseArray<>();

        public long get(int key) {
            int index = mKeyToIdMap.indexOfKey(key);
            if (index >= 0) {
                return mKeyToIdMap.valueAt(index);
            }

            long id = nextId.getAndIncrement();
            mKeyToIdMap.put(key, id);
            mIdToKeyMap.put(id, key);
            return id;
        }
    }
}