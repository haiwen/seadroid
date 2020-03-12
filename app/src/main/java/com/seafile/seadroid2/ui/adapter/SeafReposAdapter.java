package com.seafile.seadroid2.ui.adapter;

import android.widget.ImageView;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.SeafRepo;

import java.util.Collections;
import java.util.List;

/**
 * Repos adapter
 */
public class SeafReposAdapter extends ReposAdapter {

    public SeafReposAdapter(boolean onlyShowWritableRepos, String encryptedRepoId) {
        super(onlyShowWritableRepos, encryptedRepoId);
    }

    /** sort files type */
    public static final int SORT_BY_NAME = 9;
    /** sort files type */
    public static final int SORT_BY_LAST_MODIFIED_TIME = 10;
    /** sort files order */
    public static final int SORT_ORDER_ASCENDING = 11;
    /** sort files order */
    public static final int SORT_ORDER_DESCENDING = 12;


    @Override
    public int getCount() {
        return repos.size();
    }

    @Override
    public boolean isEmpty() {
        return repos.isEmpty();
    }

    public void add(SeafRepo repo) {
        repos.add(repo);
    }

    @Override
    public SeafRepo getItem(int position) {
        return repos.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public void clearRepos() {
        repos.clear();
    }

    public void sortFiles(int type, int order) {
        List<SeafRepo> folders = Lists.newArrayList();

        for (SeafRepo item : repos) {
            folders.add(item);
        }
        repos.clear();

        if (type == SORT_BY_NAME) {
            // sort by name, in ascending order
            Collections.sort(folders, new SeafRepo.RepoNameComparator());
            if (order == SORT_ORDER_DESCENDING) {
                Collections.reverse(folders);
            }
        } else if (type == SORT_BY_LAST_MODIFIED_TIME) {
            // sort by last modified time, in ascending order
            Collections.sort(folders, new SeafRepo.RepoLastMTimeComparator());
            if (order == SORT_ORDER_DESCENDING) {
                Collections.reverse(folders);
            }
        }
        // Adds the objects in the specified collection to this ArrayList
        repos.addAll(folders);
    }

    @Override
    protected int getChildLayout() {
        return R.layout.repo_list_item;
    }

    @Override
    protected int getChildTitleId() {
        return R.id.repo_list_item_title;
    }

    @Override
    protected int getChildSubTitleId() {
        return R.id.repo_list_item_subtitle;
    }

    @Override
    protected int getChildIconId() {
        return R.id.repo_list_item_icon;
    }

    @Override
    protected int getChildActionId() {
        return R.id.repo_list_item_action;
    }

    @Override
    protected SeafRepo getChildSeafRepo(int position) {
        return repos.get(position);
    }

    @Override
    protected void showRepoSelectedIcon(int position, ImageView imageView) {
        return;
    }
}
