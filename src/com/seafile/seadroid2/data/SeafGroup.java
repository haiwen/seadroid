package com.seafile.seadroid2.data;

import com.google.common.collect.Lists;
import com.seafile.seadroid2.ui.adapter.SeafItemAdapter;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class SeafGroup implements SeafItem {
    private String name;
    private List<SeafRepo> repos = Lists.newArrayList();

    public SeafGroup(String name) {
        this.name = name;
    }

    @Override
    public String getTitle() {
        return name;
    }

    @Override
    public String getSubtitle() {
        return null;
    }

    @Override
    public int getIcon() {
        return 0;
    }

    public List<SeafRepo> getRepos() {
        return repos;
    }

    public void addIfAbsent(SeafRepo repo) {
        if (!repos.contains(repo))
            this.repos.add(repo);
    }

    /**
     * sort collections by repository name or modification time
     */
    public void sortByType(int type) {
        if (type == SeafItemAdapter.SORT_BY_NAME) {
            Collections.sort(repos, new RepoNameComparator());
        } else if (type == SeafItemAdapter.SORT_BY_MODIFICATION_TIME) {
            Collections.sort(repos, new RepoMTimeComparator());
        }
    }

    /**
     * Repository modification time comparator class
     */
    private class RepoMTimeComparator implements Comparator<SeafRepo> {

        @Override
        public int compare(SeafRepo itemA, SeafRepo itemB) {
            return (int) (itemB.mtime - itemA.mtime);
        }
    }

    /**
     * Repository name comparator class
     */
    private class RepoNameComparator implements Comparator<SeafRepo> {

        @Override
        public int compare(SeafRepo itemA, SeafRepo itemB) {
            return itemA.name.toLowerCase().compareTo(itemB.name.toLowerCase());
        }
    }

}
