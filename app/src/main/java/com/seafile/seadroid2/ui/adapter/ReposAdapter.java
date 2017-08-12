package com.seafile.seadroid2.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.SeadroidApplication;
import com.seafile.seadroid2.data.SeafRepo;

import java.util.List;

/**
 * Base ReposAdapter
 */
public abstract class ReposAdapter extends BaseAdapter {

    protected List<SeafRepo> repos = Lists.newArrayList();
    protected boolean onlyShowWritableRepos;
    protected String encryptedRepoId;

    public ReposAdapter(boolean onlyShowWritableRepos, String encryptedRepoId) {
        this.onlyShowWritableRepos = onlyShowWritableRepos;
        this.encryptedRepoId = encryptedRepoId;
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public boolean areAllReposSelectable() {
        return false;
    }

    public  List<SeafRepo> getData() {
        return repos;
    }

    public void setRepos(List<SeafRepo> seafRepos) {
        repos.clear();
        for (SeafRepo repo: seafRepos) {
            if (onlyShowWritableRepos && !repo.hasWritePermission()) {
                continue;
            }
            if (encryptedRepoId != null && !repo.id.equals(encryptedRepoId)) {
                continue;
            }
            repos.add(repo);
        }
        notifyDataSetChanged();
    }

    protected abstract int getChildLayout();

    protected abstract int getChildTitleId();

    protected abstract int getChildSubTitleId();

    protected abstract int getChildIconId();

    protected abstract int getChildActionId();

    protected abstract SeafRepo getChildSeafRepo(int position);

    protected abstract void showRepoSelectedIcon(int position, ImageView imageView);

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        View view = convertView;
        Viewholder viewHolder;

        SeafRepo repo = getChildSeafRepo(position);

        if (convertView == null) {
            view = LayoutInflater.from(SeadroidApplication.getAppContext())
                    .inflate(getChildLayout(), null);
            TextView title = (TextView) view.findViewById(getChildTitleId());
            TextView subtitle = (TextView) view.findViewById(getChildSubTitleId());
            ImageView icon = (ImageView) view.findViewById(getChildIconId());
            ImageView action = (ImageView) view.findViewById(getChildActionId());
            viewHolder = new Viewholder(title, subtitle, icon, action);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        viewHolder.title.setText(repo.getTitle());
        viewHolder.subtitle.setText(repo.getSubtitle());
        viewHolder.icon.setImageResource(repo.getIcon());
        viewHolder.action.setVisibility(View.INVISIBLE);

        showRepoSelectedIcon(position, viewHolder.action);
        return view;
    }

    private class Viewholder {
        TextView title, subtitle;
        ImageView icon, action;

        public Viewholder(TextView title, TextView subtitle, ImageView icon, ImageView action) {
            super();
            this.icon = icon;
            this.action = action;
            this.title = title;
            this.subtitle = subtitle;
        }
    }
}
