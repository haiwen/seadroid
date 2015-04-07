package com.seafile.seadroid2.cameraupload;

import android.view.View;
import android.widget.ImageView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.ui.adapter.ReposAdapter;

import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Cloud library adapter
 */
public class CloudLibraryAdapter extends ReposAdapter {

    protected LinkedHashMap<SeafRepo, Boolean> repos = new LinkedHashMap<SeafRepo, Boolean>();

    public CloudLibraryAdapter(boolean onlyShowWritableRepos, String encryptedRepoId) {
        super(onlyShowWritableRepos, encryptedRepoId);
    }

    @Override
    protected int getChildLayout() {
        return R.layout.cuc_repo_list_item;
    }

    @Override
    protected int getChildTitleId() {
        return R.id.cuc_repo_list_item_title;
    }

    @Override
    protected int getChildSubTitleId() {
        return R.id.cuc_repo_list_item_subtitle;
    }

    @Override
    protected int getChildIconId() {
        return R.id.cuc_repo_list_item_icon;
    }

    @Override
    protected int getChildActionId() {
        return R.id.cuc_repo_list_item_action;
    }

    @Override
    protected SeafRepo getChildSeafRepo(int position) {
        return new ArrayList<SeafRepo>(repos.keySet()).get(position);
    }

    @Override
    public boolean isEnabled(int position) {
        // if repo is encrypted, disable it
        // because camera upload service doesn`t support it
        return !(new ArrayList<SeafRepo>(repos.keySet()).get(position).encrypted);
    }

    @Override
    protected void showRepoSelectedIcon(int position, ImageView imageView) {
        boolean isChecked = new ArrayList<Boolean>(repos.values()).get(position);
        if (isChecked)
            imageView.setVisibility(View.VISIBLE);
        else
            imageView.setVisibility(View.INVISIBLE);
    }


    public void setRepos(List<SeafRepo> reposMap) {
        this.repos.clear();
        for (SeafRepo repo : reposMap) {
            if (onlyShowWritableRepos && !repo.hasWritePermission()) {
                continue;
            }
            if (encryptedRepoId != null && !repo.id.equals(encryptedRepoId)) {
                continue;
            }
            this.repos.put(repo, false);
        }
        notifyDataSetChanged();
    }

    public void setRepo(SeafRepo repo, boolean isChecked) {
        for (SeafRepo key : repos.keySet()) {
            repos.put(key, false);
        }
        // only check one item
        repos.put(repo, isChecked);
        notifyDataSetChanged();
    }

    @Override
    public int getCount() {
        return repos.size();
    }

    @Override
    public boolean isEmpty() {
        return repos.isEmpty();
    }

    public void add(SeafRepo repo) {
        repos.put(repo, false);
    }

    @Override
    public SeafRepo getItem(int position) {
        return new ArrayList<SeafRepo>(repos.keySet()).get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

}
