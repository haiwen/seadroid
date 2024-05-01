package com.seafile.seadroid2.ui.repo;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.databinding.ItemRepoBinding;

public class RepoViewHolder extends BaseViewHolder {
    public ItemRepoBinding binding;

    public RepoViewHolder(@NonNull ItemRepoBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
