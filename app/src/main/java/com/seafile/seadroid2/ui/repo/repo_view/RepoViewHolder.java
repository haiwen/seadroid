package com.seafile.seadroid2.ui.repo.repo_view;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemAccountBinding;
import com.seafile.seadroid2.databinding.ItemRepoViewBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class RepoViewHolder extends BaseViewHolder {
    public ItemRepoViewBinding binding;

    public RepoViewHolder(@NonNull ItemRepoViewBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
