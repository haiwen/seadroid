package com.seafile.seadroid2.ui.repo.tag_view;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemRepoViewBinding;
import com.seafile.seadroid2.databinding.ItemTagListBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class TagListHolder extends BaseViewHolder {
    public ItemTagListBinding binding;

    public TagListHolder(@NonNull ItemTagListBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
