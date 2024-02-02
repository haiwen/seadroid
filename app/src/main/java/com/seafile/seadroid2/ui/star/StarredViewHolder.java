package com.seafile.seadroid2.ui.star;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.databinding.ItemStarredBinding;

public class StarredViewHolder extends BaseViewHolder {
    public ItemStarredBinding binding;

    public StarredViewHolder(@NonNull ItemStarredBinding binding) {
        super(binding.getRoot());
        this.binding = binding;
    }
}
