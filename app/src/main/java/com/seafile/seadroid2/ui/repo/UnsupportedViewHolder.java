package com.seafile.seadroid2.ui.repo;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.databinding.ItemUnsupportedBinding;

public class UnsupportedViewHolder extends BaseViewHolder {
    public ItemUnsupportedBinding binding;

    public UnsupportedViewHolder(@NonNull ItemUnsupportedBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
