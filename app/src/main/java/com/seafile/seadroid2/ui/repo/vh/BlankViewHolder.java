package com.seafile.seadroid2.ui.repo.vh;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemBlankBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class BlankViewHolder extends BaseViewHolder {
    public ItemBlankBinding binding;

    public BlankViewHolder(@NonNull ItemBlankBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
