package com.seafile.seadroid2.ui.repo.vh;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemDirentGridBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class DirentGridViewHolder extends BaseViewHolder {
    public ItemDirentGridBinding binding;

    public DirentGridViewHolder(@NonNull ItemDirentGridBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
