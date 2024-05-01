package com.seafile.seadroid2.ui.repo;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.databinding.ItemDirentBinding;

public class DirentViewHolder extends BaseViewHolder {
    public ItemDirentBinding binding;

    public DirentViewHolder(@NonNull ItemDirentBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
