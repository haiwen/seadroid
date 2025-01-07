package com.seafile.seadroid2.ui.repo.vh;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemDirentBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class DirentViewHolder extends BaseViewHolder {
    public ItemDirentBinding binding;

    public DirentViewHolder(@NonNull ItemDirentBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
