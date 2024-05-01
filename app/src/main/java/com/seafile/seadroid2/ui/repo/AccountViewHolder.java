package com.seafile.seadroid2.ui.repo;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.databinding.ItemAccountBinding;

public class AccountViewHolder extends BaseViewHolder {
    public ItemAccountBinding binding;

    public AccountViewHolder(@NonNull ItemAccountBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
