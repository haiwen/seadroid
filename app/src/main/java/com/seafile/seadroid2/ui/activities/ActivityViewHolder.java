package com.seafile.seadroid2.ui.activities;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.databinding.ItemActivityBinding;

public class ActivityViewHolder extends BaseViewHolder {
    public ItemActivityBinding binding;

    public ActivityViewHolder(@NonNull ItemActivityBinding binding) {
        super(binding.getRoot());
        this.binding = binding;
    }
}
