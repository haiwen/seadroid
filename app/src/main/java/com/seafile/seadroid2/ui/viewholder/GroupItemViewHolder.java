package com.seafile.seadroid2.ui.viewholder;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.databinding.ItemGroupItemBinding;

public class GroupItemViewHolder extends BaseViewHolder {
    public ItemGroupItemBinding binding;

    public GroupItemViewHolder(@NonNull ItemGroupItemBinding binding) {
        super(binding.getRoot());
        this.binding = binding;
    }
}
