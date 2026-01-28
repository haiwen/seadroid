package com.seafile.seadroid2.ui.viewholder;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemGroupItemBinding;
import com.seafile.seadroid2.databinding.ItemWikiGroupItemBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class WikiGroupItemViewHolder extends BaseViewHolder {
    public ItemWikiGroupItemBinding binding;

    public WikiGroupItemViewHolder(@NonNull ItemWikiGroupItemBinding binding) {
        super(binding.getRoot());
        this.binding = binding;
    }
}
