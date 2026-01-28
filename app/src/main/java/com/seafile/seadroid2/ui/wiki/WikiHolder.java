package com.seafile.seadroid2.ui.wiki;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemWikiBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class WikiHolder extends BaseViewHolder {
    public ItemWikiBinding binding;
    public WikiHolder(@NonNull ItemWikiBinding binding) {
        super(binding.getRoot());
        this.binding = binding;
    }
}
