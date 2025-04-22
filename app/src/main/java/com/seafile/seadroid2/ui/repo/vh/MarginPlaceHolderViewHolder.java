package com.seafile.seadroid2.ui.repo.vh;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemBlankBinding;
import com.seafile.seadroid2.databinding.ViewMarginPlaceholderBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class MarginPlaceHolderViewHolder extends BaseViewHolder {
    public ViewMarginPlaceholderBinding binding;

    public MarginPlaceHolderViewHolder(@NonNull ViewMarginPlaceholderBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
