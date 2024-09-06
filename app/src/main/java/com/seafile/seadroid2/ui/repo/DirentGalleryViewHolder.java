package com.seafile.seadroid2.ui.repo;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemDirentGalleryBinding;
import com.seafile.seadroid2.databinding.ItemDirentGridBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class DirentGalleryViewHolder extends BaseViewHolder {
    public ItemDirentGalleryBinding binding;

    public DirentGalleryViewHolder(@NonNull ItemDirentGalleryBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
