package com.seafile.seadroid2.ui.sdoc.comments;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemSdocCommentBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class SDocCommentViewHolder extends BaseViewHolder {

    public ItemSdocCommentBinding binding;

    public SDocCommentViewHolder(@NonNull ItemSdocCommentBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
