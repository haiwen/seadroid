package com.seafile.seadroid2.ui.docs_comment;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemFileCommentBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class DocsCommentViewHolder extends BaseViewHolder {

    public ItemFileCommentBinding binding;

    public DocsCommentViewHolder(@NonNull ItemFileCommentBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
