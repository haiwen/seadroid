package com.seafile.seadroid2.ui.docs_comment;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemUserAvatarBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class DocsCommentUserViewHolder extends BaseViewHolder {

    public ItemUserAvatarBinding binding;

    public DocsCommentUserViewHolder(@NonNull ItemUserAvatarBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
