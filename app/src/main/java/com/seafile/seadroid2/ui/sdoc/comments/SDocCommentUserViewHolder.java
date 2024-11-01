package com.seafile.seadroid2.ui.sdoc.comments;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemUserAvatarBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class SDocCommentUserViewHolder extends BaseViewHolder {

    public ItemUserAvatarBinding binding;

    public SDocCommentUserViewHolder(@NonNull ItemUserAvatarBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
