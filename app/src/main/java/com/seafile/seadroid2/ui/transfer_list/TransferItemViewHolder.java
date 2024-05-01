package com.seafile.seadroid2.ui.transfer_list;

import androidx.annotation.NonNull;

import com.seafile.seadroid2.databinding.ItemTransferListBinding;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class TransferItemViewHolder extends BaseViewHolder {
    public ItemTransferListBinding binding;

    public TransferItemViewHolder(@NonNull ItemTransferListBinding binding) {
        super(binding.getRoot());

        this.binding = binding;
    }
}
