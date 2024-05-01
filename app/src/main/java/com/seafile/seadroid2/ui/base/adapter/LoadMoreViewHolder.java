package com.seafile.seadroid2.ui.base.adapter;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.seafile.seadroid2.databinding.ViewLoadMoreBinding;

public class LoadMoreViewHolder extends RecyclerView.ViewHolder {
    public ViewLoadMoreBinding viewBinding;

    public LoadMoreViewHolder(@NonNull ViewLoadMoreBinding binding) {
        super(binding.getRoot());
        viewBinding = binding;
    }
}
