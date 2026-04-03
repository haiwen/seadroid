package com.seafile.seadroid2.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.chad.library.adapter4.loadState.LoadState;
import com.chad.library.adapter4.loadState.trailing.TrailingLoadStateAdapter;
import com.seafile.seadroid2.databinding.ViewLoadMoreBinding;

public class CustomLoadMoreAdapter extends TrailingLoadStateAdapter<CustomLoadMoreAdapter.CustomVH> {

    @Override
    public void onBindViewHolder(@NonNull CustomVH holder, @NonNull LoadState loadState) {
        if (loadState instanceof LoadState.NotLoading) {
            if (loadState.getEndOfPaginationReached()) {
                holder.viewBinding.loadMoreLoadCompleteView.setVisibility(View.GONE);
                holder.viewBinding.loadMoreLoadingView.setVisibility(View.GONE);
                holder.viewBinding.loadMoreLoadFailView.setVisibility(View.GONE);
                holder.viewBinding.loadMoreLoadEndView.setVisibility(View.VISIBLE);
            } else {
                holder.viewBinding.loadMoreLoadCompleteView.setVisibility(View.VISIBLE);
                holder.viewBinding.loadMoreLoadingView.setVisibility(View.GONE);
                holder.viewBinding.loadMoreLoadFailView.setVisibility(View.GONE);
                holder.viewBinding.loadMoreLoadEndView.setVisibility(View.GONE);
            }
        } else if (loadState instanceof LoadState.Loading) {
            holder.viewBinding.loadMoreLoadCompleteView.setVisibility(View.GONE);
            holder.viewBinding.loadMoreLoadingView.setVisibility(View.VISIBLE);
            holder.viewBinding.loadMoreLoadFailView.setVisibility(View.GONE);
            holder.viewBinding.loadMoreLoadEndView.setVisibility(View.GONE);
        } else if (loadState instanceof LoadState.Error) {
            holder.viewBinding.loadMoreLoadCompleteView.setVisibility(View.GONE);
            holder.viewBinding.loadMoreLoadingView.setVisibility(View.GONE);
            holder.viewBinding.loadMoreLoadFailView.setVisibility(View.VISIBLE);
            holder.viewBinding.loadMoreLoadEndView.setVisibility(View.GONE);
        } else if (loadState instanceof LoadState.None) {
            holder.viewBinding.loadMoreLoadCompleteView.setVisibility(View.GONE);
            holder.viewBinding.loadMoreLoadingView.setVisibility(View.GONE);
            holder.viewBinding.loadMoreLoadFailView.setVisibility(View.GONE);
            holder.viewBinding.loadMoreLoadEndView.setVisibility(View.GONE);
        }
    }

    @NonNull
    @Override
    public CustomVH onCreateViewHolder(@NonNull ViewGroup viewGroup, @NonNull LoadState loadState) {
        ViewLoadMoreBinding viewBinding = ViewLoadMoreBinding.inflate(LayoutInflater.from(viewGroup.getContext()), viewGroup, false);
        return new CustomVH(viewBinding);
    }

    public static class CustomVH extends RecyclerView.ViewHolder {
        public ViewLoadMoreBinding viewBinding;

        public CustomVH(@NonNull ViewLoadMoreBinding binding) {
            super(binding.getRoot());
            viewBinding = binding;
        }
    }
}
