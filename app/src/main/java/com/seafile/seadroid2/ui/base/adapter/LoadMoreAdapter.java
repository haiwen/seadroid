package com.seafile.seadroid2.ui.base.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;

import com.chad.library.adapter4.loadState.LoadState;
import com.chad.library.adapter4.loadState.trailing.TrailingLoadStateAdapter;
import com.seafile.seadroid2.databinding.ViewLoadMoreBinding;

public class LoadMoreAdapter extends TrailingLoadStateAdapter<LoadMoreViewHolder> {

    @Override
    public void onBindViewHolder(@NonNull LoadMoreViewHolder holder, @NonNull LoadState loadState) {

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
    public LoadMoreViewHolder onCreateViewHolder(@NonNull ViewGroup viewGroup, @NonNull LoadState loadState) {
        ViewLoadMoreBinding viewBinding = ViewLoadMoreBinding.inflate(LayoutInflater.from(viewGroup.getContext()), viewGroup, false);
        viewBinding.loadMoreLoadFailView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                invokeFailRetry();
            }
        });
        return new LoadMoreViewHolder(viewBinding);
    }
}
