package com.seafile.seadroid2.ui.base.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.chad.library.adapter.base.loadState.LoadState;
import com.chad.library.adapter.base.loadState.trailing.TrailingLoadStateAdapter;
import com.seafile.seadroid2.R;

public class CustomLoadMoreAdapter extends TrailingLoadStateAdapter<CustomLoadMoreAdapter.CustomVH> {

    @Override
    public void onBindViewHolder(@NonNull CustomVH holder, @NonNull LoadState loadState) {
        if (loadState instanceof LoadState.NotLoading) {
            if (loadState.getEndOfPaginationReached()) {
                holder.loadMoreLoadCompleteView.setVisibility(View.GONE);
                holder.loadMoreLoadingView.setVisibility(View.GONE);
                holder.loadMoreLoadFailView.setVisibility(View.GONE);
                holder.loadMoreLoadEndView.setVisibility(View.VISIBLE);
            } else {
                holder.loadMoreLoadCompleteView.setVisibility(View.VISIBLE);
                holder.loadMoreLoadingView.setVisibility(View.GONE);
                holder.loadMoreLoadFailView.setVisibility(View.GONE);
                holder.loadMoreLoadEndView.setVisibility(View.GONE);
            }
        } else if (loadState instanceof LoadState.Loading) {
            holder.loadMoreLoadCompleteView.setVisibility(View.GONE);
            holder.loadMoreLoadingView.setVisibility(View.VISIBLE);
            holder.loadMoreLoadFailView.setVisibility(View.GONE);
            holder.loadMoreLoadEndView.setVisibility(View.GONE);
        } else if (loadState instanceof LoadState.Error) {
            holder.loadMoreLoadCompleteView.setVisibility(View.GONE);
            holder.loadMoreLoadingView.setVisibility(View.GONE);
            holder.loadMoreLoadFailView.setVisibility(View.VISIBLE);
            holder.loadMoreLoadEndView.setVisibility(View.GONE);
        } else if (loadState instanceof LoadState.None) {
            holder.loadMoreLoadCompleteView.setVisibility(View.GONE);
            holder.loadMoreLoadingView.setVisibility(View.GONE);
            holder.loadMoreLoadFailView.setVisibility(View.GONE);
            holder.loadMoreLoadEndView.setVisibility(View.GONE);
        }
    }

    @NonNull
    @Override
    public CustomVH onCreateViewHolder(@NonNull ViewGroup viewGroup, @NonNull LoadState loadState) {
        View view = LayoutInflater.from(viewGroup.getContext()).inflate(R.layout.view_load_more, viewGroup, false);
        return new CustomVH(view);
    }

    public static class CustomVH extends RecyclerView.ViewHolder {

        public View loadMoreLoadCompleteView;
        public View loadMoreLoadingView;
        public View loadMoreLoadFailView;
        public View loadMoreLoadEndView;

        public CustomVH(@NonNull View view) {
            super(view);

            loadMoreLoadingView = view.findViewById(R.id.load_more_loading_view);
            loadMoreLoadCompleteView = view.findViewById(R.id.load_more_load_complete_view);
            loadMoreLoadFailView = view.findViewById(R.id.load_more_load_fail_view);
            loadMoreLoadEndView = view.findViewById(R.id.load_more_load_end_view);
        }
    }
}
