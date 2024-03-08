package com.seafile.seadroid2.ui.search;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.Nullable;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.model.search.SearchModel;
import com.seafile.seadroid2.databinding.ItemSearchBinding;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.util.Utils;

import org.jetbrains.annotations.NotNull;

public class SearchRecyclerViewAdapter extends BaseAdapter<SearchModel, SearchRecyclerViewAdapter.SearchItemViewHolder> {
    private Context context;

    public SearchRecyclerViewAdapter(Context context) {
        this.context = context;
    }

    @Override
    protected void onBindViewHolder(@NotNull SearchItemViewHolder holder, int i, @Nullable SearchModel model) {
        holder.binding.icon.setImageResource(model.getIcon());
        holder.binding.title.setText(model.getTitle());
        holder.binding.subtitle.setText(model.getSubtitle());
    }

    @NotNull
    @Override
    protected SearchItemViewHolder onCreateViewHolder(@NotNull Context context, @NotNull ViewGroup viewGroup, int i) {
        ItemSearchBinding binding = ItemSearchBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new SearchItemViewHolder(binding);
    }

    public static class SearchItemViewHolder extends BaseViewHolder {
        public ItemSearchBinding binding;

        public SearchItemViewHolder(ItemSearchBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }

    }
}
