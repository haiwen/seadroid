package com.seafile.seadroid2.ui.selector.versatile;


import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;

import com.google.android.material.checkbox.MaterialCheckBox;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.ItemSelectorRecentlyUsedBinding;
import com.seafile.seadroid2.databinding.ItemStarredBinding;
import com.seafile.seadroid2.framework.model.versatile.RecentlyUsedModel;
import com.seafile.seadroid2.framework.util.FileUtils;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.listener.OnFileItemChangeListener;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.ui.selector.folder_selector.FileBean;
import com.seafile.seadroid2.ui.selector.folder_selector.FileListViewHolder;

import org.apache.commons.io.FilenameUtils;

public class RecentlyUsedListAdapter extends BaseAdapter<RecentlyUsedModel, RecentlyUsedListAdapter.RecentlyUsedListViewHolder> {

    private ItemSelectorRecentlyUsedBinding binding;

    @NonNull
    @Override
    protected RecentlyUsedListAdapter.RecentlyUsedListViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        binding = ItemSelectorRecentlyUsedBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new RecentlyUsedListViewHolder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull RecentlyUsedListViewHolder holder, int i, @Nullable RecentlyUsedModel model) {
        if (model == null) {
            holder.binding.itemTitle.setText(null);
            holder.binding.itemSubtitle.setText(null);
            holder.binding.itemSubtitle.setVisibility(View.GONE);
        } else if (TextUtils.equals("/", model.path)) {
            holder.binding.itemTitle.setText(model.repoName);

            holder.binding.itemSubtitle.setText(null);
            holder.binding.itemSubtitle.setVisibility(View.GONE);
        } else {
            if (!TextUtils.equals("/", model.path) && model.path.endsWith("/")) {
                model.path = model.path.substring(0, model.path.length() - 1);
            }
            String name = FilenameUtils.getName(model.path);
            holder.binding.itemTitle.setText(name);

            holder.binding.itemSubtitle.setVisibility(View.VISIBLE);
            holder.binding.itemSubtitle.setText(model.repoName);
        }

        holder.binding.itemSelectView.setVisibility(model.isSelected ? View.VISIBLE : View.INVISIBLE);
    }


    public static class RecentlyUsedListViewHolder extends BaseViewHolder {
        public ItemSelectorRecentlyUsedBinding binding;

        public RecentlyUsedListViewHolder(ItemSelectorRecentlyUsedBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }

}