package com.seafile.seadroid2.ui.repo.repo_view;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.ItemRepoViewBinding;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.framework.model.repo.views.RepoViewModel;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.widget.prefs.background_pref.BackgroundShapeUtils;

public class RepoViewAdapter extends BaseAdapter<RepoViewModel, RepoViewHolder> {

    private Drawable topShapeBackgroundDrawable;
    private Drawable bottomShapeBackgroundDrawable;
    private Drawable allShapeBackgroundDrawable;
    private Drawable noneShapeBackgroundDrawable;

    @Override
    public void onAttachedToRecyclerView(@NonNull RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);

        int itemBackColor = ContextCompat.getColor(getContext(), R.color.bar_background_color);

        topShapeBackgroundDrawable = BackgroundShapeUtils.genBackgroundDrawable(BackgroundShapeUtils.SHAPE_TOP, itemBackColor, Constants.DP.DP_8);
        bottomShapeBackgroundDrawable = BackgroundShapeUtils.genBackgroundDrawable(BackgroundShapeUtils.SHAPE_BOTTOM, itemBackColor, Constants.DP.DP_8);
        allShapeBackgroundDrawable = BackgroundShapeUtils.genBackgroundDrawable(BackgroundShapeUtils.SHAPE_ALL, itemBackColor, Constants.DP.DP_8);
        noneShapeBackgroundDrawable = BackgroundShapeUtils.genBackgroundDrawable(BackgroundShapeUtils.SHAPE_NONE, itemBackColor, Constants.DP.DP_8);

    }

    @Override
    protected void onBindViewHolder(@NonNull RepoViewHolder holder, int i, @Nullable RepoViewModel model) {
        onBind(holder, model, i);
    }

    @NonNull
    @Override
    protected RepoViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemRepoViewBinding binding = ItemRepoViewBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new RepoViewHolder(binding);
    }

    protected void onBind(RepoViewHolder holder, RepoViewModel model, int position) {
        holder.binding.itemIcon.setImageResource(getIconByType(model.type));
        holder.binding.itemTitle.setText(model.name);

        //set background color for item
        if (position == 0) {
            holder.itemView.setBackground(topShapeBackgroundDrawable);
        } else if (position == getItemCount() - 1) {
            holder.itemView.setBackground(bottomShapeBackgroundDrawable);
        } else {
            holder.itemView.setBackground(noneShapeBackgroundDrawable);
        }

        //hide divider for bottom item
        if (position == getItemCount() - 1) {
            holder.binding.divider.setVisibility(View.GONE);
        } else {
            holder.binding.divider.setVisibility(View.VISIBLE);
        }
    }

    private int getIconByType(String t) {
        if ("card".equals(t)) {
            return R.drawable.icon_view_card_view;
        } else if ("table".equals(t)) {
            return R.drawable.icon_view_table_view;
        } else if ("gallery".equals(t)) {
            return R.drawable.icon_view_gallery_view;
        } else if ("kanban".equals(t)) {
            return R.drawable.icon_view_kanban_view;
        } else if ("map".equals(t)) {
            return R.drawable.icon_view_map_view;
        } else if ("statistics".equals(t)) {
            return R.drawable.icon_view_statistics;
        }
        return R.drawable.icon_view_table_view;
    }
}
