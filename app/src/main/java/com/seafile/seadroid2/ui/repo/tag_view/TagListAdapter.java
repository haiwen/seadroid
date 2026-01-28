package com.seafile.seadroid2.ui.repo.tag_view;

import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.Constants;
import com.seafile.seadroid2.databinding.ItemTagListBinding;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.framework.model.repo.tags.TagResultModel;
import com.seafile.seadroid2.framework.model.repo.views.RepoViewModel;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.repo.repo_view.RepoViewHolder;
import com.seafile.seadroid2.widget.prefs.background_pref.BackgroundShapeUtils;

public class TagListAdapter extends BaseAdapter<TagResultModel, TagListHolder> {
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
    protected void onBindViewHolder(@NonNull TagListHolder holder, int i, @Nullable TagResultModel model) {
        onBind(holder, model, i);
    }

    @NonNull
    @Override
    protected TagListHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemTagListBinding binding = ItemTagListBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new TagListHolder(binding);
    }

    protected void onBind(TagListHolder holder, TagResultModel model, int position) {
        holder.binding.itemTitle.setText(model._tag_name);

        //set background color for item
        if (position == 0) {
            holder.itemView.setBackground(topShapeBackgroundDrawable);
        } else if (position == getItemCount() - 1) {
            holder.itemView.setBackground(bottomShapeBackgroundDrawable);
        } else {
            holder.itemView.setBackground(noneShapeBackgroundDrawable);
        }

        if (TextUtils.isEmpty(model._tag_color)){
            holder.binding.itemIcon.setCardBackgroundColor(Color.parseColor("#FFFCB5"));
        }else{
            holder.binding.itemIcon.setCardBackgroundColor(Color.parseColor(model._tag_color));
        }

        if (CollectionUtils.isEmpty(model._tag_file_links)){
            holder.binding.itemSubtitle.setText("0");
        }else {
            holder.binding.itemSubtitle.setText(model._tag_file_links.size()+"");
        }

        //hide divider for bottom item
        if (position == getItemCount() - 1) {
            holder.binding.divider.setVisibility(View.GONE);
        } else {
            holder.binding.divider.setVisibility(View.VISIBLE);
        }
    }
}

