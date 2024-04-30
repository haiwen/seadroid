package com.seafile.seadroid2.ui.activities;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.SpanUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.framework.util.GlideApp;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.ui.viewholder.GroupItemViewHolder;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.framework.data.model.BaseModel;
import com.seafile.seadroid2.framework.data.model.GroupItemModel;
import com.seafile.seadroid2.framework.data.model.activities.ActivityModel;
import com.seafile.seadroid2.databinding.ItemActivityBinding;
import com.seafile.seadroid2.databinding.ItemGroupItemBinding;
import com.seafile.seadroid2.framework.util.SystemSwitchUtils;

import java.util.List;

public class ActivityAdapter extends BaseMultiAdapter<BaseModel> {

    public ActivityAdapter() {
        addItemType(AbsLayoutItemType.GROUP_ITEM, new OnMultiItem<BaseModel, RecyclerView.ViewHolder>() {
            @NonNull
            @Override
            public GroupItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemGroupItemBinding binding = ItemGroupItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new GroupItemViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull RecyclerView.ViewHolder viewHolder, int i, @Nullable BaseModel baseModel) {

            }
        }).addItemType(AbsLayoutItemType.ACTIVITY, new OnMultiItem<BaseModel, ActivityViewHolder>() {
            @NonNull
            @Override
            public ActivityViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemActivityBinding binding = ItemActivityBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new ActivityViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull ActivityViewHolder holder, int i, @Nullable BaseModel activityModel) {
                onBindActivity(holder, (ActivityModel) activityModel);
            }
        }).onItemViewType(new OnItemViewTypeListener<BaseModel>() {
            @Override
            public int onItemViewType(int i, @NonNull List<? extends BaseModel> list) {
                if (list.get(i) instanceof ActivityModel) {
                    return AbsLayoutItemType.ACTIVITY;
                } else if (list.get(i) instanceof GroupItemModel) {
                    return AbsLayoutItemType.GROUP_ITEM;
                }
                return AbsLayoutItemType.UNSUPPORTED;
            }
        });
    }

    private void onBindActivity(ActivityViewHolder holder, ActivityModel model) {
        holder.binding.itemNickName.setText(model.author_name);
        holder.binding.itemTime.setText(model.getTime());

        String desc = SystemSwitchUtils.obj_type(getContext(), model.obj_type, model.op_type);
        holder.binding.itemDesc.setText(desc);

        if (model.obj_type.equals("repo")) {
            holder.binding.itemRepoName.setText("");
            holder.binding.itemDetail.setText(model.repo_name);
        } else {
            holder.binding.itemRepoName.setText(model.repo_name);

            if (model.op_type.equals("rename")) {
                SpanUtils.with(holder.binding.itemDetail)
                        .append(model.old_name)
                        .append(" => ")
                        .append(model.name)
                        .setForegroundColor(ContextCompat.getColor(getContext(), R.color.fancy_orange))
                        .create();

            } else if (model.op_type.equals("delete")) {
                holder.binding.itemDetail.setText(model.name);
                holder.binding.itemDetail.setTextColor(ContextCompat.getColor(getContext(), R.color.list_item_subtitle_color));
            } else {
                holder.binding.itemDetail.setText(model.name);
                holder.binding.itemDetail.setTextColor(ContextCompat.getColor(getContext(), R.color.fancy_orange));
            }
        }

        GlideApp.with(getContext())
                .load(GlideLoadConfig.getGlideUrl(model.avatar_url))
                .apply(GlideLoadConfig.getOptions())
                .into(holder.binding.itemAvatar);
    }
}
