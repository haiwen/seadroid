package com.seafile.seadroid2.ui.activities;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;
import androidx.recyclerview.widget.RecyclerView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.GlideLoadConfig;
import com.seafile.seadroid2.databinding.ItemActivityBinding;
import com.seafile.seadroid2.databinding.ItemGroupItemBinding;
import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.GroupItemModel;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.ui.viewholder.GroupItemViewHolder;

import java.util.List;

public class ActivityAdapter extends BaseMultiAdapter<BaseModel> {

    int defaultColor;
    int fancyColor;

    @Override
    public void onAttachedToRecyclerView(@NonNull RecyclerView recyclerView) {
        super.onAttachedToRecyclerView(recyclerView);

        defaultColor = ContextCompat.getColor(getContext(), R.color.item_subtitle_color);
        fancyColor = ContextCompat.getColor(getContext(), R.color.fancy_orange);
    }

    public ActivityAdapter() {

        addItemType(AbsLayoutItemType.GROUP_ITEM, new OnMultiItem<BaseModel, GroupItemViewHolder>() {
            @NonNull
            @Override
            public GroupItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemGroupItemBinding binding = ItemGroupItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new GroupItemViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull GroupItemViewHolder holder, int i, @Nullable BaseModel baseModel) {
                GroupItemModel model = (GroupItemModel) baseModel;
                holder.binding.itemGroupTitle.setText(model.title);
                holder.binding.itemGroupExpand.setVisibility(View.GONE);
                holder.binding.getRoot().setClickable(false);
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
                return AbsLayoutItemType.NOT_SUPPORTED;
            }
        });
    }

    private void onBindActivity(ActivityViewHolder holder, ActivityModel model) {
        holder.binding.itemNickName.setText(model.author_name);
        holder.binding.itemTime.setText(model.getTime());
        holder.binding.itemDesc.setText(model.getDesc(getContext()));

        // load avatar
        if (TextUtils.isEmpty(model.avatar_url)) {
            holder.binding.itemAvatar.setImageResource(R.drawable.default_avatar);
        } else {
            GlideApp.with(holder.binding.getRoot())
                    .load(model.avatar_url)
                    .apply(GlideLoadConfig.getAvatarOptions())
                    .into(holder.binding.itemAvatar);
        }

        holder.binding.itemRepoName.setText(model.getRepoNameText());
        holder.binding.itemDetail.setText(model.getDetailText(getContext(), defaultColor, fancyColor));
    }
}
