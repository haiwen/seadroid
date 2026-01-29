package com.seafile.seadroid2.ui.wiki;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.config.AbsLayoutItemType;
import com.seafile.seadroid2.config.WikiType;
import com.seafile.seadroid2.databinding.ItemWikiBinding;
import com.seafile.seadroid2.databinding.ItemWikiGroupItemBinding;
import com.seafile.seadroid2.framework.model.BaseModel;
import com.seafile.seadroid2.framework.model.wiki.WikiGroupModel;
import com.seafile.seadroid2.framework.model.wiki.WikiInfoModel;
import com.seafile.seadroid2.framework.util.Times;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.adapter.BaseMultiAdapter;
import com.seafile.seadroid2.ui.viewholder.WikiGroupItemViewHolder;

import java.util.List;

public class WikiAdapter extends BaseMultiAdapter<BaseModel> {
    public WikiAdapter() {
        addItemType(AbsLayoutItemType.WIKI, new OnMultiItem<BaseModel, WikiHolder>() {
            @NonNull
            @Override
            public WikiHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemWikiBinding binding = ItemWikiBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new WikiHolder(binding);
            }

            @Override
            public void onBind(@NonNull WikiHolder holder, int i, @Nullable BaseModel baseModel) {
                onBindWiki(holder, baseModel, i);
            }
        }).addItemType(AbsLayoutItemType.GROUP_ITEM, new OnMultiItem<BaseModel, WikiGroupItemViewHolder>() {
            @NonNull
            @Override
            public WikiGroupItemViewHolder onCreate(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
                ItemWikiGroupItemBinding binding = ItemWikiGroupItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
                return new WikiGroupItemViewHolder(binding);
            }

            @Override
            public void onBind(@NonNull WikiGroupItemViewHolder holder, int i, @Nullable BaseModel item) {
                onBindGroup(holder, item, i);
            }

        }).onItemViewType(new OnItemViewTypeListener<BaseModel>() {
            @Override
            public int onItemViewType(int i, @NonNull List<? extends BaseModel> list) {
                if (list.get(i) instanceof WikiGroupModel) {
                    return AbsLayoutItemType.GROUP_ITEM;
                } else {
                    return AbsLayoutItemType.WIKI;
                }
            }
        });
    }

    private void onBindWiki(WikiHolder holder, BaseModel model, int position) {
        WikiInfoModel m = (WikiInfoModel) model;


        // old user data
        if (TextUtils.equals(m.type, WikiType.TYPE_OLD)) {
            holder.binding.itemUserContainer.setVisibility(View.VISIBLE);
            holder.binding.itemUserName.setText(m.owner_nickname);
        } else {
            holder.binding.itemUserName.setText(null);
            holder.binding.itemUserContainer.setVisibility(View.INVISIBLE);
        }

        // publish state
        if (m.is_published) {
            holder.binding.itemPublishState.setVisibility(View.VISIBLE);
        } else {
            holder.binding.itemPublishState.setVisibility(View.GONE);
        }


        // more state
        if (TextUtils.equals(m.type, WikiType.TYPE_GROUP)) {
            holder.binding.itemWikiMore.setVisibility(View.GONE);
        } else {
            holder.binding.itemWikiMore.setVisibility(View.VISIBLE);
        }


        // time
        long l = Times.convertMtime2Long(m.updated_at);
        String t = Utils.translateCommitTime(l);
        holder.binding.itemTime.setText(t);

        // item name
        holder.binding.itemName.setText(m.name);
    }

    protected void onBindGroup(WikiGroupItemViewHolder holder, BaseModel model, int position) {
        WikiGroupModel m = (WikiGroupModel) model;
        holder.binding.itemWikiGroupTitle.setText(m.getTitle());
        holder.binding.itemWikiGroupIcon.setImageResource(m.getIcon());
    }
}
