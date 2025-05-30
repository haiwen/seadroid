/*
 * Copyright 2022 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.seafile.seadroid2.ui.media.image;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.recyclerview.widget.DiffUtil;

import com.blankj.utilcode.util.EncryptUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.request.RequestOptions;
import com.bumptech.glide.signature.ObjectKey;
import com.seafile.seadroid2.framework.glide.GlideApp;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.ItemThumbnailItemVerticalBinding;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.framework.db.entities.DirentModel;
import com.seafile.seadroid2.framework.util.ThumbnailUtils;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

import java.util.List;

public class ThumbnailAdapter extends BaseAdapter<DirentModel, ThumbnailAdapter.ThumbnailItemViewHolder> {
    private final int itemWidth;
    private final int offsetWidth;
    private final String serverUrl;

    public ThumbnailAdapter(Context context, String serverUrl) {
        this.serverUrl = serverUrl;
        this.itemWidth = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_width);
        this.offsetWidth = ScreenUtils.getScreenWidth() / 2 - itemWidth / 2;
    }

    @Override
    protected void onBindViewHolder(@NonNull ThumbnailItemViewHolder holder, int i, @Nullable DirentModel direntModel) {
        bind(holder, i, direntModel);
    }

    @NonNull
    @Override
    protected ThumbnailItemViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemThumbnailItemVerticalBinding binding = ItemThumbnailItemVerticalBinding.inflate(LayoutInflater.from(viewGroup.getContext()), viewGroup, false);
        return new ThumbnailItemViewHolder(binding);
    }

    private void bind(ThumbnailItemViewHolder holder, int pos, DirentModel model) {

        if (model.item_position == ItemPositionEnum.START) {
            holder.itemView.getLayoutParams().width = offsetWidth;
            holder.itemView.setEnabled(false);

            GlideApp.with(holder.binding.imageView)
                    .load(R.color.transparent)
                    .into(holder.binding.imageView);
            return;
        }

        if (model.item_position == ItemPositionEnum.END) {
            holder.itemView.setEnabled(false);
            holder.itemView.getLayoutParams().width = offsetWidth;
            GlideApp.with(holder.binding.imageView)
                    .load(R.color.transparent)
                    .into(holder.binding.imageView);
            return;
        }

        holder.itemView.setEnabled(true);
        holder.itemView.getLayoutParams().width = itemWidth;
        String thumbnailUrl = convertThumbnailUrl(model.repo_id, model.full_path);
        if (TextUtils.isEmpty(thumbnailUrl)) {
            holder.binding.imageView.setImageResource(R.drawable.shape_solid_grey100_radius_4);
            return;
        }

        String thumbKey = EncryptUtils.encryptMD5ToString(thumbnailUrl);
        GlideApp.with(holder.binding.imageView)
                .load(thumbnailUrl)
                .signature(new ObjectKey(thumbKey))
                .apply(opt)
                .into(holder.binding.imageView);
    }

    private final RequestOptions opt = new RequestOptions()
            .placeholder(R.drawable.shape_solid_grey100_radius_4)
            .fallback(R.drawable.shape_solid_grey100_radius_4)
            .error(R.drawable.icon_format_pic)
            .diskCacheStrategy(DiskCacheStrategy.AUTOMATIC);

    private String convertThumbnailUrl(String repoId, String fullPath) {

        if (TextUtils.isEmpty(serverUrl)) {
            return null;
        }

        return ThumbnailUtils.convertThumbnailUrl(serverUrl, repoId, fullPath);
    }


    public static class ThumbnailItemViewHolder extends BaseViewHolder {
        private final ItemThumbnailItemVerticalBinding binding;

        ThumbnailItemViewHolder(ItemThumbnailItemVerticalBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }

    public void notify(List<DirentModel> list) {

        final List<DirentModel> oldList = getItems();
        final List<DirentModel> newList = list;

        final DiffUtil.DiffResult diffResult = DiffUtil.calculateDiff(new DiffUtil.Callback() {
            @Override
            public int getOldListSize() {
                return getItems().size();
            }

            @Override
            public int getNewListSize() {
                return newList.size();
            }

            @Override
            public boolean areItemsTheSame(int oldItemPosition, int newItemPosition) {
                DirentModel oldT = getItems().get(newItemPosition);
                DirentModel newT = newList.get(newItemPosition);
                return TextUtils.equals(newT.full_path, oldT.full_path);
            }

            @Override
            public boolean areContentsTheSame(int oldItemPosition, int newItemPosition) {
                DirentModel oldT = getItems().get(newItemPosition);
                DirentModel newT = newList.get(newItemPosition);
                return newT.equals(oldT);
            }
        });

        setItems(newList);
        diffResult.dispatchUpdatesTo(this);
    }

}
