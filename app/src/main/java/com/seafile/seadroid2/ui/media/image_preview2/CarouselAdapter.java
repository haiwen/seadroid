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

package com.seafile.seadroid2.ui.media.image_preview2;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.LayoutRes;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.ListAdapter;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.EncodeUtils;
import com.blankj.utilcode.util.ScreenUtils;
import com.blankj.utilcode.util.SizeUtils;
import com.bumptech.glide.load.engine.DiskCacheStrategy;
import com.bumptech.glide.request.RequestOptions;
import com.google.common.base.Strings;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.account.Account;
import com.seafile.seadroid2.account.SupportAccountManager;
import com.seafile.seadroid2.framework.data.db.entities.DirentModel;
import com.seafile.seadroid2.framework.util.GlideApp;

public class CarouselAdapter extends ListAdapter<DirentModel, CarouselAdapter.CarouselItemViewHolder> {

    private static final DiffUtil.ItemCallback<DirentModel> DIFF_CALLBACK =
            new DiffUtil.ItemCallback<>() {
                @Override
                public boolean areItemsTheSame(
                        @NonNull DirentModel oldItem, @NonNull DirentModel newItem) {
                    // User properties may have changed if reloaded from the DB, but ID is fixed
                    return oldItem == newItem;
                }

                @Override
                public boolean areContentsTheSame(
                        @NonNull DirentModel oldItem, @NonNull DirentModel newItem) {
                    return false;
                }
            };

    private final CarouselItemListener listener;
    @LayoutRes
    private final int itemLayoutRes;
    private int sidePadding;
    private int itemWidth;

    public interface CarouselItemListener {
        void onItemClicked(DirentModel item, int position);
    }

    public CarouselAdapter(Context context, CarouselItemListener listener) {
        super(DIFF_CALLBACK);
        this.listener = listener;
        this.itemLayoutRes = R.layout.item_carousel_item_vertical;

        itemWidth = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_width);
        int itemMargin = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_margin);

        int screenWidth = ScreenUtils.getAppScreenWidth();
        sidePadding = (screenWidth - itemWidth) / 2 - itemMargin * 2;
    }

    public CarouselAdapter(Context context, CarouselItemListener listener, @LayoutRes int itemLayoutRes) {
        super(DIFF_CALLBACK);
        this.listener = listener;
        this.itemLayoutRes = itemLayoutRes;

        itemWidth = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_width);
        int itemMargin = context.getResources().getDimensionPixelSize(R.dimen.carousel_item_margin);


        int screenWidth = ScreenUtils.getAppScreenWidth();
        sidePadding = (screenWidth - itemWidth) / 2 - itemMargin * 2;
    }

    @Override
    public DirentModel getItem(int position) {
        return super.getItem(position);
    }

    @NonNull
    @Override
    public CarouselItemViewHolder onCreateViewHolder(@NonNull ViewGroup viewGroup, int pos) {
        return new CarouselItemViewHolder(LayoutInflater.from(viewGroup.getContext()).inflate(itemLayoutRes, viewGroup, false));
    }

    @Override
    public void onBindViewHolder(@NonNull CarouselItemViewHolder carouselItemViewHolder, int pos) {
        bind(carouselItemViewHolder, pos);
    }


    private void bind(CarouselItemViewHolder holder, int pos) {
        DirentModel model = getItem(pos);
//        holder.textView.setText(pos + "");

        holder.itemView.setOnClickListener(v -> {
            listener.onItemClicked(model, pos);
        });

        if (TextUtils.isEmpty(model.name)) {
            holder.itemView.getLayoutParams().width = sidePadding;
            holder.imageView.setVisibility(View.INVISIBLE);
            return;
        }

        holder.imageView.setVisibility(View.VISIBLE);
        holder.itemView.getLayoutParams().width = itemWidth;

        String url = getUrl(model.repo_id, model.full_path);
        if (url == null) {
            holder.imageView.setImageResource(R.drawable.icon_image_error_filled);
            return;
        }

        RequestOptions opt = new RequestOptions()
                .skipMemoryCache(true)
                .error(R.drawable.icon_image_error_filled)
                .diskCacheStrategy(DiskCacheStrategy.AUTOMATIC);
        GlideApp.with(holder.imageView)
                .load(url)
                .apply(opt)
                .fitCenter()
                .into(holder.imageView);
    }

    private String getUrl(String repoId, String fullPath) {
        Account account = SupportAccountManager.getInstance().getCurrentAccount();
        if (account == null) {
            return null;
        }

//        //https://dev.seafile.com/seafhttp/repos/4809a6f3-250c-4435-bdd8-b68f34c128d1/files//6f64603fd19f9ec45d05ec379e69e22.gif/?op=download
//        //https://dev.seafile.com/seahub/repo/4809a6f3-250c-4435-bdd8-b68f34c128d1/raw/6f64603fd19f9ec45d05ec379e69e22.gif
//        if (direntModel.name.toLowerCase().endsWith(".gif")) {
//            return String.format(Locale.ROOT, "%srepo/%s/raw/%s", account.getServer(), direntModel.repo_id, direntModel.name);
//        }

//        return String.format("%srepo/%s/raw%s", account.getServer(), repoId, fileFullPath);
        int size = SizeUtils.dp2px(300);
        return String.format("%sapi2/repos/%s/thumbnail/?p=%s&size=%s", account.getServer(), repoId, EncodeUtils.urlEncode(fullPath), size);
    }


    public static class CarouselItemViewHolder extends RecyclerView.ViewHolder {

        public final ImageView imageView;
        public final TextView textView;

        CarouselItemViewHolder(@NonNull View itemView) {
            super(itemView);
            imageView = itemView.findViewById(R.id.image_view);
            textView = itemView.findViewById(R.id.text_view);
        }

    }
}
