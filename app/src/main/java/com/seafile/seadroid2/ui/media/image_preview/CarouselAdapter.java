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

package com.seafile.seadroid2.ui.media.image_preview;

import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.LayoutRes;
import androidx.annotation.NonNull;
import androidx.recyclerview.widget.DiffUtil;
import androidx.recyclerview.widget.ListAdapter;

import com.seafile.seadroid2.data.db.entities.DirentModel;

/**
 * An adapter that displays {@link CarouselItem}s for a Carousel.
 */
class CarouselAdapter extends ListAdapter<DirentModel, CarouselItemViewHolder> {

    private static final DiffUtil.ItemCallback<DirentModel> DIFF_CALLBACK =
            new DiffUtil.ItemCallback<DirentModel>() {
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

    CarouselAdapter(CarouselItemListener listener, @LayoutRes int itemLayoutRes) {
        super(DIFF_CALLBACK);
        this.listener = listener;
        this.itemLayoutRes = itemLayoutRes;
    }

    @NonNull
    @Override
    public CarouselItemViewHolder onCreateViewHolder(@NonNull ViewGroup viewGroup, int pos) {
        return new CarouselItemViewHolder(
                LayoutInflater.from(viewGroup.getContext())
                        .inflate(itemLayoutRes, viewGroup, false), listener);
    }

    @Override
    public void onBindViewHolder(@NonNull CarouselItemViewHolder carouselItemViewHolder, int pos) {
        carouselItemViewHolder.bind(getItem(pos));
    }

}
