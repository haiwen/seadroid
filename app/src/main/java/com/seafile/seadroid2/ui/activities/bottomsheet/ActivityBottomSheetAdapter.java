package com.seafile.seadroid2.ui.activities.bottomsheet;

import android.content.Context;
import android.graphics.Color;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.joanzapata.iconify.fonts.MaterialCommunityIcons;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;
import com.seafile.seadroid2.data.EventDetailsFileItem;
import com.seafile.seadroid2.databinding.ListItemDiffBinding;

public class ActivityBottomSheetAdapter extends BaseAdapter<EventDetailsFileItem, ActivityBottomSheetAdapter.ActivityBottomSheetViewHolder> {

    @Override
    protected void onBindViewHolder(@NonNull ActivityBottomSheetViewHolder holder, int i, @Nullable EventDetailsFileItem eventDetailsFileItem) {
        holder.binding.tvDiffFileName.setText(eventDetailsFileItem.getPath());
        switch (eventDetailsFileItem.geteType()) {
            case FILE_ADDED:
            case DIR_ADDED:
                holder.binding.tvDiffIcon.setTextColor(Color.parseColor("#6CC644"));
                holder.binding.tvDiffIcon.setText("{" + MaterialCommunityIcons.mdi_plus.key() + " #6CC644}");
                break;
            case FILE_MODIFIED:
                holder.binding.tvDiffIcon.setTextColor(Color.parseColor("#D0B44C"));
                holder.binding.tvDiffIcon.setText("{" + MaterialCommunityIcons.mdi_pencil.key() + " #D0B44C}");
                break;
            case FILE_RENAMED:
                holder.binding.tvDiffIcon.setTextColor(Color.parseColor("#677A85"));
                holder.binding.tvDiffIcon.setText("{" + MaterialCommunityIcons.mdi_arrow_right.key() + " #677A85}");
                break;
            case FILE_DELETED:
            case DIR_DELETED:
                holder.binding.tvDiffIcon.setTextColor(Color.parseColor("#BD2C00"));
                holder.binding.tvDiffIcon.setText("{" + MaterialCommunityIcons.mdi_minus.key() + " #BD2C00}");
                break;
        }
    }

    @NonNull
    @Override
    protected ActivityBottomSheetViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ListItemDiffBinding binding = ListItemDiffBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new ActivityBottomSheetViewHolder(binding);
    }

    static class ActivityBottomSheetViewHolder extends BaseViewHolder {
        public ListItemDiffBinding binding;

        public ActivityBottomSheetViewHolder(@NonNull ListItemDiffBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
