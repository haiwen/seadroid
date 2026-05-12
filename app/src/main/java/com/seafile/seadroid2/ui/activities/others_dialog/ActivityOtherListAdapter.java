package com.seafile.seadroid2.ui.activities.others_dialog;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.databinding.ItemActivityOtherItemBinding;
import com.seafile.seadroid2.framework.model.activities.ActivityDetailModel;
import com.seafile.seadroid2.framework.model.activities.ActivityModel;
import com.seafile.seadroid2.framework.util.Utils;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

import org.apache.commons.lang3.StringUtils;

public class ActivityOtherListAdapter extends BaseAdapter<ActivityDetailModel, ActivityOtherListAdapter.ActivityOtherListHolder> {
    private ActivityModel activityModel;

    public void setActivityModel(ActivityModel activityModel) {
        this.activityModel = activityModel;
    }

    @NonNull
    @Override
    protected ActivityOtherListHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemActivityOtherItemBinding binding = ItemActivityOtherItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new ActivityOtherListHolder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull ActivityOtherListHolder holder, int i, @Nullable ActivityDetailModel detailModel) {
        if (detailModel == null) {
            return;
        }

        holder.binding.itemTitle.setText(Utils.getFileNameFromPath(detailModel.path));
        holder.binding.itemTime.setText(detailModel.getTime());

        if (activityModel == null) {
            throw new IllegalArgumentException("ActivityModel param must not be null");
        }

        if (StringUtils.equals(activityModel.op_type, "delete") || StringUtils.equals(activityModel.op_type, "batch_delete")) {
            holder.binding.itemTitle.setTextColor(ContextCompat.getColor(getContext(), R.color.item_subtitle_color));
        } else {
            holder.binding.itemTitle.setTextColor(ContextCompat.getColor(getContext(), R.color.fancy_orange));
        }
    }


    public static class ActivityOtherListHolder extends BaseViewHolder {
        ItemActivityOtherItemBinding binding;

        public ActivityOtherListHolder(@NonNull ItemActivityOtherItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
