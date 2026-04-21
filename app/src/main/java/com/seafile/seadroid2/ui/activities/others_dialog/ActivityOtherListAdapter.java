package com.seafile.seadroid2.ui.activities.others_dialog;

import static com.seafile.seadroid2.config.Constants.DP.DP_8;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.databinding.ItemActivityOtherItemBinding;
import com.seafile.seadroid2.databinding.ItemSdocOutlineBinding;
import com.seafile.seadroid2.framework.model.activities.ActivityDetailModel;
import com.seafile.seadroid2.framework.model.sdoc.OutlineItemModel;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class ActivityOtherListAdapter extends BaseAdapter<ActivityDetailModel, ActivityOtherListAdapter.ActivityOtherListHolder> {

    private final int _paddingStart = DP_8;

    @NonNull
    @Override
    protected ActivityOtherListHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemActivityOtherItemBinding binding = ItemActivityOtherItemBinding.inflate(LayoutInflater.from(context), viewGroup, false);
        return new ActivityOtherListHolder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull ActivityOtherListHolder holder, int i, @Nullable OutlineItemModel outlineItemModel) {
        if (outlineItemModel == null) {
            return;
        }

        int padding = 0;
        if ("header1".equals(outlineItemModel.type)) {
            padding = _paddingStart;
        } else if ("header2".equals(outlineItemModel.type)) {
            padding = _paddingStart * 3;
        } else if ("header3".equals(outlineItemModel.type)) {
            padding = _paddingStart * 6;
        }

        holder.binding.title.setPadding(padding, 0, 0, 0);
        holder.binding.title.setText(outlineItemModel.text);
    }


    public static class ActivityOtherListHolder extends BaseViewHolder {
        ItemActivityOtherItemBinding binding;

        public ActivityOtherListHolder(@NonNull ItemActivityOtherItemBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
