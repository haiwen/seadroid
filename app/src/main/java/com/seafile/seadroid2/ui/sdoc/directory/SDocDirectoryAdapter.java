package com.seafile.seadroid2.ui.sdoc.directory;

import static com.seafile.seadroid2.config.Constants.DP.DP_8;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.seafile.seadroid2.databinding.ItemSdocDirectoryBinding;
import com.seafile.seadroid2.framework.data.model.sdoc.SDocModel;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class SDocDirectoryAdapter extends BaseAdapter<SDocModel, SDocDirectoryAdapter.SDocDirectoryHolder> {

    private final int _paddingStart = DP_8;

    @NonNull
    @Override
    protected SDocDirectoryHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        ItemSdocDirectoryBinding binding = ItemSdocDirectoryBinding.inflate(LayoutInflater.from(context));
        return new SDocDirectoryHolder(binding);
    }

    @Override
    protected void onBindViewHolder(@NonNull SDocDirectoryHolder holder, int i, @Nullable SDocModel sDocModel) {
        if (sDocModel == null) {
            return;
        }

        int padding;
        if ("header1".equals(sDocModel.type)) {
            padding = _paddingStart;
        } else if ("header2".equals(sDocModel.type)) {
            padding = _paddingStart * 3;
        } else if ("header3".equals(sDocModel.type)) {
            padding = _paddingStart * 6;
        } else {
            return;
        }


        holder.binding.title.setPadding(padding, 0, 0, 0);
        holder.binding.title.setText(sDocModel.text);
    }


    public static class SDocDirectoryHolder extends BaseViewHolder {
        ItemSdocDirectoryBinding binding;

        public SDocDirectoryHolder(@NonNull ItemSdocDirectoryBinding binding) {
            super(binding.getRoot());
            this.binding = binding;
        }
    }
}
