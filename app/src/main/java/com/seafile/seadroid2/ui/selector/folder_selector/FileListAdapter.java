package com.seafile.seadroid2.ui.selector.folder_selector;


import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;

import com.google.android.material.checkbox.MaterialCheckBox;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.listener.OnFileItemChangeListener;

public class FileListAdapter extends BaseAdapter<FileBean, FileListViewHolder> {

    private OnFileItemChangeListener onFileItemChangeListener;

    public void setOnFileItemChangeListener(OnFileItemChangeListener onFileItemChangeListener) {
        this.onFileItemChangeListener = onFileItemChangeListener;
    }

    @NonNull
    @Override
    protected FileListViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        View view = LayoutInflater.from(context).inflate(R.layout.item_files_list, viewGroup, false);
        return new FileListViewHolder(view);
    }

    @Override
    protected void onBindViewHolder(@NonNull FileListViewHolder holder, int i, @Nullable FileBean fileBean) {

        holder.tvFileName.setText(fileBean.getFileName());
        holder.imgvFiletype.setImageResource(fileBean.getFileImgType());

        if (fileBean.isDir()) {
            String c = String.format(getContext().getString(R.string.folder_file_item_describe), fileBean.getChildrenFileNumber(), fileBean.getChildrenDirNumber());
            holder.tvFileDetail.setText(c);
            holder.tvFileName.setTextColor(ContextCompat.getColor(getContext(), R.color.list_item_title_color));

        } else {
            holder.tvFileDetail.setText(String.format(getContext().getString(R.string.folder_file_item_size), fileBean.getSize()));
            holder.tvFileName.setTextColor(ContextCompat.getColor(getContext(), R.color.list_item_subtitle_color));
        }

        holder.checkBoxFile.clearOnCheckedStateChangedListeners();

        holder.checkBoxFile.setCheckedState(fileBean.getCheckedState());
        if (fileBean.isDir()) {
            holder.checkBoxFile.setVisibility(View.VISIBLE);
        } else {
            holder.checkBoxFile.setVisibility(View.GONE);
        }

        final int p = i;
        holder.checkBoxFile.addOnCheckedStateChangedListener(new MaterialCheckBox.OnCheckedStateChangedListener() {
            @Override
            public void onCheckedStateChangedListener(@NonNull MaterialCheckBox checkBox, int state) {
                onCheckBoxChanged(p, state);
            }
        });
    }

    private void onCheckBoxChanged(int position, int checkState) {
        getItems().get(position).setCheckedState(checkState);
        if (onFileItemChangeListener != null) {
            onFileItemChangeListener.onChanged(getItems().get(position), position, checkState == MaterialCheckBox.STATE_CHECKED);
        }
    }
}