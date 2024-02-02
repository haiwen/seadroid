package com.seafile.seadroid2.ui.selector.folder_selector;


import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.CompoundButton;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

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

        holder.checkBoxFile.setChecked(fileBean.isChecked());
        holder.tvFileName.setText(fileBean.getFileName());
        holder.imgvFiletype.setImageResource(fileBean.getFileImgType());
        boolean isFile = fileBean.isFile();

        if (isFile) {
            holder.tvFileDetail.setText(String.format(getContext().getString(R.string.folder_file_item_size), fileBean.getSize()));
        } else {
            String c = String.format(getContext().getString(R.string.folder_file_item_describe), fileBean.getChildrenFileNumber(), fileBean.getChildrenDirNumber());
            holder.tvFileDetail.setText(c);
        }

        if (!isFile) {
            holder.checkBoxFile.setVisibility(View.VISIBLE);
        } else {
            holder.checkBoxFile.setVisibility(View.GONE);
        }

        final int p = i;
        holder.checkBoxFile.setOnCheckedChangeListener(new CompoundButton.OnCheckedChangeListener() {
            @Override
            public void onCheckedChanged(CompoundButton buttonView, boolean isChecked) {
                onCheckBoxChanged(p, isChecked);
            }
        });
    }

    private void onCheckBoxChanged(int position, boolean isChecked) {
        getItems().get(position).setChecked(isChecked);
        if (onFileItemChangeListener != null) {
            onFileItemChangeListener.onChanged(getItems().get(position), position, isChecked);
        }
    }
}