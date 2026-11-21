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

    /**
     * if maxSelectCount is 1, means single select.
     * if maxSelectCount is 0, means no limit.
     * if maxSelectCount is greater than 1, means multi select.
     * <p>
     * default 0, no limit.;
     */
    private int maxSelectCount = 0;
    private OnFileItemChangeListener onFileItemChangeListener;

    public void setMaxSelectCount(int maxSelectCount) {
        this.maxSelectCount = maxSelectCount;
    }

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
            holder.tvFileName.setTextColor(ContextCompat.getColor(getContext(), R.color.item_title_color));

        } else {
            holder.tvFileDetail.setText(String.format(getContext().getString(R.string.folder_file_item_size), fileBean.getSize()));
            holder.tvFileName.setTextColor(ContextCompat.getColor(getContext(), R.color.item_subtitle_color));
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
        FileBean currentItem = getItems().get(position);
        boolean isChecking = checkState == MaterialCheckBox.STATE_CHECKED;

        // Handle deselection directly
        if (!isChecking) {
            currentItem.setCheckedState(checkState);
            if (onFileItemChangeListener != null) {
                onFileItemChangeListener.onChanged(currentItem, position, false);
            }
            return;
        }

        // Handle selection logic
        if (maxSelectCount == 1) {
            // Single-selection mode: deselect previously selected items
            for (int i = 0; i < getItems().size(); i++) {
                FileBean item = getItems().get(i);
                if (i != position && item.getCheckedState() == MaterialCheckBox.STATE_CHECKED) {
                    item.setCheckedState(MaterialCheckBox.STATE_UNCHECKED);
                    notifyItemChanged(i);
                    if (onFileItemChangeListener != null) {
                        onFileItemChangeListener.onChanged(item, i, false);
                    }
                }
            }
            // Select the current item
            currentItem.setCheckedState(checkState);
            if (onFileItemChangeListener != null) {
                onFileItemChangeListener.onChanged(currentItem, position, true);
            }
        } else if (maxSelectCount > 1) {
            // Multi-selection mode: check whether the selection limit is exceeded
            int checkedCount = 0;
            for (FileBean item : getItems()) {
                if (item.getCheckedState() == MaterialCheckBox.STATE_CHECKED) {
                    checkedCount++;
                }
            }

            // Disallow selecting an unselected item once the selection limit is reached
            if (checkedCount >= maxSelectCount && currentItem.getCheckedState() != MaterialCheckBox.STATE_CHECKED) {
                // Do not apply the selection; restore the unchecked state
                currentItem.setCheckedState(MaterialCheckBox.STATE_UNCHECKED);
                notifyItemChanged(position);
                return;
            }

            // The limit has not been reached, so the item can be selected
            currentItem.setCheckedState(checkState);
            if (onFileItemChangeListener != null) {
                onFileItemChangeListener.onChanged(currentItem, position, true);
            }
        } else {
            // maxSelectCount = 0, no selection limit
            currentItem.setCheckedState(checkState);
            if (onFileItemChangeListener != null) {
                onFileItemChangeListener.onChanged(currentItem, position, true);
            }
        }
    }

}
