package com.seafile.seadroid2.ui.bottomsheetmenu;

import android.content.Context;
import android.content.res.ColorStateList;
import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.content.ContextCompat;

import com.blankj.utilcode.util.ScreenUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

public class BottomSheetMenuAdapter extends BaseAdapter<MenuItem, BottomSheetMenuAdapter.BottomSheetViewHolder> {
    private final int columnCount;
    private final int screenWidth = ScreenUtils.getAppScreenWidth();
    private boolean isHorizontal = false;

    public BottomSheetMenuAdapter(int columnCount, boolean isHorizontal) {
        this.columnCount = columnCount;
        this.isHorizontal = isHorizontal;
    }

    public BottomSheetMenuAdapter(int columnCount) {
        this.columnCount = columnCount;
        this.isHorizontal = false;
    }


    @Override
    protected void onBindViewHolder(@NonNull BottomSheetMenuAdapter.BottomSheetViewHolder holder, int i, @Nullable MenuItem menuItem) {
        if (menuItem == null) {
            return;
        }

        holder.icon.setImageDrawable(menuItem.getIcon());
        holder.name.setText(menuItem.getTitle());

        holder.name.setEnabled(menuItem.isEnabled());
        holder.itemView.setClickable(menuItem.isEnabled());

        int color;
        if (menuItem.isEnabled()) {
            color = ContextCompat.getColor(getContext(), R.color.bottom_sheet_pop_enable_color);
        } else {
            color = ContextCompat.getColor(getContext(), R.color.bottom_sheet_pop_disable_color);
        }
        holder.icon.setImageTintList(ColorStateList.valueOf(color));
        holder.name.setTextColor(color);

    }

    @NonNull
    @Override
    protected BottomSheetMenuAdapter.BottomSheetViewHolder onCreateViewHolder(@NonNull Context context, @NonNull ViewGroup viewGroup, int i) {
        int layoutId = columnCount == 1 ? R.layout.bottom_sheet_item_list : R.layout.bottom_sheet_item_grid;
        View view = LayoutInflater.from(getContext()).inflate(layoutId, viewGroup, false);
        if (isHorizontal && columnCount > 1) {
            view.getLayoutParams().width = screenWidth / columnCount;
        }
        return new BottomSheetMenuAdapter.BottomSheetViewHolder(view);
    }

    public static class BottomSheetViewHolder extends BaseViewHolder {
        public ImageView icon;
        public TextView name;

        public BottomSheetViewHolder(@NonNull View itemView) {
            super(itemView);
            icon = itemView.findViewById(R.id.icon);
            name = itemView.findViewById(R.id.text);
        }
    }

}
