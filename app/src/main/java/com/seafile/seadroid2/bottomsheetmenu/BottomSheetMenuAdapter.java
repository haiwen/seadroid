package com.seafile.seadroid2.bottomsheetmenu;

import android.view.LayoutInflater;
import android.view.MenuItem;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.seafile.seadroid2.R;

import java.util.ArrayList;
import java.util.List;

public class BottomSheetMenuAdapter extends RecyclerView.Adapter<BottomSheetMenuAdapter.BottomSheetViewHolder> {
    private List<MenuItem> dataList = new ArrayList<>();
    private int columnCount = 1;
    private OnMenuClickListener onMenuClickListener;

    public BottomSheetMenuAdapter(List<MenuItem> dataList, int columnCount) {
        this.dataList = dataList;
        this.columnCount = columnCount;
    }

    public void setOnMenuClickListener(OnMenuClickListener onMenuClickListener) {
        this.onMenuClickListener = onMenuClickListener;
    }

    @NonNull
    @Override
    public BottomSheetViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        int layoutId = columnCount == 1 ? R.layout.bottom_sheet_item_list : R.layout.bottom_sheet_item_grid;
        View view = LayoutInflater.from(parent.getContext()).inflate(layoutId, parent, false);
        return new BottomSheetViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull BottomSheetViewHolder holder, int position) {
        final int p = position;
        holder.icon.setImageDrawable(dataList.get(p).getIcon());
        holder.name.setText(dataList.get(p).getTitle());
        if (onMenuClickListener != null) {
            holder.itemView.setOnClickListener(new View.OnClickListener() {
                @Override
                public void onClick(View v) {
                    onMenuClickListener.onMenuClick(dataList.get(p));
                }
            });
        }
    }

    @Override
    public int getItemCount() {
        return dataList.size();
    }

    public static class BottomSheetViewHolder extends RecyclerView.ViewHolder {
        public ImageView icon;
        public TextView name;

        public BottomSheetViewHolder(@NonNull View itemView) {
            super(itemView);
            icon = itemView.findViewById(R.id.icon);
            name = itemView.findViewById(R.id.name);
        }
    }

}
