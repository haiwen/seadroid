package com.seafile.seadroid2.ui.repo.sort;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.seafile.seadroid2.R;

import java.util.List;

public class SortMenuAdapter extends RecyclerView.Adapter<SortMenuAdapter.VH> {
    public interface OnItemClickListener {
        void onClick(int menuId);
    }

    private final List<SortMenuItem> list;

    private final OnItemClickListener listener;

    public SortMenuAdapter(List<SortMenuItem> list, OnItemClickListener listener) {
        this.list = list;
        this.listener = listener;
    }

    @NonNull
    @Override
    public SortMenuAdapter.VH onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {

        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.item_sort_menu, parent, false);

        return new VH(view);
    }

    @Override
    public void onBindViewHolder(@NonNull VH holder, int position) {

        SortMenuItem item = list.get(position);
        holder.tvTitle.setText(item.title);
        holder.tvCheck.setVisibility(item.checked ? View.VISIBLE : View.INVISIBLE);

        holder.itemView.setOnClickListener(v -> {
            listener.onClick(position);
        });
    }

    @Override
    public int getItemCount() {
        return list.size();
    }

    public static class VH extends RecyclerView.ViewHolder {

        TextView tvTitle;

        ImageView tvCheck;

        public VH(@NonNull View itemView) {
            super(itemView);

            tvCheck = itemView.findViewById(R.id.menu_view_gallery_icon);
            tvTitle = itemView.findViewById(R.id.menu_view_gallery_title);
        }
    }
}
