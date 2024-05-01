package com.seafile.seadroid2.ui.folder_backup;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.Nullable;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.ui.base.adapter.BaseAdapter;
import com.seafile.seadroid2.ui.base.viewholder.BaseViewHolder;

import org.jetbrains.annotations.NotNull;

public class FolderBackupSelectedPathAdapter extends BaseAdapter<String, FolderBackupSelectedPathAdapter.SearchItemViewHolder> {
    @Override
    protected void onBindViewHolder(@NotNull SearchItemViewHolder viewHolder, int i, @Nullable String s) {
        viewHolder.title.setText(s);
        viewHolder.title.setSelected(true);
    }

    @NotNull
    @Override
    protected SearchItemViewHolder onCreateViewHolder(@NotNull Context context, @NotNull ViewGroup viewGroup, int i) {
        View view = LayoutInflater.from(context).inflate(R.layout.item_text_more, viewGroup, false);
        return new SearchItemViewHolder(view);
    }

    public static class SearchItemViewHolder extends BaseViewHolder {
        public TextView title;
        public View icon;

        public SearchItemViewHolder(View view) {
            super(view);

            title = view.findViewById(R.id.title);
            icon = view.findViewById(R.id.more);
        }
    }


}
