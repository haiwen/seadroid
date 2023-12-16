package com.seafile.seadroid2.ui.search;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.annotation.Nullable;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SearchedFile;
import com.seafile.seadroid2.ui.base.adapter.BaseViewHolder;
import com.seafile.seadroid2.ui.base.adapter.ParentAdapter;
import com.seafile.seadroid2.util.Utils;

import org.jetbrains.annotations.NotNull;

public class SearchRecyclerViewAdapter extends ParentAdapter<SearchedFile, SearchRecyclerViewAdapter.SearchItemViewHolder> {
    private Context context;

    public SearchRecyclerViewAdapter(Context context) {
        this.context = context;
    }

    @Override
    protected void onBindViewHolder(@NotNull SearchItemViewHolder viewHolder, int i, @Nullable SearchedFile searchedFile) {
        viewHolder.icon.setImageResource(searchedFile.getIcon());
        viewHolder.path.setText(filePath(searchedFile));
        viewHolder.title.setText(searchedFile.getTitle());
        viewHolder.subtitle.setText(searchedFile.getSubtitle());
    }

    @NotNull
    @Override
    protected SearchItemViewHolder onCreateViewHolder(@NotNull Context context, @NotNull ViewGroup viewGroup, int i) {
        View view = LayoutInflater.from(context).inflate(R.layout.search_list_item, viewGroup, false);
        return new SearchItemViewHolder(view);
    }

    private String filePath(SearchedFile searchedFile) {
        String parentPath = Utils.getParentPath(searchedFile.getPath());
        SeafRepo seafRepo = ((Search2Activity) context).getDataManager().getCachedRepoByID(searchedFile.getRepoID());
        if (seafRepo != null)
            return Utils.pathJoin(seafRepo.getRepoName(), parentPath);
        else
            return parentPath;
    }

    public static class SearchItemViewHolder extends BaseViewHolder {
        public TextView path;
        public TextView title;
        public TextView subtitle;
        public ImageView icon;

        public SearchItemViewHolder(View view) {
            super(view);

            path = (TextView) view.findViewById(R.id.search_item_path);
            title = (TextView) view.findViewById(R.id.search_item_title);
            subtitle = (TextView) view.findViewById(R.id.search_item_subtitle);
            icon = (ImageView) view.findViewById(R.id.search_item_icon);
        }

    }
}
