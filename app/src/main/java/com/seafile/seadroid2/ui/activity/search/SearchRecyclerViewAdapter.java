package com.seafile.seadroid2.ui.activity.search;

import android.content.Context;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SearchedFile;
import com.seafile.seadroid2.listener.OnItemClickListener;
import com.seafile.seadroid2.util.Utils;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

public class SearchRecyclerViewAdapter extends RecyclerView.Adapter<SearchRecyclerViewAdapter.SearchItemViewHolder> {
    private final List<SearchedFile> mItemList = new ArrayList<>();
    private OnItemClickListener<SearchedFile> onItemClickListener;

    private final WeakReference<Context> contextWeakReference;

    public SearchRecyclerViewAdapter(Context context) {
        this.contextWeakReference = new WeakReference<>(context);
    }

    public void setOnItemClickListener(OnItemClickListener<SearchedFile> onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    @NonNull
    @Override
    public SearchItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(contextWeakReference.get()).inflate(R.layout.search_list_item, parent, false);
        return new SearchItemViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull SearchRecyclerViewAdapter.SearchItemViewHolder viewHolder, int position) {
        viewHolder.icon.setImageResource(mItemList.get(position).getIcon());
        viewHolder.path.setText(filePath(mItemList.get(position)));
        viewHolder.title.setText(mItemList.get(position).getTitle());
        viewHolder.subtitle.setText(mItemList.get(position).getSubtitle());

        viewHolder.itemView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (onItemClickListener != null) {
                    onItemClickListener.onItemClick(mItemList.get(position), position);
                }
            }
        });
    }

    private String filePath(SearchedFile searchedFile) {
        String parentPath = Utils.getParentPath(searchedFile.getPath());
        SeafRepo seafRepo = ((Search2Activity) contextWeakReference.get()).getDataManager().getCachedRepoByID(searchedFile.getRepoID());
        if (seafRepo != null)
            return Utils.pathJoin(seafRepo.getName(), parentPath);
        else
            return parentPath;
    }

    public List<SearchedFile> getItemList() {
        return mItemList;
    }

    @Override
    public int getItemCount() {
        return mItemList.size();
    }

    public void notifyDataClear() {
        if (!CollectionUtils.isEmpty(mItemList)) {
            mItemList.clear();
            notifyDataSetChanged();
        }
    }

    public void notifyDataChanged(List<SearchedFile> list) {
        if (!CollectionUtils.isEmpty(list)) {
            mItemList.addAll(list);
            notifyDataSetChanged();
        }
    }

    public static class SearchItemViewHolder extends RecyclerView.ViewHolder {
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
