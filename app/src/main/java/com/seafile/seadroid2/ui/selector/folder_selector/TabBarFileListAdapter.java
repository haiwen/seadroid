package com.seafile.seadroid2.ui.selector.folder_selector;

import android.app.Activity;
import android.content.Context;

import androidx.recyclerview.widget.RecyclerView;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.listener.OnItemClickListener;

import java.util.List;

public class TabBarFileListAdapter extends RecyclerView.Adapter<TabbarFileViewHolder> {

    private List<TabBarFileBean> mListData;
    private Context mContext;
    private com.seafile.seadroid2.listener.OnItemClickListener<TabBarFileBean> onItemClickListener;

    public void setOnItemClickListener(OnItemClickListener<TabBarFileBean> onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    public TabBarFileListAdapter(Activity context, List<TabBarFileBean> listData) {
        mListData = listData;
        mContext = context;
    }

    @Override
    public TabbarFileViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.item_tabbar_files_list, parent, false);
        return new TabbarFileViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final TabbarFileViewHolder holder, int positon) {
        final TabBarFileBean entity = mListData.get(positon);
        String fileName = entity.getFileName().replaceAll("[^\\p{Print}]", "");
        holder.tvName.setText(fileName);
        holder.llRoot.setOnClickListener(v -> {
            if (onItemClickListener != null) {
                onItemClickListener.onItemClick(entity, positon);
            }
        });
    }

    @Override
    public int getItemCount() {
        if (mListData == null) {
            return 0;
        } else {
            return mListData.size();
        }
    }

    public void updateListData(List<TabBarFileBean> mListData) {
        this.mListData = mListData;
    }
}
