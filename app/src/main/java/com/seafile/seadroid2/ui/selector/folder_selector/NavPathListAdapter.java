package com.seafile.seadroid2.ui.selector.folder_selector;

import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.blankj.utilcode.util.CollectionUtils;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.enums.ItemPositionEnum;
import com.seafile.seadroid2.listener.OnItemClickListener;

import java.util.List;

public class NavPathListAdapter extends RecyclerView.Adapter<NavPathViewHolder> {

    private List<TabVolumeBean> mListData;
    private final Context mContext;
    private com.seafile.seadroid2.listener.OnItemClickListener<TabVolumeBean> onItemClickListener;

    public List<TabVolumeBean> getListData() {
        return mListData;
    }

    public void setOnItemClickListener(OnItemClickListener<TabVolumeBean> onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    public NavPathListAdapter(Context context) {
        mContext = context;
    }

    @NonNull
    @Override
    public NavPathViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.item_tabbar_files_list, parent, false);
        return new NavPathViewHolder(view);
    }

    @Override
    public void onBindViewHolder(final NavPathViewHolder holder, int position) {
        final TabVolumeBean entity = mListData.get(position);
        String fileName = entity.getFileName().replaceAll("[^\\p{Print}]", "");
        holder.name.setText(fileName);
        if (entity.getItemPosition() == ItemPositionEnum.END || entity.getItemPosition() == ItemPositionEnum.ALL) {
            holder.name.setAlpha(1f);
            holder.icon.setAlpha(1f);
        } else {
            holder.name.setAlpha(0.3f);
            holder.icon.setAlpha(0.3f);
        }

        holder.llRoot.setOnClickListener(v -> {
            if (onItemClickListener != null) {
                onItemClickListener.onItemClick(entity, position);
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

    public void updateListData(List<TabVolumeBean> mListData) {
        if (CollectionUtils.isEmpty(mListData)) {
            return;
        }

        this.mListData = mListData;
        notifyDataSetChanged();
    }
}
