package com.seafile.seadroid2.backupdirectory;

import android.app.Activity;
import android.content.Context;
import android.support.v7.widget.RecyclerView;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.util.Utils;

import java.util.List;

public class TabbarFileListAdapter extends RecyclerView.Adapter<TabbarFileViewHolder> {
    private List<TabbarFileBean> mListData;
    private Context mContext;
    private OnFileItemClickListener onItemClickListener;

    public void setOnItemClickListener(OnFileItemClickListener onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    public TabbarFileListAdapter(Activity context, List<TabbarFileBean> listData) {
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
        final TabbarFileBean entity = mListData.get(positon);
        String fileName=entity.getFileName().replaceAll("[^\\p{Print}]","");
        holder.tvName.setText(fileName);
        holder.llRoot.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (onItemClickListener != null) {
                    onItemClickListener.click(holder.getAdapterPosition());
                }
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

    public void updateListData(List<TabbarFileBean> mListData) {
        this.mListData = mListData;
    }
}
