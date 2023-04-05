package com.seafile.seadroid2.folderbackup;

import android.app.Activity;
import android.content.Context;
import android.content.DialogInterface;
import android.support.annotation.NonNull;
import android.support.v7.widget.RecyclerView;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.TextView;

import com.blankj.utilcode.util.CollectionUtils;
import com.cocosw.bottomsheet.BottomSheet;
import com.google.gson.Gson;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.SettingsManager;
import com.seafile.seadroid2.data.SeafRepo;
import com.seafile.seadroid2.data.SearchedFile;
import com.seafile.seadroid2.folderbackup.selectfolder.StringTools;
import com.seafile.seadroid2.listener.OnItemClickListener;
import com.seafile.seadroid2.ui.activity.search.Search2Activity;
import com.seafile.seadroid2.util.Utils;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

public class FolderBackSelectedPatRecyclerViewAdapter extends RecyclerView.Adapter<FolderBackSelectedPatRecyclerViewAdapter.SearchItemViewHolder> {
    private final List<String> mItemList = new ArrayList<>();
    private OnItemClickListener<String> onItemClickListener;

    private final WeakReference<Context> contextWeakReference;

    public FolderBackSelectedPatRecyclerViewAdapter(Context context) {
        this.contextWeakReference = new WeakReference<>(context);
    }

    public void setOnItemClickListener(OnItemClickListener<String> onItemClickListener) {
        this.onItemClickListener = onItemClickListener;
    }

    @NonNull
    @Override
    public SearchItemViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(contextWeakReference.get()).inflate(R.layout.item_text_more, parent, false);
        return new SearchItemViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull FolderBackSelectedPatRecyclerViewAdapter.SearchItemViewHolder viewHolder, int position) {
        final int p = position;
        viewHolder.title.setText(mItemList.get(p));
        viewHolder.title.setSelected(true);

        viewHolder.icon.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                showRepoBottomSheet(p);
            }
        });

        viewHolder.itemView.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                if (onItemClickListener != null) {
                    onItemClickListener.onItemClick(mItemList.get(p), p);
                }
            }
        });
    }

    public List<String> getItemList() {
        return mItemList;
    }

    @Override
    public int getItemCount() {
        return mItemList.size();
    }

    public void notifyDataChanged(List<String> list) {
        mItemList.clear();
        if (!CollectionUtils.isEmpty(list)) {
            mItemList.addAll(list);
            notifyDataSetChanged();
        }
    }

    public void notifyDataClear() {
        if (!CollectionUtils.isEmpty(mItemList)) {
            mItemList.clear();
            notifyDataSetChanged();
        }
    }

    public static class SearchItemViewHolder extends RecyclerView.ViewHolder {
        public TextView title;
        public View icon;

        public SearchItemViewHolder(View view) {
            super(view);

            title = (TextView) view.findViewById(R.id.title);
            icon = (View) view.findViewById(R.id.more);
        }
    }

    private void showRepoBottomSheet(int position) {
        final BottomSheet.Builder builder = new BottomSheet.Builder((Activity) contextWeakReference.get());
        builder.sheet(R.menu.bottom_sheet_delete).listener(new DialogInterface.OnClickListener() {
            @Override
            public void onClick(DialogInterface dialog, int which) {
                if (which == R.id.delete) {
                    mItemList.remove(position);
                    notifyDataSetChanged();

                    String strJsonPath = new Gson().toJson(mItemList);
                    SettingsManager.instance().saveBackupPaths(strJsonPath);
                }
            }
        }).show();
    }
}
