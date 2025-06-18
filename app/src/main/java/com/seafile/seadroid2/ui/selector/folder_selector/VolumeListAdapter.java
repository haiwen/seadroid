package com.seafile.seadroid2.ui.selector.folder_selector;

import android.app.Activity;
import android.content.Context;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;

import androidx.recyclerview.widget.RecyclerView;

import com.seafile.seadroid2.R;
import com.seafile.seadroid2.listener.OnItemClickListener;

import java.util.List;

public class VolumeListAdapter extends RecyclerView.Adapter<VolumeViewHolder> {

    private List<VolumeBean> mVolumeList;
    private Context mContext;
    private OnItemClickListener<VolumeBean> onItemClickListener;

    public VolumeListAdapter(Activity context, List<VolumeBean> volumeList) {
        this.mVolumeList = volumeList;
        this.mContext = context;
    }

    public void setOnItemClickListener(OnItemClickListener<VolumeBean> listener) {
        this.onItemClickListener = listener;
    }

    @Override
    public VolumeViewHolder onCreateViewHolder(ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(mContext).inflate(R.layout.item_volume_list, parent, false);
        return new VolumeViewHolder(view);
    }

    @Override
    public void onBindViewHolder(VolumeViewHolder holder, int position) {
        final VolumeBean volume = mVolumeList.get(position);
        holder.tvDescription.setText(volume.getDescription());

        holder.llRoot.setOnClickListener(v -> {
            if (onItemClickListener != null) {
                onItemClickListener.onItemClick(volume, position);
            }
        });
    }

    @Override
    public int getItemCount() {
        return mVolumeList != null ? mVolumeList.size() : 0;
    }

    public void updateVolumeList(List<VolumeBean> newList) {
        this.mVolumeList = newList;
        notifyDataSetChanged();
    }
}
