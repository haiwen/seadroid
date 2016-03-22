package com.seafile.seadroid2.ui.adapter;

import android.content.Context;
import android.graphics.Color;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.TextView;

import com.joanzapata.iconify.fonts.MaterialCommunityIcons;
import com.joanzapata.iconify.widget.IconTextView;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.EventDetailsFileItem;
import com.seafile.seadroid2.data.EventDetailsTree;

import java.util.List;

public class BottomSheetAdapter extends BaseAdapter {
    private List<EventDetailsFileItem> items;
    private Context context;

    public BottomSheetAdapter(Context context, List<EventDetailsFileItem> items) {
        this.items = items;
        this.context = context;
    }

    @Override
    public int getCount() {
        return items.size();
    }

    @Override
    public Object getItem(int i) {
        return items.get(i);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View contentView, ViewGroup viewGroup) {
        ViewHolder holder;
        if (contentView == null) {
            holder = new ViewHolder();
            contentView = View.inflate(context, R.layout.list_item_diff, null);
            holder.file = (TextView) contentView.findViewById(R.id.tv_diff_file_name);
            holder.icon = (IconTextView) contentView.findViewById(R.id.tv_diff_icon);
            contentView.setTag(holder);
        } else {
            holder = (ViewHolder) contentView.getTag();
        }

        final EventDetailsFileItem eventDetailsFileItem = items.get(position);

        holder.file.setText(eventDetailsFileItem.getPath());
        switch (eventDetailsFileItem.geteType()) {
            case FILE_ADDED:
            case DIR_ADDED:
                holder.file.setTextColor(Color.parseColor("#6CC644"));
                holder.icon.setText("{" + MaterialCommunityIcons.mdi_plus.key() + " #6CC644}");
                break;
            case FILE_MODIFIED:
                holder.file.setTextColor(Color.parseColor("#D0B44C"));
                holder.icon.setText("{" + MaterialCommunityIcons.mdi_pencil.key() + " #D0B44C}");
                break;
            case FILE_RENAMED:
                holder.file.setTextColor(Color.parseColor("#677A85"));
                holder.icon.setText("{" + MaterialCommunityIcons.mdi_arrow_right.key() + " #677A85}");
                break;
            case FILE_DELETED:
            case DIR_DELETED:
                holder.file.setTextColor(Color.parseColor("#BD2C00"));
                holder.icon.setText("{" + MaterialCommunityIcons.mdi_minus.key() + " #BD2C00}");
                break;
        }

        return contentView;
    }

    static class ViewHolder{
        public TextView file;
        public IconTextView icon;
    }
}
