package com.seafile.seadroid2.ui.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.ImageView;
import android.widget.TextView;
import com.google.common.collect.Lists;
import com.seafile.seadroid2.R;
import com.seafile.seadroid2.data.SeafItem;
import com.seafile.seadroid2.data.SearchedFile;
import com.seafile.seadroid2.ui.activity.SearchActivity;

import java.util.List;

/**
 * Adapter for search list
 *
 */
public class SearchAdapter extends BaseAdapter {

    private List<SearchedFile> items;
    private SearchActivity mActivity;

    public SearchAdapter(SearchActivity activity) {
        this.mActivity = activity;
        items = Lists.newArrayList();
    }

    public void setItems(List<SearchedFile> data) {
        this.items = data;
    }

    @Override
    public int getCount() {
        return items.size();
    }

    @Override
    public Object getItem(int position) {
        return items.get(position);
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    @Override
    public View getView(int position, View convertView, ViewGroup parent) {
        SeafItem item = items.get(position);
        View view = convertView;
        Viewholder viewHolder;

        if (convertView == null) {
            view = LayoutInflater.from(mActivity).inflate(R.layout.search_list_item, null);
            TextView path = (TextView) view.findViewById(R.id.search_item_path);
            TextView title = (TextView) view.findViewById(R.id.search_item_title);
            TextView subtitle = (TextView) view.findViewById(R.id.search_item_subtitle);
            ImageView icon = (ImageView) view.findViewById(R.id.search_item_icon);
            ImageView action = (ImageView) view.findViewById(R.id.search_item_action);
            viewHolder = new Viewholder(path, title, subtitle, icon, action);
            view.setTag(viewHolder);
        } else {
            viewHolder = (Viewholder) convertView.getTag();
        }

        viewHolder.icon.setImageResource(item.getIcon());
        viewHolder.path.setText(((SearchedFile)item).getPath());
        viewHolder.title.setText(item.getTitle());
        viewHolder.subtitle.setText(item.getSubtitle());

        //setFileView((SeafDirent)item, viewHolder, position);

        return view;
    }

    public void notifyChanged() {
        notifyDataSetChanged();
    }

    private class Viewholder {
        TextView path, title, subtitle;
        ImageView icon, action;

        public Viewholder(TextView path, TextView title, TextView subtitle, ImageView icon, ImageView action) {
            super();
            this.icon = icon;
            this.action = action;
            this.path = path;
            this.title = title;
            this.subtitle = subtitle;
        }
    }
}
